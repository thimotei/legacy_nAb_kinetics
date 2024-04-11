source("scripts/setup.R")

dt_all_exp <- dt_trim[last_exp_type != "others"][
  !last_exp_type %in% c("Alpha",
                        "D614G", "BQ.1.1", "XBB.1.5",
                        "mRNA1273", "mRNA1273.214", 
                        "BNT162b2+BA1")][
                          !is.na(last_exp_type)][
  (!titre_type %in% c("Beta", "D614G")) & 
  (!last_exp_type %in% c("Alpha", "Delta", "AZD1222"))][
  !(titre_type == "D614G" & 
  last_exp_type == "BA.1")][
  (last_exp_type == "BNT162b2" & exp_num %in% 2:5) |
  (last_exp_type == "BA.1" & exp_num %in% 4:5) |
  (last_exp_type == "BA.2" & exp_num %in% 4:5) |
  (last_exp_type == "BA.5" & exp_num %in% 4:5)]
# 
# dt_n_exp <- dt_trim[
#   (exp_num == 1) & (titre_type %in% c("Wildtype", "Alpha")) |
#     (exp_num == 2) & (titre_type %in% c("Wildtype", "Alpha", "Delta")) |
#     (exp_num == 3) & (titre_type %in% c("Alpha", "Delta", "BA.1")) |
#     (exp_num == 4) & (titre_type %in% c("Delta", "BA.1", "BA.2")) |
#     (exp_num == 5) & (titre_type %in% c("BA.1", "BA.2", "BA.5")) |
#     (exp_num == 6) & (titre_type %in% c("BA.2", "BA.5", "BQ.1.1"))]

dt_n_exp <- dt_trim[titre_type %in% c(
  "Wildtype", "Delta",
  "BA.1", "BA.2", "BA.5")]

dt_n_exp[, infection_history := ifelse(
  inf_num > 0 & last_inf_date < last_exp_date, 
  "previously infected", "infection naive"), by = id]

dt_n_exp <- dt_n_exp[
  !(exp_num %in% 1:2 & infection_history == "previously infected")][
    t_since_last_exp < 100]

# what are the options: 
# last_inf_date = last_exp_date implies current exposure is an infection
# last_inf_date != last_exp_date implies current exposure is not an infection
# inf_num > 0

dt_n_exp[, n_bleeds_by_group := .N,
         by = c("exp_num", "last_exp_type", "infection_history", "titre_type")]

dt_n_exp <- dt_n_exp[n_bleeds_by_group > 50]

dt_n_exp |> 
  ggplot(aes(x = t_since_last_exp, y = titre, colour = titre_type)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = id), alpha = 0.1) + 
  geom_smooth(method = "lm") +
  geom_hline(
    aes(yintercept = 40),
    linetype = "twodash") +
  geom_hline(
    aes(yintercept = 5120),
    linetype = "twodash") +
  # scale_x_date(
  #   date_breaks = waiver(),
  #   date_labels = "%b-%y",
  # ) + 
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120),
    labels = c(expression(""<=40),
               "80", "160", "320", "640", "1280", "2560",
               expression("">=5120)), 
    limits = c(40, 5120)) +
  facet_grid(exp_num + last_exp_type + infection_history ~ titre_type ) +
  labs(x = "Date", y = "Titre", colour = "Titre type") + 
  # scale_color_lancet() + 
  theme_minimal() + 
  theme(legend.position = "none")

#--- FIT MODEL
dt_n_exp[, exp_num := factor(exp_num)]
dt_n_exp[, last_exp_type := factor(last_exp_type)]
dt_n_exp[, titre_type := factor(titre_type)]
dt_n_exp[, infection_history := factor(infection_history)]

dt_n_exp[, exp_num := fct_drop(exp_num)]
dt_n_exp[, last_exp_type := fct_drop(last_exp_type)]
dt_n_exp[, titre_type := fct_drop(titre_type)]
dt_n_exp[, infection_history := fct_drop(infection_history)]

#--- RUNNING INFERENCE
formula <- ~ exp_num:infection_history:last_exp_type
dt_n_exp_stan <- convert_log_scale(copy(dt_n_exp))
dt_n_exp_stan <- prepare_stan_data(dt_n_exp_stan)
stan_data_exp <- retrieve_stan_data(dt_n_exp_stan, formula)

mod <- cmdstan_model(
  "stan/individual_exposure_by_covariates.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE)) 

res_exp <- mod$sample(
  data = stan_data_exp,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000, 
  iter_sampling = 1000,
  # init = initial_values_list,
  threads_per_chain = 4)

res_exp$save_object("outputs/fits/exposures.rds")
