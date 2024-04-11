source("scripts/setup/setup.R")

# Specifying formula for the subsequent inference
formula_exp <- ~ 0 + exp_num

dt_exp <- dt_clean[
  # exp_num %in% 1:5 &
  # titre_type %in% c("BQ.1.1", "XBB") &
  exp_type_sum == "Vaccination" &
  !is.na(titre)]

# Combining pre-omicron and omicron infection histories for this
dt_exp[infection_history %in% c(
  "Previously infected (Omicron)", 
  "Previously infected (Pre-Omicron)"), 
  infection_history := "Previously infected"]

# Ensuring both covariate categories are factors
dt_exp[, vax_num := factor(vax_num)]
dt_exp[, inf_num := factor(inf_num)]
dt_exp[, exp_num := factor(exp_num)]
dt_exp[, exp_type_sum := factor(exp_type_sum)]
dt_exp[, infection_history := factor(infection_history)]

# Preparing data for inference
dt_exp_stan <- prepare_stan_data(dt_exp)

dt_exp_stan |>
  ggplot(aes(x = t_since_last_exp, y = titre, colour = titre_type)) +
  geom_point(size = 0.2) +
  geom_line(aes(x = t_since_last_exp, y = titre, group = interaction(titre_type, id), colour = titre_type), alpha = 0.2) +
  # geom_vline(aes(xintercept = date_delta), colour = "red") +
  # geom_vline(aes(xintercept = last_exp_date), alpha = 0.5, linetype = "twodash") +
  geom_smooth(method = "lm") +
  facet_grid(exp_num~titre_type, scales = "free") +
  # scale_colour_manual(values = manual_pal_figure_3) +
  theme_bw()

# Retrieving data in format for Stan
stan_data_exp <- retrieve_stan_data(
  dt_exp_stan, time_type = "relative", formula_exp)

#--- Running inference - commented out by default for now, as fits
#--- have been run and are saved locally
# mod <- cmdstan_model(
#   "stan/antibody_kinetics_main.stan",
#   include_paths = "stan",
#   stanc_options = list("O1"),
#   cpp_options = list(stan_threads = TRUE))
# 
# fit_exp <- mod$sample(
#   data = stan_data_exp,
#   chains = 4,
#   parallel_chains = 4,
#   iter_warmup = 1000,
#   iter_sampling = 1000,
#   threads_per_chain = 4)
# 
# fit_exp$save_object("outputs/fits/exp.rds")
