dt_n_exp_tmp <- dt_n_exp[
  last_exp_type %in% c
  ("BNT162b2+BA1", 
    "mRNA1273.214", 
    "BA.1", "BA.2", "BA.5")]

dt_n_exp_tmp <- dt_n_exp[
  n_bleeds_by_group > 50][
  , .SD[sample(.N, min(50, .N))],
  by = .(exp_num, last_exp_type, infection_history, titre_type)]

dt_n_exp_tmp[, .N, by = .(exp_num, last_exp_type, infection_history, titre_type)]

dt_n_exp_tmp |> 
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

dt_n_exp_stan |> 
  ggplot(aes(x = t_since_last_exp, y = titre, colour = titre_type)) +
  geom_point(alpha = 0.1) +
  geom_line(aes(group = id), alpha = 0.1) + 
  geom_smooth(method = "lm") + 
  facet_grid(exp_num + last_exp_type + infection_history ~ titre_type )

#--- FIT MODEL
dt_n_exp_tmp[, exp_num := factor(exp_num)]
dt_n_exp_tmp[, last_exp_type := factor(last_exp_type)]
dt_n_exp_tmp[, titre_type := factor(titre_type)]
dt_n_exp_tmp[, infection_history := factor(infection_history)]

dt_n_exp_tmp[, exp_num := fct_drop(exp_num)]
dt_n_exp_tmp[, last_exp_type := fct_drop(last_exp_type)]
dt_n_exp_tmp[, titre_type := fct_drop(titre_type)]
dt_n_exp_tmp[, infection_history := fct_drop(infection_history)]

#--- RUNNING INFERENCE
formula <- ~ exp_num:infection_history:last_exp_type
dt_n_exp_stan <- convert_log_scale(copy(dt_n_exp_tmp))
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

# res_exp <- readRDS("outputs/fits/exposures.rds")
res_exp$save_object("outputs/fits/exposures.rds")

draws_exp <- res_exp$draws()

dt_pop_params <- spread_draws(
  draws_exp, 
  tp_pop[K], beta_tp[P],
  m1_pop[K], beta_m1[P],
  m2_pop[K], beta_m2[P],
  m3_pop[K], beta_m3[P]) |> 
  data.table()

# dt_tp_pop_adj <- copy(dt_tp_pop)

dt_pop_params_adj <- adjust_parameters(
  dt_pop_params,
  params_to_adjust = c("tp_pop", "m1_pop", "m2_pop", "m3_pop"))

dt_pop_params_adj_long <- melt(
  dt_pop_params_adj,
  measure.vars = c("tp_pop", "m1_pop", "m2_pop", "m3_pop"))

dt_pop_params_adj_long |> 
  ggplot() + 
  geom_density_ridges(
    aes(x = value, y= factor(K), fill = variable)) +
  facet_grid(P~variable, scales = "free")




