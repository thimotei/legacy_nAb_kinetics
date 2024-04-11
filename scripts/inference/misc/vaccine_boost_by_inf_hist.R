library(forcats)
library(cmdstanr)

source("R/prepare_inference.R")

dt_vax <- readRDS("data/processed_data.rds")

#--- 2nd vaccine dose boost
dt_vax_2_dose <- dt_vax[vax_num == 2]

dt_vax_2_stan <- dt_vax_2_stan[
  titre_type %in% c("Alpha", "Delta")]

dt_vax_2_dose[, vax_num := factor(vax_num)]
dt_vax_2_dose[, last_vax_type := fct_drop(last_vax_type)]
dt_vax_2_dose[, exp_hist := fct_drop(exp_hist)]

#--- RUNNING INFERENCE
formula <- ~ 0 + last_vax_type:exp_hist
dt_stan_2_dose <- prepare_stan_data(dt_vax_2_dose)
stan_data_2_dose <- retrieve_stan_data(dt_stan_2_dose, formula)

mod <- cmdstan_model(
  "stan/boost_wane_formatted.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE)) 

res_second_dose <- mod$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000, 
  iter_sampling = 1000,
  # init = initial_values_list,
  threads_per_chain = 4)

# res$save_object("outputs/fits/second_vax_inf_hist.rds")
res_second_dose <- readRDS("outputs/fits/second_vax_inf_hist.rds")

#--- 3rd vax
dt_vax_3_dose <- dt_vax[vax_num == 3]

dt_vax_3_dose[, vax_num := factor(vax_num)]
dt_vax_3_dose[, last_vax_type := fct_drop(last_vax_type)]
dt_vax_3_dose[, exp_hist := fct_drop(exp_hist)]

#--- RUNNING INFERENCE
formula <- ~ 0 + last_vax_type:exp_hist
dt_stan_3_dose <- prepare_stan_data(dt_vax_3_dose)
stan_data_3_dose <- retrieve_stan_data(dt_stan_3_dose, formula)

#--- 4th vax
dt_vax_4_stan <- dt_vax[vax_num == 4]

# dt_vax_4_stan <- dt_vax_4_stan[
#   !titre_type %in% c("Alpha", "XBB", "XBB.1.5")]
# 
dt_vax_4_stan <- dt_vax_4_stan[
  !last_exp_type %in% c("BNT162b2", "mRNA1273")]

dt_vax_4_stan[, vax_num := factor(vax_num)]
dt_vax_4_stan[, last_vax_type := fct_drop(last_vax_type)]
dt_vax_4_stan[, exp_hist := fct_drop(exp_hist)]

#--- RUNNING INFERENCE
formula <- ~ 0 + last_vax_type:exp_hist
dt_stan_4_dose <- prepare_stan_data(dt_vax_4_stan)
stan_data_4_dose <- retrieve_stan_data(dt_stan_4_dose, formula)

dt_stan_4_dose |> 
  ggplot(aes(x = t_since_last_exp, 
             y = titre,
             colour = titre_type)) + 
  geom_point(alpha = 0.1) + 
  geom_line(alpha = 0.1) + 
  geom_smooth(method = "lm", se = FALSE) + 
  facet_nested(exp_hist~last_vax_type, scales = "free") +
  theme_minimal()

mod <- cmdstan_model(
  "stan/boost_wane_formatted.stan",
  include_paths = "stan",
  stanc_options = list("O1"),
  cpp_options = list(stan_threads = TRUE)) 

res_forth_dose <- mod$sample(
  data = stan_data_4_dose,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000, 
  iter_sampling = 1000,
  # init = initial_values_list,
  threads_per_chain = 4)

res_forth_dose$save_object("outputs/fits/forth_vax_inf_hist.rds")
