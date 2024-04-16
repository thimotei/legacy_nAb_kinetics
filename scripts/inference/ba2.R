# Munging data into format for Stan
stan_data_ba2_full <- retrieve_stan_data(
  dt_ba2_full, time_type = "relative", covariate_formula)
stan_data_ba2_trunc <- retrieve_stan_data(
  dt_ba2_trunc, time_type = "relative", covariate_formula)

# Running inference - only runs if posteriors not yet generated -----------
if(!file.exists("outputs/fits/ba2_trunc.rds")){

  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))

  # Fitting model to real-time data
  fit_ba2_trunc <- mod$sample(
    data = stan_data_ba2_trunc,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Fitting model to full data
  fit_ba2_full <- mod$sample(
    data = stan_data_ba2_full,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Saving fits
  fit_ba2_trunc$save_object("outputs/fits/ba2_trunc.rds")
  fit_ba2_full$save_object("outputs/fits/ba2_full.rds")
}
