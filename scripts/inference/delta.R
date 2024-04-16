# Munging data into format for Stan
stan_data_delta_full <- retrieve_stan_data(
  dt_delta_full, time_type = "relative", covariate_formula)
stan_data_delta_trunc <- retrieve_stan_data(
  dt_delta_trunc, time_type = "relative", covariate_formula)

# Running inference - only runs if posteriors not yet generated -----------
if(!file.exists("outputs/fits/delta_trunc.rds")){

  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))

  # Fitting the model to the real-time dataset, i.e. truncated at chosen date
  fit_delta_trunc <- mod$sample(
    data = stan_data_delta_trunc,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Fitting the model to the full dataset
  fit_delta_full <- mod$sample(
    data = stan_data_delta_full,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Saving fits
  fit_delta_trunc$save_object("outputs/fits/delta_trunc.rds")
  fit_delta_full$save_object("outputs/fits/delta_full.rds")
}
