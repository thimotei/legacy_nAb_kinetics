# Munging data into format for Stan
stan_data_xbb_full <- retrieve_stan_data(
  dt_xbb_full, time_type = "relative", covariate_formula)
stan_data_xbb_trunc <- retrieve_stan_data(
  dt_xbb_trunc, time_type = "relative", covariate_formula)

# Running inference - only runs if posteriors not yet generated -----------
if(!file.exists("outputs/fits/xbb_trunc.rds")){

  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))

  # Fitting model to real-time data
  fit_xbb_trunc <- mod$sample(
    data = stan_data_xbb_trunc,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 4000,
    threads_per_chain = 4)

  # Fitting model to full data
  fit_xbb_full <- mod$sample(
    data = stan_data_xbb_full,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 4000,
    threads_per_chain = 4)

  # Saving fits
  fit_xbb_trunc$save_object("outputs/fits/xbb_trunc.rds")
  fit_xbb_full$save_object("outputs/fits/xbb_full.rds")
}
