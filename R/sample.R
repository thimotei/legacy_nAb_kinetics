# sample_population_priors <- function(
#     mu_t0 = 5, sigma_t0 = 2.5, 
#     mu_tp = 20, sigma_tp = 3,
#     mu_ts = 60, sigma_ts = 5,
#     mu_m1 = 0.02, sigma_m1 = 0.05,
#     mu_m2 = -0.02, sigma_m2 = 0.05,
#     mu_m3 = 0, sigma_m3 = 0.05,
#     n_samples = 1
# ) {
#   
#   population_priors <- data.table(
#     t0_pop = rnorm(n_samples, mu_t0, sigma_t0),
#     # t0_pop = runif(n_samples, 0, 10),
#     tp_pop = rtruncnorm(n_samples, a = 0, mean = mu_tp, sd = sigma_tp),
#     ts_pop = rtruncnorm(n_samples, a = 0, mean = mu_ts, sd = sigma_ts),
#     
#     m1_pop = rtruncnorm(n_samples, a = 0, mean = mu_m1, sd = sigma_m1),
#     m2_pop = rtruncnorm(n_samples, b = 0, mean = mu_m2, sd = sigma_m2),
#     m3_pop = rtruncnorm(n_samples, b = 0, mean = mu_m3, sd = sigma_m3)
#   )[, id := .I]
#   
#   return(population_priors)
# }

sample_pop_priors <- function(
    dt_in = set_prior_values(),
    n_samples = 10,
    lower_bounds = NULL, 
    upper_bounds = NULL) {
  
  # Extracting number of parameters from input data.table
  n_params <- length(unique(dt_in[, name]))
  
  # Check if lower_bounds or upper_bounds were passed to the function
  if (missing(lower_bounds) || is.null(lower_bounds)) {
    lower_bounds <- rep(-Inf, n_params)
  }
  
  if (missing(upper_bounds) || is.null(upper_bounds)) {
    upper_bounds <- rep(Inf, n_params)
  }
  
  # Assign bounds to each parameter
  dt_in[, lower := lower_bounds[match(name, unique(name))]]
  dt_in[, upper := upper_bounds[match(name, unique(name))]]
  
  # # Reshape to wide format including mu, sigma, lower, and upper
  dt_wide <- dcast(dt_in, name + lower + upper ~ type, value.var = "value")
  
  # Adding ID for merge 
  dt_wide[, id := .GRP, by = name]
  
  # Creating temporary data.table to help with replicating samples n_samples 
  # times
  dt_rep <- data.table(id = rep(1:n_params, times = n_samples))
  
  # Merge data.tables to replicate parameter values n_samples times
  dt_proc <- merge(dt_wide, dt_rep, all.y = TRUE, by = "id")
  
  # Add sampled values as a new column
  dt_proc[
    , sample := rtruncnorm(
      1, a = lower, b = upper, mean = mu, sd = sigma),
    by = name]
  
  # Add a new sample ID to help with dcast() (is there a way to do this without
  # the new ID?)
  dt_proc[, sample_id := 1:.N, by = name]
  
  # Apply dcast to change format of data to wide
  dt_out <- dcast(dt_proc, sample_id ~ name, value.var = "sample")
  
  return(dt_out)
}

#--- FIX INDIVIDUAL-LEVEL PARAMETER SAMPLING
# dt_pop <- sample_pop_priors()
# 
# sample_ind_parameters(sample_pop_priors(), 10, sigma_params = c(1, 1, 1, 1, 1, 1))
# 
# sample_ind_parameters <- function(dt_pop, n_events, sigma_params) {
#   
#   dt_pop <- sample_pop_priors()
#   
#   dt_sigmas <- data.table(rep(sigma_params, nrow(dt_pop)))
#   
#   dt_pop_long <- melt(dt_pop, id.vars = "sample_id", variable.name = "parameter")
#   
#   dt_pop_long[, beta := rnorm(1), by = .I]
#   dt_pop_long[, sigma := rnorm(1, 0, sigma_params)]
#   dt_pop_long[, z := rnorm(1), by = .I]
#   
#   dt_pop[, sample_ind := value + beta + sigma_param*z[[parameter]], by = parameter]
#   
# }
# 
# 
# sample_ind_priors <- function(
#     K, N_events, n_samples = 1) {
#   
#   dt_pop_priors <- sample_pop_priors(n_samples = 100)
#   
#   dt_pop_priors[, `:=` (
#     t0 = t0 + rnorm(1, 0, 1) + rnorm(1, 0, sigma_t0) * rnorm(1, 0, 1),
#     tp = tp + rnorm(1, 0, 1) + rnorm(1, 0, sigma_tp) * rnorm(1, 0, 1),
#     ts = ts + rnorm(1, 0, 1) + rnorm(1, 0, sigma_ts) * rnorm(1, 0, 1),
#     m1 = m1 + rnorm(1, 0, 1) + rnorm(1, 0, sigma_m1) * rnorm(1, 0, 1),
#     m2 = m2 + rnorm(1, 0, 1) + rnorm(1, 0, sigma_m2) * rnorm(1, 0, 1),
#     m3 = m3 + rnorm(1, 0, 1) + rnorm(1, 0, sigma_m3) * rnorm(1, 0, 1))]
#   
#   
#   ts_pop_samples <- population_priors[, .(ts_pop = tp_pop + rnorm(.N, 0, 5))]  # Assumed `ts_pop_delta` has normal(0, 5) distribution
#   
#   combined_samples <- data.table(
#     sigma_t0_ind = rnorm(n_samples, 0, 3),
#     sigma_tp_ind = rnorm(n_samples, 0, 5),
#     sigma_ts_ind = rnorm(n_samples, 0, 5),
#     sigma_m1_ind = rcauchy(n_samples, 0, 0.25),
#     sigma_m2_ind = rcauchy(n_samples, 0, 0.25),
#     sigma_m3_ind = rcauchy(n_samples, 0, 0.25),
#     z_t0 = rnorm(n_samples * N_events),
#     z_tp = rnorm(n_samples * N_events),
#     z_ts = rnorm(n_samples * N_events),
#     z_m1 = rnorm(n_samples * N_events),
#     z_m2 = rnorm(n_samples * N_events),
#     z_m3 = rnorm(n_samples * N_events)
#   )
#   
#   individual_priors <- population_priors[, {
#     t0_ind_sample = t0_pop + sigma_t0_ind * z_t0
#     tp_ind_sample = tp_pop + sigma_tp_ind * z_tp
#     ts_ind_sample = ts_pop_samples$ts_pop + sigma_ts_ind * z_ts
#     m1_ind_sample = m1_pop + sigma_m1_ind * z_m1
#     m2_ind_sample = m2_pop + sigma_m2_ind * z_m2
#     m3_ind_sample = m3_pop + sigma_m3_ind * z_m3
#     
#     .(t0_ind = t0_ind_sample, 
#       tp_ind = tp_ind_sample, 
#       ts_ind = ts_ind_sample, 
#       m1_ind = m1_ind_sample, 
#       m2_ind = m2_ind_sample, 
#       m3_ind = m3_ind_sample)
#   }, by=1:nrow(population_priors), env=combined_samples]
#   
#   return(individual_priors)
# }
# 
# 
# sample_ind_priors <- function(N_events = 10, n_samples = 1, sigmas) {
#   # Assume sample_population_priors is defined and returns a data.table
#   dt_pop_priors <- sample_population_priors(n_samples = 10)
#   
#   # Define parameter names to apply variations
#   param_names <- names(dt_pop_priors)
#   
#   dt_pop_priors[, beta := rnorm(1, 0, 1), by = .I]
#   dt_pop_priors[, z := rnorm(1, 0, 1), by = .I]
#   
#   sigmas
#   
#   melt(dt_pop_priors, id.vars = "sample_id")
#   
#   dt_pop_priors[,  + beta + rnorm(1, 0, sigma * z)]
#   
#   # Apply transformations using data.table's .SD and .SDcols
#   dt_pop_priors[, (param_names) := lapply(.SD, function(x, idx) {
#     x + betas[[idx]] + rnorm(1, 0, sigmas[[param_names[idx]]]) * zs[[idx]]
#   }, idx = seq_along(.SD)), .SDcols = param_names]
#   
#   return(dt_pop_priors)
# }
