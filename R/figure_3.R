figure_3_data <- function(
    fit, dt_stan, stan_data, formula_val, wave_manual,
    cleaned_names = c("Infection history", "Titre type")) {
  
  # Extracting population-level parameters
  dt_peak_switch <- extract_parameters_pop(fit)[, Wave := wave_manual]

  # Calculating the peak and switch titre values stratified by covariates
  # and titre types
  dt_peak_switch[, `:=` (
    mu_p = simulate_trajectory(
      tp_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
    mu_s = simulate_trajectory(
      ts_pop, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop)), 
    by = c("p", "k", ".draw", "Wave")]
  
  # Convert back to natural units
  convert_log_scale_inverse(
    dt_peak_switch, vars_to_transform = c("mu_p", "mu_s"))
  
  # Recover which covariates were used in the inference
  dt_peak_switch_plot <- recover_covariate_names(
    dt_peak_switch, dt_stan, stan_data, formula_val)
  
  # Cleaning the recovered covariate names
  dt_out <- clean_covariate_names(
    dt_peak_switch_plot, formula_val, cleaned_names)
  
  return(dt_out)
}