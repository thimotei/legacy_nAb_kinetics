extract_parameters_pop <- function(
    fit, params = c(
      "t0_pop[k]", "tp_pop[k]", "ts_pop[k]", "m1_pop[k]", "m2_pop[k]",
      "m3_pop[k]", "beta_t0[p]", "beta_tp[p]", "beta_ts[p]", "beta_m1[p]",
      "beta_m2[p]", "beta_m3[p]"),
    adjust = TRUE, add_variation_params = FALSE, n_draws = 2500) {

  if(add_variation_params == TRUE) {
    pop_var_params <- c(
      "sigma_t0_ind[k]", "sigma_tp_ind[k]", "sigma_ts_ind[k]",
      "sigma_m1_ind[k]", "sigma_m2_ind[k]", "sigma_m3_ind[k]")

    params = c(params, pop_var_params)
  }

  params_proc <- rlang::parse_exprs(params)

  dt_proc <- spread_draws(fit, !!!params_proc) |>
    data.table()

  dt_proc[, `:=` (.chain = NULL, .iteration = NULL)]

  setcolorder(dt_proc, c("k", "p", ".draw"))

  if(adjust == TRUE) {
    dt_out <- adjust_parameters(dt_proc)
  } else {
    dt_out <- dt_proc
  }

  dt_proc <- dt_proc[.draw %in% n_draws]

  return(dt_out)
}

extract_parameters_ind <- function(
    fit, params = c(
      "t0_ind[n, k]", "tp_ind[n, k]", "ts_ind[n, k]",
      "m1_ind[n, k]", "m2_ind[n, k]", "m3_ind[n, k]"),
    format = "wide",
    add_variation_params = TRUE) {

  if(add_variation_params == TRUE) {

    ind_var_params <- c(
      "z_t0[n]", "z_tp[n]", "z_ts[n]", "z_m1[n]", "z_m2[n]", "z_m3[n]")

    params = c(params, ind_var_params)
  }

  params_proc <- rlang::parse_exprs(params)

  dt_out <- spread_draws(fit, !!!params_proc) |>
    data.table()

  dt_out[, `:=` (.chain = NULL, .iteration = NULL)]

  setcolorder(dt_out, c("n", "k", ".draw"))
  setnames(dt_out, c("n", "k", ".draw"), c("stan_id", "titre_type", "draw"))

  return(dt_out)
}

extract_ind_params_clean <- function(
    fit, dt_data, n_draws = NULL, wave_manual,
    scale = "log") {

  # Extracting parameters from fit
  dt_params_ind <- extract_parameters_ind(
    fit, add_variation_params = FALSE)

  if(is.null(n_draws)) {
    n_draws <- dt_params_ind[, max(stan_id)*max(titre_type)*max(draw*4)]
  }

  dt_params_ind_trim <- dt_params_ind[, .SD[draw %in% 1:n_draws], by = stan_id]

  if(scale == "natural") {
    # Converting back to the original scale
    dt_params_ind_trim <- convert_log_scale_inverse_cpp(
      dt_params_ind_trim, vars_to_transform = "t0_ind")
  }

  setnames(dt_params_ind_trim, "titre_type", "titre_type_num")

  dt_titre_types <- data.table(
    titre_type = dt_data[, unique(titre_type)],
    titre_type_num = dt_params_ind_trim[, unique(titre_type_num)])

  dt_params_ind_trim <- merge(
    dt_params_ind_trim,
    dt_titre_types,
    by = "titre_type_num")[, titre_type_num := NULL]

  dt_lookup <- dt_data[, .(
    exposure_date = min(last_exp_date)),
    by = .(id, stan_id, infection_history)]

  dt_out <- merge(dt_params_ind_trim, dt_lookup, by = "stan_id")

  return(dt_out)
}

extract_titres_set_date <- function(
    dt_in, set_date, N_days = 75, buffer_days = 10) {

  # Identify relevant exposures based on set_date and N_days
  relevant_exposures <- dt_in[
    last_exp_date <= set_date & last_exp_date >= set_date - N_days,
    .(relevant_last_exp_date = max(last_exp_date)),
    by = id]

  # Merge and filter by buffer
  dt_out <- merge(dt_in, relevant_exposures, by = "id")[
    date >= (relevant_last_exp_date - buffer_days)
  ]

  # Calculate next_exposure_date for each id
  next_exposure_dates <- dt_out[
    last_exp_date != relevant_last_exp_date & date > set_date,
    .(next_exposure_date = min(date)),
    by = id
  ]

  # Merge this back to the main data table
  dt_out <- merge(
    dt_out, next_exposure_dates, by = "id", all.x = TRUE)

  # Filter based on next_exposure_date
  dt_out <- dt_out[
    date <= ifelse(is.na(next_exposure_date),
                   max(date),
                   next_exposure_date - 1)]

  return(dt_out)
}

extract_wave_data <- function(
    dt_in,
    date_set,
    covariate_formula,
    t_max,
    titre_types,
    truncate_at_date = TRUE,
    time_threshold = 60,
    # threshold is on the log2(titre/5) scale
    titre_threshold = 1) {

  dt_out <- dt_in[!is.na(titre)]

  # Extract relevant observations
  dt_out <- extract_titres_set_date(
    dt_out, date_set, t_max, 0)

  # Filter out relevant exposure and titre types
  dt_out <- dt_out[
    titre_type %in% titre_types]

  # Crudely removing titre increases above a given threshold
  # to reduce the chance we are including missed infections
  dt_out <- remove_missed_infections(
    dt_out,
    time_threshold = time_threshold,
    titre_threshold = titre_threshold)

  # Keeping the data only up to the VOC date for comparison
  if(truncate_at_date == TRUE) {
    dt_out <- dt_out[date <= date_set]
  }

  dt_out[, t_since_min_date := as.numeric(
    date - min(date), units = "days")]

  dt_out <- dt_out[
    , lapply(.SD, function(x) if (is.factor(x)) fct_drop(x) else x)]

  return(dt_out)
}

extract_parameters_pop_clean <- function(
    fit, dt_data, dt_data_stan, stan_data,
    formula, format = "long",
    cleaned_names = c(
      "Infection history", "Titre type")) {

  dt_proc <- extract_parameters_pop(
    fit)

  dt_out <- recover_covariate_names(
    dt_proc, dt_data_stan,
    stan_data, formula) |>
    clean_covariate_names(
      formula,
      cleaned_names = cleaned_names)

  dt_out[, k := NULL]
  dt_out[, p := NULL]

  id_vars_custom <- c(cleaned_names, ".draw")

  if(format == "long"){
    dt_out <- melt(
      dt_out,
      id.vars = id_vars_custom)
  }

  return(dt_out)
}
