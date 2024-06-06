summarise_draws <- function(dt_in, column_name, by = by) {

  dt_out <- dt_in[,.(
    me = quantile(get(column_name), 0.5),
    lo = quantile(get(column_name), 0.025),
    hi = quantile(get(column_name), 0.975)
  ),
  by = by
  ]

  return(dt_out)
}

summarise_draws_vector <- function(
    dt_in, column_names, by = NULL, format = "long") {
  # Function to calculate the quantiles for each column and rename
  summarise_column <- function(column_data, column_name) {
    quants <- quantile_fun(column_data)
    setNames(quants, paste0(names(quants), "_", column_name))
  }

  # Create an empty list to collect our results
  results <- vector("list", length(column_names))

  # Loop through each column name and apply the summarise_column function
  for (i in seq_along(column_names)) {
    column_name <- column_names[i]
    results[[i]] <- dt_in[, summarise_column(.SD[[column_name]], column_name), by = by]
  }

  # Merge the results by the 'by' columns if they exist
  if (!is.null(by)) {
    dt_out <- Reduce(function(...) merge(..., by = by, all = TRUE), results)
  } else {
    dt_out <- do.call(cbind, results)
  }

  if(format == "long") {
    # Extract unique parameter names from the column names
    all_columns <- colnames(dt_out)
    measure_columns <- all_columns[grepl("^(me_|lo_|hi_)", all_columns)]
    unique_parameters <- unique(sub("^(me_|lo_|hi_)(.*)", "\\2", measure_columns))

    # Melt the data.table to long format
    dt_out <- melt(
      dt_out,
      id.vars = c("stan_id", "titre_type", "infection_history"),
      measure.vars = patterns("^me_", "^lo_", "^hi_"),
      value.name = c("me", "lo", "hi"),
      variable.name = "parameter")

    # Map the numerical 'parameter' values to actual parameter names dynamically
    dt_out[, parameter := factor(parameter, labels = unique_parameters)]
  }

  return(dt_out)
}

summarise_pop_fit <- function(
    fit, time_range = seq(0, 200, 1), summarise = TRUE,
    n_draws = 2500) {

  dt_samples_wide <- spread_draws(
    fit,
    t0_pop[k], tp_pop[k], ts_pop[k],
    m1_pop[k], m2_pop[k], m3_pop[k],
    beta_t0[p], beta_tp[p], beta_ts[p],
    beta_m1[p], beta_m2[p], beta_m3[p]) |>
    data.table()

  dt_samples_wide <- dt_samples_wide[.draw %in% 1:n_draws]

  dt_samples_wide[, `:=` (.chain = NULL, .iteration = NULL)]

  setcolorder(dt_samples_wide, c("k", "p", ".draw"))

  dt_samples_wide_adj <- adjust_parameters(dt_samples_wide)

  dt_times = data.table(t = time_range)

  # Artificial time IDs so merge creates all time points for each sample
  dt_times[, t_id := 1, by = t]
  dt_samples_wide_adj[, t_id := 1]

  dt_out <- merge(
    dt_samples_wide_adj, dt_times, by = "t_id", allow.cartesian = TRUE)

  dt_out[, mu := simulate_trajectory(
    t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
    by = c("t", "p", "k", ".draw")]

  if(summarise == TRUE) {
    dt_out <- summarise_draws(
      dt_out, column_name = "mu", by = c("t", "p", "k"))
  }

  return(dt_out)
}


