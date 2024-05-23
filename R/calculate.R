calculate_total_exposures <- function(dt) {

  dt_in <- copy(dt)

  dt_clean <- dt_in

  dt_exposures_wide <- dt_in[, .(id,
                                 dose_1,
                                 dose_2,
                                 dose_3,
                                 dose_4,
                                 dose_5,
                                 date_roche_pos,
                                 episode_1_start,
                                 episode_2_start,
                                 episode_3_start,
                                 episode_4_start,
                                 episode_5_start)] |> unique()

  dt_exposures_long <- melt(dt_exposures_wide,
                            id.vars = "id",
                            variable.name = "event",
                            value.name = "date")

  dt_exposures_long_trim <- dt_exposures_long[, .SD[!is.na(date)], by = id]

  # [, .SD[!all(is.na(date))],
  #   by = id][order(id, date)]

  # dt_exposures_sum <- dt_exposures_long_trim[, .(no_exposures = .N), by = id]

  dt_exposures_long_trim[, exposure_no := 1:.N, by = c("id")]

  dt_out <- merge(dt_in, dt_exposures_long_trim,
                  all.x = TRUE, allow.cartesian = TRUE, roll = "nearest",
                  by = c("id", "date"))

  return(dt_out)

}

calculate_last_exposure <- function(dt) {

  dt_in <- copy(dt)

  dt_exposures_wide <- dt_in[, .(id,
                                 dose_1,
                                 dose_2,
                                 dose_3,
                                 dose_4,
                                 dose_5,
                                 date_roche_pos,
                                 episode_1_start,
                                 episode_2_start,
                                 episode_3_start,
                                 episode_4_start,
                                 episode_5_start)] |> unique()

  dt_exposures_long <- melt(dt_exposures_wide,
                            id.vars = "id",
                            variable.name = "event",
                            value.name = "date")

  dt_exposures_long_trim <- dt_exposures_long[, .SD[!is.na(date)], by = id]

  # [, .SD[!all(is.na(date))],
  #   by = id][order(id, date)]

  dt_last_exposure <- dt_exposures_long_trim[, .SD[date == max(date)], by = c("id")]

  setnames(dt_last_exposure,
           c("event", "date"),
           c("last_exposure", "date_last_exposure"))

  dt_last_exposure[last_exposure %like% "episode" |
                   last_exposure %like% "date_roche",
                   last_exposure := "infection"]

  dt_last_exposure[last_exposure %like% "episode" |
                   last_exposure %like% "date_roche",
                   last_exposure := "infection"]

  dt_last_exposure[last_exposure %like% "dose",
                   last_exposure := "vaccine"]

  dt_out <- merge(dt_in, dt_last_exposure, all.x = TRUE, by = "id")

  return(dt_out)

}

calculate_summaries <- function(dt_pop_post_samples_wide) {

  dt_pop_post_samples_adj <- adjust_parameters(
    dt_pop_post_samples_wide)

  dt_pop_post_samples_adj_long <- melt(
    dt_pop_post_samples_adj, id.vars = c("k", "i", ".draw"))

  dt_pop_post_samples_adj_long[!variable %like% "beta"] |>
    ggplot() +
    geom_density_ridges(aes(x = value, y = factor(k), fill = variable)) +
    facet_grid(i~variable, scales = "free") +
    theme_minimal() +
    labs(x = "Value", y = "Last vaccine type")

  dt_times = data.table(t = seq(0, 200, by = 1))

  # adding artificial ids so that we can do a big merge, adding times to
  # each set of parameter samples
  dt_times[, t_id := 1, by = t]
  dt_pop_post_samples_adj[, t_id := 1]

  dt_trajectories <- merge(
    dt_pop_post_samples_adj, dt_times,
    by = "t_id",
    allow.cartesian = TRUE
  )

  dt_trajectories[,
                  mu := simulate_trajectory(
                    t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop),
                  by = c("t", "i", "k", ".draw")
  ]

  dt_trajectories[, i := factor(i)]
  dt_trajectories[, k := factor(k)]

  # Use the function
  dt_summary <-  summarise_trajectories(
    dt_trajectories,
    by = c("t", "i", "k"))

  dt_summary_plot <- setnames(
    dt_summary,
    c("i", "k"),
    c("Last exposure", "Titre type"))

}

# Function currently requires simplify_voc_names() to be run first -------------
calculate_infection_history <- function(dt_in, use_anti_n = FALSE) {
  dt_out <- copy(dt_in)

  # Define the types of infections
  omicron_types <- c("BQ.1.1", "BA.2", "BA.5", "BA.1", "XBB")
  pre_omicron_types <- c("D614G", "Alpha", "Delta")

  dt_out[, last_inf_category := ""]

  # Categorise the type of the last infection
  dt_out[, last_inf_category := ifelse(
    last_inf_type %in% omicron_types, "Omicron",
    ifelse(last_inf_type %in% pre_omicron_types, "Pre-Omicron", NA)), by = id]

  # Determine infection history
  dt_out[
    , infection_history := ifelse(
      inf_num > 0 & date >= last_inf_date & last_inf_date < last_exp_date,
      paste("Previously infected (", last_inf_category, ")", sep=""), "Infection naive"),
    by = id]

  # Use anti-N data if specified
  if (use_anti_n) {
    # Summarize anti_n_result to check if any are positive within each id and consider the date of the anti-N test
    anti_n_summary <- dt_out[, .(any_anti_n_positive = any(anti_n_result == "positive")), by = id]

    # Merge the summary back into the main data table
    dt_out <- merge(dt_out, anti_n_summary, by = "id", all.x = TRUE)

    # Update infection history based on anti-N data considering the date of the test
    dt_out[
      any_anti_n_positive == TRUE & infection_history == "Infection naive",
      infection_history := "Previously infected (Pre-Omicron)"
    ]

    # Drop the temporary summary column
    dt_out[, any_anti_n_positive := NULL]
  }

  # Drop the temporary column used for categorisation
  dt_out[, last_inf_category := NULL]

  return(dt_out)
}
