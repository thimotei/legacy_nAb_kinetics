library(data.table)
library(future)
library(future.apply)

# Function to process each chunk within data.table
process_chunk <- function(chunk, n_draws, dt_titre_types, dt_lookup) {
  message("Processing chunk...")

  # Trim data to include only the specified draws
  dt_params_ind_trim <- chunk[draw %in% 1:n_draws, ]

  message("Running C++ simulation...")
  # Select only the required columns for the C++ function
  dt_params_ind_trim <- dt_params_ind_trim[, .(
    stan_id, t0_ind, tp_ind, ts_ind, m1_ind, m2_ind, m3_ind, t_max, titre_type, draw
  )]

  # Running the C++ code to simulate trajectories for each parameter sample for each individual
  dt_params_ind_traj <- tryCatch({
    simulation_wrapper_cpp(dt_params_ind_trim) |> data.table()
  }, error = function(e) {
    message("Error in simulation_wrapper_cpp: ", e$message)
    return(NULL)
  })

  if (is.null(dt_params_ind_traj)) {
    stop("C++ simulation failed")
  }

  message("Merging titre types...")
  # Merge with titre types using precomputed lookup table
  dt_params_ind_traj <- merge(
    dt_params_ind_traj,
    dt_titre_types,
    by = "titre_type_num",
    all.x = TRUE
  )[, titre_type_num := NULL]

  message("Resolving calendar dates...")
  # Resolve calendar dates using exposure dates
  dt_out <- merge(dt_params_ind_traj, dt_lookup, by = "stan_id")

  dt_out[, calendar_date := exposure_date + t, by = .(stan_id, titre_type)]

  message("Calculating means...")
  # Calculate the mean for each combination
  dt_trajectories_mean <- dt_out[
    !is.nan(mu), .(pop_mu_mean = mean(mosaic::resample(mu))),
    by = .(calendar_date, draw, titre_type)
  ]

  message("Summarising results...")
  dt_trajectories_summary <- summarise_draws(
    dt_trajectories_mean,
    column_name = "pop_mu_mean",
    by = c("calendar_date", "titre_type")
  )

  return(dt_trajectories_summary)
}

# Main function to process all waves
simulate_and_sum_pop_mean_from_ind_parallel <- function(
    fit_1, data_1, fit_2, data_2, fit_3, data_3,
    wave_1 = "Delta", wave_2 = "BA.2", wave_3 = "XBB",
    adjust_dates, n_draws, time_shift, t_max = 150) {

  fits <- list(fit_1, fit_2, fit_3)
  data_list <- list(data_1, data_2, data_3)
  waves <- list(wave_1, wave_2, wave_3)

  results_list <- vector("list", length(fits))

  for (i in seq_along(fits)) {
    fit <- fits[[i]]
    dt_data <- data_list[[i]]
    wave_manual <- waves[[i]]

    # Extracting parameters from fit
    dt_params_ind <- extract_parameters_ind(fit, add_variation_params = FALSE)

    # Create titre type lookup table
    dt_titre_types <- data.table(
      titre_type = dt_data[, unique(titre_type)],
      titre_type_num = dt_params_ind[, unique(titre_type)]
    )

    # Create infection history lookup table
    dt_lookup <- dt_data[, .(
      exposure_date = if (adjust_dates) min(last_exp_date) - time_shift else min(last_exp_date)),
      by = .(id, stan_id, infection_history)
    ]

    # Merge dt_params_ind with infection histories
    dt_params_ind <- merge(dt_params_ind, dt_lookup, by = "stan_id")

    dt_params_ind[, t_max := t_max]

    # Split the data into manageable chunks based on titre_type
    split_data <- split(dt_params_ind, by = "titre_type")

    # Plan for parallel processing
    # plan(multisession, workers = availableCores())

    # Process each chunk in parallel
    chunk_results <- lapply(
      split_data, process_chunk, n_draws = n_draws,
       dt_titre_types = dt_titre_types, dt_lookup = dt_lookup)

    # Combine results from all chunks
    dt_trajectories_mean_sum <- rbindlist(chunk_results, use.names = TRUE, fill = TRUE)

    results_list[[i]] <- dt_trajectories_mean_sum
  }

  # Combine results from all waves
  dt_trajectories_mean_sum_all <- rbindlist(results_list, use.names = TRUE, fill = TRUE)

  return(dt_trajectories_mean_sum_all)
}

# result <- simulate_and_sum_pop_mean_from_ind_parallel(
#   fit_1 = fit_delta_full, data_1 = dt_delta_full,
#   fit_2 = fit_ba2_full, data_2 = dt_ba2_full,
#   fit_3 = fit_xbb_full, data_3 = dt_xbb_full,
#   wave_1 = "Delta", wave_2 = "BA.2", wave_3 = "XBB",
#   adjust_dates = FALSE, n_draws = 100, time_shift = 0
# )
#
# # Combine results from all chunks
# dt_trajectories_mean_sum <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
#
# dt_trajectories_mean_sum_plot <- convert_log_scale_inverse(result)
#
# dt_trajectories_mean_sum_plot |>
#   ggplot() +
#   geom_line(aes(x = calendar_date, y = me,
#                 colour = factor(titre_type))) +
#   geom_ribbon(
#     aes(x = calendar_date, ymin = lo, ymax = hi,
#         fill = factor(titre_type)),
#     alpha = 0.5) +
#   # geom_smooth(
#   #   aes(x = calendar_date, y = mu,
#   #       colour = factor(titre_type),
#   #       group = interaction(titre_type)),
#   #   alpha = 0.1) +
#   # facet_grid(~infection_history) +
#   scale_y_continuous(
#     trans = "log2",
#     breaks = c(40, 80, 160, 320, 640, 1280, 2560),
#     labels = c(expression(""<=40),
#                "80", "160", "320", "640", "1280",
#                expression("">=2560)))
#
#
