library(Rcpp)
library(parallel)

sourceCpp("cpp/simulate_trajectories.cpp")
sourceCpp("cpp/convert_log_scale_inverse.cpp")

source("scripts/setup/setup.R")
source("scripts/inference/delta.R")
source("scripts/inference/ba2.R")
source("scripts/inference/xbb.R")

cols_to_keep <- c(
  "stan_id", "date", "titre", "titre_type",
  "last_exp_date", "last_exp_type", "vax_num",
  "t_since_last_exp", "infection_history")

fit_delta_full <- readRDS("outputs/fits/delta_full.rds")
fit_ba2_full <- readRDS("outputs/fits/ba2_full.rds")
fit_xbb_full <- readRDS("outputs/fits/xbb_full.rds")

# Define your range of time_shift values
time_shift_values <- seq(-75, 75, by = 15)

indices <- seq_along(time_shift_values)  # Generate indices for time_shift_values

results_list <- lapply(indices, function(index) {
  shift <- time_shift_values[index]  # Get the time_shift value for the current index
  print(paste("Processing index:", index, "with time shift:", shift))  # Print the current index and time shift

  # Run your function with the current value of time_shift
  result <- simulate_and_sum_pop_mean_from_ind(
    fit_delta_full, dt_delta_full_stan,
    fit_ba2_full, dt_ba2_full_stan,
    fit_xbb_full, dt_xbb_full_stan, n_draws = 10,
    wave_1 = "Delta", wave_2 = "BA.2", wave_3 = "XBB",
    time_shift = shift, adjust_dates = TRUE,
    formula = formula_delta
  )

  # Add a column for the time_shift value
  result[, time_shift := shift]

  return(result)
})

# Combine all results into one data.table
dt_figure_4_data_full <- data.table(rbindlist(results_list))

fwrite(dt_figure_4_data_full, "outputs/data/figure_4_data.rds")

