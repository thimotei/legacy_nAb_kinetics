#-------------------------------------------------------#
#--- Extracting and cleaning wave data for inference ---#
#-------------------------------------------------------#

# Setting most relevant titre types for each wave
titre_types_ba2 <- c("Delta", "BA.1", "BA.2")

# Extracting wave data
dt_ba2_data_full <- extract_wave_data(
  dt_in = dt_clean,
  date_set = date_ba2,
  t_max = 150,
  covariate_formula = covariate_formula,
  titre_types = titre_types_ba2,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

# Truncating the dataset at chosen date, for real-time fit
dt_ba2_data_trunc <- dt_ba2_data_full[date <= date_ba2]

# Preparing data for inference
dt_ba2_full_stan <- prepare_stan_data(dt_ba2_data_full)
dt_ba2_trunc_stan <- prepare_stan_data(dt_ba2_data_trunc)

# Trimming data down to minimal set required for inference
dt_ba2_full_stan_trim <- dt_ba2_full_stan[, ..cols_to_keep]
dt_ba2_trunc_stan_trim <- dt_ba2_trunc_stan[, ..cols_to_keep]

# Saving minimal dataset
fwrite(dt_ba2_full_stan_trim, "data/ba2_full.rds")
fwrite(dt_ba2_trunc_stan_trim, "data/ba2_trunc.rds")

