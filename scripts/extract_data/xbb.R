#-------------------------------------------------------#
#--- Extracting and cleaning wave data for inference ---#
#-------------------------------------------------------#

# Setting most relevant titre types for each wave
titre_types_xbb <- c("BA.5", "BQ.1.1", "XBB")

# Extracting wave data
dt_xbb_data_full <- extract_wave_data(
  dt_in = dt_clean,
  date_set = date_xbb,
  t_max = 150,
  covariate_formula = covariate_formula,
  titre_types = titre_types_xbb,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

# Truncating the dataset at chosen date, for real-time fit
dt_xbb_data_trunc <- dt_xbb_data_full[date <= date_xbb]

# Preparing data for inference
dt_xbb_full_stan <- prepare_stan_data(dt_xbb_data_full)
dt_xbb_trunc_stan <- prepare_stan_data(dt_xbb_data_trunc)

# Trimming data down to minimal set required for inference
dt_xbb_full_stan_trim <- dt_xbb_full_stan[, ..cols_to_keep]
dt_xbb_trunc_stan_trim <- dt_xbb_trunc_stan[, ..cols_to_keep]

# Saving minimal dataset
fwrite(dt_xbb_full_stan_trim, "data/xbb_full.rds")
fwrite(dt_xbb_trunc_stan_trim, "data/xbb_trunc.rds")