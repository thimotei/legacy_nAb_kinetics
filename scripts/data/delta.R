# Extracting and cleaning wave data for inference -------------------------

# Setting most relevant titre types for each wave
titre_types_delta <- c("Ancestral", "Alpha", "Delta")

# Extracting wave data
dt_delta_data_full <- extract_wave_data(
  dt_in = dt_clean,
  date_set = date_delta,
  t_max = 150,
  covariate_formula = covariate_formula,
  titre_types = titre_types_delta,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

#--- Removing dodgy observations after discussion with Crick collaborators
dt_delta_data_full <- dt_delta_data_full[!obs_id %in% c(21615, 560758, 995249)]

# Truncating the dataset at chosen date, for real-time fit
dt_delta_data_trunc <- dt_delta_data_full[date <= date_delta]

# Preparing data for inference
dt_delta_full_stan <- prepare_stan_data(dt_delta_data_full)
dt_delta_trunc_stan <- prepare_stan_data(dt_delta_data_trunc)

# Trimming data down to minimal set required for inference
dt_delta_full_stan_trim <- dt_delta_full_stan[, ..cols_to_keep]
dt_delta_trunc_stan_trim <- dt_delta_trunc_stan[, ..cols_to_keep]

# Saving minimal dataset
fwrite(dt_delta_full_stan_trim, "data/delta_full.rds")
fwrite(dt_delta_trunc_stan_trim, "data/delta_trunc.rds")
