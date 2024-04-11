#-------------------------------------------------------#
#--- Extracting and cleaning wave data for inference ---#
#-------------------------------------------------------#

# Specifying formula for the subsequent inference
formula_xbb <- ~ 0 + infection_history

# Defining pre-wave/post-vaccine dates
date_xbb <- ymd("2023-01-09")

# Setting most relevant titre types for each wave
titre_types_xbb <- c("BA.5", "BQ.1.1", "XBB")

# Setting most relevant exposure types for each wave
# exp_types_xbb <- c("mRNA1273.214", "BNT162b2+BA.1", "BNT162b2", "mRNA1273")
exp_types_xbb <- c("")

# Extracting wave data
dt_xbb_data_full <- extract_wave_data(
  dt_in = dt_clean,
  date_set = date_xbb,
  t_max = 150,
  covariate_formula = formula_xbb,
  titre_types = titre_types_xbb,
  exposure_types = exp_types_xbb,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

# Plotting raw data
dt_xbb_data_full |>
  ggplot(aes(x = t_since_last_exp, y = titre, colour = titre_type)) +
  geom_point(size = 0.2) +
  geom_line(aes(x = t_since_last_exp, y = titre, group = interaction(titre_type, id), colour = titre_type), alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_grid(infection_history ~ titre_type) +
  scale_colour_lancet() +
  theme_bw()

# Truncating the dataset at chosen date, for real-time fit
dt_xbb_data_trunc <- dt_xbb_data_full[date <= date_xbb]

# Preparing data for inference
dt_xbb_full_stan <- prepare_stan_data(dt_xbb_data_full)
dt_xbb_trunc_stan <- prepare_stan_data(dt_xbb_data_trunc)

# Retrieving data in format for Stan
stan_data_xbb_full <- retrieve_stan_data(
  dt_xbb_full_stan, time_type = "relative", formula_xbb)
stan_data_xbb_trunc <- retrieve_stan_data(
  dt_xbb_trunc_stan, time_type = "relative", formula_xbb)

# Running inference - only runs if posteriors not yet generated -----------

if(!file.exists("outputs/fits/xbb_trunc.rds")){
  
  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))
  
  # Fitting model to real-time data
  fit_xbb_trunc <- mod$sample(
    data = stan_data_xbb_trunc,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)
  
  # Fitting model to full data
  fit_xbb_full <- mod$sample(
    data = stan_data_xbb_full,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Saving fits
  fit_xbb_trunc$save_object("outputs/fits/xbb_trunc.rds")
  fit_xbb_full$save_object("outputs/fits/xbb_full.rds")
  
}
