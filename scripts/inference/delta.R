# Extracting and cleaning wave data for inference -------------------------

# Specifying formula for the subsequent inference
formula_delta <- ~ 0 + infection_history

# Defining pre-wave/post-vaccine dates
date_delta <- ymd("2021-05-07")

# Setting most relevant titre types for each wave
titre_types_delta <- c("Ancestral", "Alpha", "Delta")

# Setting most relevant exposure types for each wave
# Slightly strange syntax for keeping all vaccine types for main analysis
exp_types_delta <- c("")

# Extracting wave data
dt_delta_data_full <- extract_wave_data(
  dt_in = dt_clean,
  date_set = date_delta,
  t_max = 150,
  covariate_formula = formula_delta,
  titre_types = titre_types_delta,
  exposure_types = exp_types_delta,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

#--- Plotting raw data, commented out by default
dt_delta_data_full |>
  ggplot(aes(x = t_since_last_exp, y = titre, colour = titre_type)) +
  geom_point(size = 0.2) +
  geom_line(aes(x = t_since_last_exp, y = titre, group = interaction(titre_type, id), colour = titre_type), alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_grid(infection_history + last_exp_type ~ titre_type) +
  scale_colour_lancet() +
  theme_bw()

#--- Removing dodgy observations after discussion with Crick collaborators
dt_delta_data_full <- dt_delta_data_full[!obs_id %in% c(21615, 560758, 995249)]

# Truncating the dataset at chosen date, for real-time fit
dt_delta_data_trunc <- dt_delta_data_full[date <= date_delta]

# Preparing data for inference
dt_delta_full_stan <- prepare_stan_data(dt_delta_data_full)
dt_delta_trunc_stan <- prepare_stan_data(dt_delta_data_trunc)

# Retrieving data in format for Stan
stan_data_delta_full <- retrieve_stan_data(
  dt_delta_full_stan, time_type = "relative", formula_delta)
stan_data_delta_trunc <- retrieve_stan_data(
  dt_delta_trunc_stan, time_type = "relative", formula_delta)

# Running inference - only runs if posteriors not yet generated -----------

if(!file.exists("outputs/fits/delta_trunc.rds")){

  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))
  
  # Fitting the model to the real-time dataset, i.e. truncated at chosen date
  fit_delta_trunc <- mod$sample(
    data = stan_data_delta_trunc,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)
  
  # Fitting the model to the full dataset
  fit_delta_full <- mod$sample(
    data = stan_data_delta_full,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)
  
  # Saving fits
  fit_delta_trunc$save_object("outputs/fits/delta_trunc.rds")
  fit_delta_full$save_object("outputs/fits/delta_full.rds")
  
}
