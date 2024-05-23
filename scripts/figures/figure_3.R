#---------------#
#--- Panel A ---#
#---------------#

# Processing fits
dt_delta_plot_trunc <- process_fits(
  fit_delta_trunc, dt_delta_trunc, stan_data_delta_trunc,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_ba2_plot_trunc <- process_fits(
  fit_ba2_trunc, dt_ba2_trunc, stan_data_ba2_trunc,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_xbb_plot_trunc <- process_fits(
  fit_xbb_trunc, dt_xbb_trunc, stan_data_xbb_trunc,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_delta_plot_full <- process_fits(
  fit_delta_full, dt_delta_full, stan_data_delta_full,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_ba2_plot_full <- process_fits(
  fit_ba2_full, dt_ba2_full, stan_data_ba2_full,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_xbb_plot_full <- process_fits(
  fit_xbb_full, dt_xbb_full, stan_data_xbb_full,
  covariate_formula, t_max = 150, summarise = FALSE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_all_data_plot <- rbind(
  dt_delta_full[, Wave := "Delta wave"],
  dt_ba2_full[, Wave := "BA.2 wave"],
  dt_xbb_full[, Wave := "XBB wave"])[
    , Wave := factor(Wave)] |>
  clean_covariate_names(
    formula_val = covariate_formula,
    cleaned_names = c("Infection history", "Titre type")) |>
  convert_log_scale_inverse(vars_to_transform = "titre")

dt_all_data_plot[`Titre type` == "XBB", `Titre type` := "XBB.1.5"][
  , `Titre type` := paste0(`Titre type`, " Abs")][
    , `Titre type` := fct_relevel(`Titre type`, c(
      "Ancestral Abs", "Alpha Abs", "Delta Abs",
      "BA.1 Abs", "BA.2 Abs",
      "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

dt_all_fits_trunc <- rbind(
  dt_delta_plot_trunc[, Wave := "Delta wave"],
  dt_ba2_plot_trunc[, Wave := "BA.2 wave"],
  dt_xbb_plot_trunc[, Wave := "XBB wave"])[
    , Wave := factor(Wave)][
      , `Infection history` := factor(`Infection history`)][
        `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
          , `Titre type` := paste0(`Titre type`, " Abs")][
            , `Titre type` := fct_relevel(`Titre type`, c(
              "Ancestral Abs", "Alpha Abs", "Delta Abs",
              "BA.1 Abs", "BA.2 Abs",
              "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

dt_all_fits_full <- rbind(
  dt_delta_plot_full[, Wave := "Delta wave"],
  dt_ba2_plot_full[, Wave := "BA.2 wave"],
  dt_xbb_plot_full[, Wave := "XBB wave"])[
    , Wave := factor(Wave)][
      , `Infection history` := factor(`Infection history`)][
        `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
          , `Titre type` := paste0(`Titre type`, " Abs")][
            , `Titre type` := fct_relevel(`Titre type`, c(
              "Ancestral Abs", "Alpha Abs", "Delta Abs",
              "BA.1 Abs", "BA.2 Abs",
              "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

dt_all_data_plot_main <- dt_all_data_plot[
  `Infection history` != "Previously infected (Omicron)"][
    , `Infection history` := fct_relevel(fct_drop(
      `Infection history`),
      c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique()

dt_all_fits_trunc_main <- dt_all_fits_trunc[
    , `Infection history` := fct_relevel(fct_drop(
      `Infection history`),
      c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique()

dt_all_fits_full_main <- dt_all_fits_full[
    , `Infection history` := fct_relevel(fct_drop(
      `Infection history`),
      c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique()

dt_emergence <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  t = c(69, 76, 89))

dt_dominance <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  t = c(150, 150, 150))

dt_dates <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  date_emergence = c(date_delta, date_ba2, date_xbb))

facet_formula <- `Infection history` ~ Wave + `Titre type`

dt_fits_full_trim <- dt_all_fits_full_main[
  , .(.draw, t, tp_pop, ts_pop, mu, `Titre type`, `Infection history`, Wave)]

dt_fits_trunc_trim <- dt_all_fits_trunc_main[
  , .(.draw, t, tp_pop, ts_pop, mu, `Titre type`, `Infection history`, Wave)]

# Wide format
dt_fits_full_trim_wide <- copy(dt_fits_full_trim)
dt_fits_trunc_trim_wide <- copy(dt_fits_trunc_trim)

setnames(dt_fits_full_trim_wide, "mu", "mu_full")
setnames(dt_fits_trunc_trim_wide, "mu", "mu_trunc")

dt_fits_wide <- merge(
  dt_fits_full_trim_wide, dt_fits_trunc_trim_wide,
  by = c(".draw", "t", "Infection history", "Titre type", "Wave"))

dt_fits_wide_nat <- convert_log_scale(
  dt_fits_wide, vars_to_transform = c("mu_full", "mu_trunc"))

dt_fits_wide[, mu_diff := abs(mu_trunc - mu_full)]

dt_fits_wide_sum <- summarise_draws(
  dt_fits_wide, column_name = "mu_diff",
  by = c("t", "Titre type", "Infection history", "Wave"))

# Long format
dt_fits_long <- rbind(
  dt_fits_full_trim[, Type := "Retrospective"],
  dt_fits_trunc_trim[, Type := "Real-time"])

# dt_fits_long_nat <- convert_log_scale_inverse(
#   dt_fits_long, vars_to_transform = "mu")

dt_fits_long_sum <- summarise_draws(
  dt_fits_long, column_name = "mu",
  by = c("t", "Titre type", "Infection history", "Wave", "Type"))

# Real-time vs retrospective fits
p1 <- dt_fits_long_sum[, Wave := fct_relevel(Wave, "Delta wave")] |>
  ggplot() +
  geom_line(aes(x = t, y = me, colour = Type)) +
  # geom_line(aes(x = t, y = mu_trunc, group = .draw), colour = "blue", alpha = 0.05) +
  geom_ribbon(aes(x = t, ymin = lo, ymax = hi, fill = Type), alpha = 0.5) +
  geom_hline(aes(yintercept = 40),
            linetype = "dashed", colour = "gray30", alpha = 0.4) +
  geom_hline(aes(yintercept = 2560),
             linetype = "dashed", colour = "gray30", alpha = 0.4) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120"),
    limits = c(40, 20480)) +
  facet_nested(`Infection history` ~ Wave + `Titre type`) +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = "black")) +
  labs(x = "Time (days since vaccination)", y = expression(paste("Titre (", IC[50], ")")))

ggsave(
  "outputs/figures/supplementary_figures/realtime_vs_retrospective_colours.png",
  p1,
  width = 12,
  height = 7,
  bg = "white")

p2 <- dt_fits_wide_sum[, Wave := fct_relevel(Wave, "Delta wave")]  |>
  ggplot() +
  geom_line(aes(x = t, y = me)) +
  geom_ribbon(aes(x = t, ymin = lo, ymax = hi), alpha = 0.5) +
  # scale_y_continuous(
    # trans = "log2",
    # breaks = c(40, 80, 160, 320, 640, 1280,
    #            2560, 5120),
    # labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120"),
    # limits = c(NA, NA)) +
  facet_nested(`Infection history` ~ Wave + `Titre type`) +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black")) +
  labs(title = "Absolute difference between retrospective and real-time fits",
       tag = "A",
       x = "Time (days since vaccination)",
       y = expression(paste("Absolute difference (", log[2], IC[50], ")")))

#--- Processing data
dt_delta_trunc_peak_switch <- figure_4_data(
  fit_delta_trunc, dt_delta_trunc,
  stan_data_delta_trunc, covariate_formula,
  wave_manual = "Delta wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_trunc = mu_0, mu_p_trunc = mu_p, mu_s_trunc = mu_s,
        `Infection history`, `Titre type`)]

dt_ba2_trunc_peak_switch <- figure_4_data(
  fit_ba2_trunc, dt_ba2_trunc,
  stan_data_ba2_trunc, covariate_formula,
  wave_manual = "BA.2 wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_trunc = mu_0, mu_p_trunc = mu_p, mu_s_trunc = mu_s,
        `Infection history`, `Titre type`)]

dt_xbb_trunc_peak_switch <- figure_4_data(
  fit_xbb_trunc, dt_xbb_trunc,
  stan_data_xbb_trunc, covariate_formula,
  wave_manual = "XBB wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_trunc = mu_0, mu_p_trunc = mu_p, mu_s_trunc = mu_s,
        `Infection history`, `Titre type`)]

dt_delta_full_peak_switch <- figure_4_data(
  fit_delta_full, dt_delta_full,
  stan_data_delta_full, covariate_formula,
  wave_manual = "Delta wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_full = mu_0, mu_p_full = mu_p, mu_s_full = mu_s,
        `Infection history`, `Titre type`)]

dt_ba2_full_peak_switch <- figure_4_data(
  fit_ba2_full, dt_ba2_full,
  stan_data_ba2_full, covariate_formula,
  wave_manual = "BA.2 wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_full = mu_0, mu_p_full = mu_p, mu_s_full = mu_s,
        `Infection history`, `Titre type`)]

dt_xbb_full_peak_switch <- figure_4_data(
  fit_xbb_full, dt_xbb_full,
  stan_data_xbb_full, covariate_formula,
  wave_manual = "XBB wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0_full = mu_0, mu_p_full = mu_p, mu_s_full = mu_s,
        `Infection history`, `Titre type`)]

# dt_delta_peak_switch <- merge(
#   dt_delta_trunc_peak_switch,
#   dt_delta_full_peak_switch,
#   by = c(".draw", "Titre type", "Infection history"))[, Wave := "Delta"]
#
# dt_ba2_peak_switch <- merge(
#   dt_ba2_trunc_peak_switch,
#   dt_ba2_full_peak_switch,
#   by = c(".draw", "Titre type", "Infection history"))[, Wave := "BA.2"]
#
# dt_xbb_peak_switch <- merge(
#   dt_xbb_trunc_peak_switch,
#   dt_xbb_full_peak_switch,
#   by = c(".draw", "Titre type", "Infection history"))[, Wave := "XBB"]

dt_delta_trunc_peak_switch <- figure_4_data(
  fit_delta_trunc, dt_delta_trunc,
  stan_data_delta_trunc, covariate_formula,
  wave_manual = "Delta wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw,  mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_ba2_trunc_peak_switch <- figure_4_data(
  fit_ba2_trunc, dt_ba2_trunc,
  stan_data_ba2_trunc, covariate_formula,
  wave_manual = "BA.2 wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_xbb_trunc_peak_switch <- figure_4_data(
  fit_xbb_trunc, dt_xbb_trunc,
  stan_data_xbb_trunc, covariate_formula,
  wave_manual = "XBB wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_delta_full_peak_switch <- figure_4_data(
  fit_delta_full, dt_delta_full,
  stan_data_delta_full, covariate_formula,
  wave_manual = "Delta wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_ba2_full_peak_switch <- figure_4_data(
  fit_ba2_full, dt_ba2_full,
  stan_data_ba2_full, covariate_formula,
  wave_manual = "BA.2 wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_xbb_full_peak_switch <- figure_4_data(
  fit_xbb_full, dt_xbb_full,
  stan_data_xbb_full, covariate_formula,
  wave_manual = "XBB wave",
  cleaned_names = c("Infection history", "Titre type"))[
    , .(.draw, mu_0, mu_p, mu_s, `Infection history`, `Titre type`)]

dt_delta_peak_switch <- rbind(
  dt_delta_trunc_peak_switch[, Type := "Real-time"],
  dt_delta_full_peak_switch[, Type := "Retrospective"])[
  , Wave := "Delta"]

dt_ba2_peak_switch <- rbind(
  dt_ba2_trunc_peak_switch[, Type := "Real-time"],
  dt_ba2_full_peak_switch[, Type := "Retrospective"])[
    , Wave := "BA.2"]

dt_xbb_peak_switch <- rbind(
  dt_xbb_trunc_peak_switch[, Type := "Real-time"],
  dt_xbb_full_peak_switch[, Type := "Retrospective"])[
    , Wave := "XBB"]

dt_peak_switch <- rbind(
  dt_delta_peak_switch, dt_ba2_peak_switch, dt_xbb_peak_switch)

dt_peak_switch_long <- melt(dt_peak_switch, measure.vars = c("mu_0", "mu_p", "mu_s"))

dt_bayesian_estimates <- summarise_draws(
  dt_peak_switch_long,
  column_name = "value",
  by = c("Infection history", "Titre type", "Type", "Wave", "variable"))

dt_bayesian_estimates[, `Model Type` := "Bayesian"]

p_mu_0 <- dt_peak_switch[
  , Wave := fct_relevel(Wave, "Delta")][
  , `Infection history` := fct_relevel(
    `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |>
  ggplot() +
  geom_boxplot(
    outlier.shape = NA,
    aes(x = Type, y = mu_0, fill = `Titre type`)) +
  # geom_boxplot(aes(x = `Wave`, y = mu_0_full, fill = `Titre type`)) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  facet_grid(`Infection history` ~ Wave + `Titre type`, scales = "free") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Model type", y = "Titre value at time of exposure")

p_mu_p <- dt_peak_switch[, Wave := fct_relevel(Wave, "Delta")][
  , `Infection history` := fct_relevel(
    `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |>
  ggplot() +
  geom_boxplot(
    outlier.shape = NA,
    aes(x = Type, y = mu_p, fill = `Titre type`)) +
  # geom_boxplot(aes(x = `Wave`, y = mu_0_full, fill = `Titre type`)) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  facet_grid(`Infection history` ~ Wave + `Titre type`, scales = "free") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Model type", y = "Titre value at peak")

p_mu_s <- dt_peak_switch[, Wave := fct_relevel(Wave, "Delta")][
  , `Infection history` := fct_relevel(
    `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))] |>
  ggplot() +
  geom_boxplot(
    outlier.shape = NA,
    aes(x = Type, y = mu_s, fill = `Titre type`)) +
  # geom_boxplot(aes(x = `Wave`, y = mu_0_full, fill = `Titre type`)) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280,
               2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  facet_grid(`Infection history` ~ Wave + `Titre type`, scales = "free") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(x = "Model type", y = "Titre value at set-point")

apply_model <- function(data, formula, points, model_func, model_type) {
  # Apply the model function based on the provided model type
  data[, .(Model = list(model_func(.SD, formula, points))), by = .(titre_type, infection_history)]
}

# Recover names generalized function
recover_names <- function(model_list) {
  recover_covariate_names_frequentist(model_list)
}

# Combine results into a single data.table
combine_results <- function(model_list) {
  rbindlist(model_list$Model, use.names = TRUE)
}

# Model formulas and points
formula <-  titre ~ t_since_last_exp
points <-  150
critical_value <- qnorm(0.975)

# List of datasets
datasets <- list(
  delta_trunc = dt_delta_trunc,
  delta_full = dt_delta_full,
  ba2_trunc = dt_ba2_trunc,
  ba2_full = dt_ba2_full,
  xbb_trunc = dt_xbb_trunc,
  xbb_full = dt_xbb_full
)

# Process models with clear identification of model type
results_list <- list()
model_types <- c("loess", "lm")  # Define model types for clarity
for (dataset_name in names(datasets)) {
  for (model_type in model_types) {
    model_func <- if (model_type == "lm") lm_wrapper_dt else loess_wrapper_dt
    model_name <- sprintf("%s_%s", sub("_", "", dataset_name), model_type)
    results_list[[model_name]] <- apply_model(datasets[[dataset_name]], formula, points, model_func, model_type)
    results_list[[model_name]] <- recover_names(results_list[[model_name]])
  }
}

# Combine results
combined_results <- lapply(results_list, combine_results)

assign_wave_type <- function(dt, name) {
  # Extract the base wave name and model type from the name
  wave <- sub("^(.*)_(loess|lm)$", "\\1", name)  # Extract base name before _loess or _lm
  wave <- gsub("trunc|full", "", wave)  # Clean the wave name by removing trunc or full
  model_type <- if (grepl("_loess", name)) "loess" else "lm"  # Determine model type based on name suffix

  # Map to formal wave names
  wave <- switch(wave, delta = "Delta", ba2 = "BA.2", xbb = "XBB", wave)

  type <- if (grepl("trunc", name)) "Real-time" else "Retrospective"

  # Assign extracted information to the data.table
  dt[, `:=` (Wave = wave, Type = type, Model_Type = model_type)]
  return(dt)
}

# Combine all results and assign wave and type
dt_freq_models <- rbindlist(lapply(names(combined_results), function(name) {
  assign_wave_type(combined_results[[name]], name)
}), use.names = TRUE)

# dt_freq_models |>
#   ggplot() +
#   geom_ribbon(aes(x = t_since_last_exp, ymin = lwr, ymax = upr, fill = titre_type)) +
#   facet_grid(Model_Type + infection_history + Type ~ Wave + titre_type, scales = "free") +
#   scale_y_continuous(trans = "log2")

# Define a function to extract parameters and subset specific columns
extract_and_subset <- function(fit, data, data_stan, formula, type) {
  extract_parameters_pop_clean(
    fit = fit, data, data, data_stan, formula, format = "wide"
  )[, .(t0_pop, tp_pop, ts_pop, `Infection history`, `Titre type`)]
}

# Define a function to calculate means and tag with additional info
calculate_means <- function(data, wave, type) {
  data[, .(
    t0_pop_mean = mean(t0_pop),
    tp_pop_mean = mean(tp_pop),
    ts_pop_mean = mean(ts_pop)),
       by = .(`Titre type`, `Infection history`)
  ][, `:=` (Wave = wave, Type = type)]
}

# List of scenarios to loop through
scenarios <- list(
  delta_trunc = list(fit = fit_delta_trunc, data = dt_delta_trunc, stan_data = stan_data_delta_trunc, wave = "Delta", type = "Real-time"),
  delta_full = list(fit = fit_delta_full, data = dt_delta_full, stan_data = stan_data_delta_full, wave = "Delta", type = "Retrospective"),
  ba2_trunc = list(fit = fit_ba2_trunc, data = dt_ba2_trunc, stan_data = stan_data_ba2_trunc, wave = "BA.2", type = "Real-time"),
  ba2_full = list(fit = fit_ba2_full, data = dt_ba2_full, stan_data = stan_data_ba2_full, wave = "BA.2", type = "Retrospective"),
  xbb_trunc = list(fit = fit_xbb_trunc, data = dt_xbb_trunc, stan_data = stan_data_xbb_trunc, wave = "XBB", type = "Real-time"),
  xbb_full = list(fit = fit_xbb_full, data = dt_xbb_full, stan_data = stan_data_xbb_full, wave = "XBB", type = "Retrospective")
)

# Loop through each scenario, extract parameters, and calculate means
results <- lapply(scenarios, function(s) {
  params <- extract_and_subset(s$fit, s$data, s$stan_data, covariate_formula, "wide")
  calculate_means(params, s$wave, s$type)
})

# Combine all results into a single data.table
dt_t_values <- rbindlist(results)

dt_t_values_lm <- copy(dt_t_values)
dt_t_values_loess <- copy(dt_t_values)

dt_t_values <- rbind(
  dt_t_values_lm[, `Model Type` := "lm"],
  dt_t_values_loess[, `Model Type` := "loess"])

setnames(
  dt_freq_models,
  c("titre_type", "infection_history", "fit", "lwr", "upr", "Model_Type"),
  c("Titre type", "Infection history", "me", "lo", "hi", "Model Type"))

dt_freq_models_t_merged <- merge(
  dt_freq_models, dt_t_values,
  by = c("Titre type", "Wave", "Infection history", "Type", "Model Type"))

dt_0 <- dt_freq_models_t_merged[
  , .SD[t_since_last_exp == 0],
  by = .(`Titre type`, Wave, `Infection history`, Type, `Model Type`)]

dt_p <- dt_freq_models_t_merged[
  , .SD[which.min(abs(t_since_last_exp - tp_pop_mean))],
  by = .(`Titre type`, Wave, `Infection history`, Type, `Model Type`)]

dt_s <- dt_freq_models_t_merged[
  , .SD[which.min(abs(t_since_last_exp - ts_pop_mean))],
  by = .(`Titre type`, Wave, `Infection history`, Type, `Model Type`)]

dt_bayesian_estimates[variable == "mu_0", `Time point` := "Time of exposure"]
dt_bayesian_estimates[variable == "mu_p", `Time point` := "Time of peak"]
dt_bayesian_estimates[variable == "mu_s", `Time point` := "Time of set point"]

dt_bayesian_estimates[, variable := NULL]

dt_freq <- rbind(
  dt_0[, `Time point` := "Time of exposure"],
  dt_p[, `Time point` := "Time of peak"],
  dt_s[, `Time point` := "Time of set point"])

dt_freq[, t_since_last_exp := NULL]
dt_freq[, t0_pop_mean := NULL]
dt_freq[, tp_pop_mean := NULL]
dt_freq[, ts_pop_mean := NULL]

dt_model_comparison <- rbind(dt_bayesian_estimates, dt_freq) |>
  relevel_factors_for_plots()

p_figure_3 <- ggplot(dt_model_comparison[`Time point` == "Time of peak"][
  !(Type == "Real-time" & `Model Type` == "lm" &
      `Infection history` == "Previously infected (Omicron)" &
      Wave == "BA.2 wave")][
        , Wave := fct_relevel(Wave, "Delta wave")][
          , `Infection history` := fct_relevel(
            `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))][
            `Infection history` != "Previously infected (Omicron)"],
  aes(x = interaction(Type, `Model Type`, sep = " - "), y = me, ymin = lo, ymax = hi, colour = `Model Type`)) +
  geom_pointrange() +
  geom_hline(
    data = dt_model_comparison[
      `Model Type` == "Bayesian" & Type == "Retrospective" &
        `Time point` == "Time of peak"][
          `Infection history` != "Previously infected (Omicron)"],
    aes(yintercept = me), linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120),
    labels = c("10", "20", "40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  facet_nested(`Infection history` ~ Wave + `Titre type`, scales = "free") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom") +
  labs(title = "Difference between retrospective and real-time fits at peak titre by model type",
       tag = "B",
       x = "Model type", y = "Titre value at peak") +
  scale_x_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1))

p_sX <- ggplot(dt_model_comparison[
  !(Type == "Real-time" & `Model Type` == "lm" &`Infection history` == "Previously infected (Omicron)" & Wave == "BA.2 wave")][
    , Wave := fct_relevel(Wave, "Delta wave")][
      , `Infection history` := fct_relevel(
        `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))],
  aes(x = interaction(Type, `Model Type`, sep = " - "),
      y = me, ymin = lo, ymax = hi, colour = `Model Type`, shape = `Infection history`)) +
  geom_pointrange() +
  geom_hline(
    data = dt_model_comparison[
      `Model Type` == "Bayesian" & Type == "Retrospective"],
    aes(yintercept = me), linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(10, 20, 40, 80, 160, 320, 640, 1280, 2560, 5120),
    labels = c("10", "20", "40", "80", "160", "320", "640", "1280", "2560", "5120")) +
  facet_nested(`Infection history` + `Time point` ~  Wave + `Titre type`, scales = "free") +
  theme_linedraw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom") +
  labs(x = "Model type", y = "Titre value") +
  scale_x_discrete(labels = function(x) sapply(strsplit(x, " - "), `[`, 1))

p_figure_3_all <- cowplot::plot_grid(p2, p_figure_3, ncol = 1, rel_heights = c(1, 1.2))

# Saving main figure --- just peak titre values
ggsave("outputs/figures/figure_3.png", p_figure_3_all, width = 10, height = 12)
ggsave("outputs/figures/figure_3.pdf", p_figure_3_all, width = 10, height = 12)

# Saving supplementary figure with all three time points
ggsave(
  "outputs/figures/supplementary_figures/realtime_vs_retrospective.png",
  p_sX, width = 13, height = 13)

ggsave(
  "outputs/figures/supplementary_figures/realtime_vs_retrospective.eps",
  p_sX, width = 13, height = 13)

ggsave(
  "outputs/figures/supplementary_figures/realtime_vs_retrospective.pdf",
  p_sX, width = 13, height = 13)



