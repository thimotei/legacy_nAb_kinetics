# Supplementary figure to investigate the difference in kinetics
# when anti-N data is included in the determination of individuals infection
# history

# Replicating preprocessing data script with anti-N included --------------
library(ggh4x)
library(cmdstanr)
library(data.table)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggsci)
library(tidybayes)
library(stringr)

# This is the full datafile, which is not publicly available
load("data_raw/Legacy_DataAnnotatedDateSeries_2023-11-09.RData")

# Convert the data.frame to a data.table
dt_raw <- data.table(chrono.df)

# Setting seed, so jumbling of IDs is the same each time
set.seed(123)

dt_clean_anti_n <- preprocess_data(
  dt_raw, trim_nas = TRUE, convert_scale = TRUE,
  add_infection_history = T,
  simplify_voc_names = TRUE, use_anti_n = TRUE)

# Replicating extract_data script for Delta -------------------------------

# Setting most relevant titre types for each wave
titre_types_delta <- c("Ancestral", "Alpha", "Delta")
date_delta <- ymd("2021-05-07")

# Defining columns to be kept across all three wave datasets
cols_to_keep <- c(
  "id", "stan_id", "date", "obs_id", "t_since_last_exp",
  "infection_history", "titre", "titre_type", "titre_type_num",
  "last_exp_date", "relevant_last_exp_date", "censored")

covariate_formula <- ~ 0 + infection_history

# Extracting wave data
dt_delta_data_full_anti_n <- extract_wave_data(
  dt_in = dt_clean_anti_n,
  date_set = date_delta,
  t_max = 150,
  covariate_formula = covariate_formula,
  titre_types = titre_types_delta,
  truncate_at_date = FALSE,
  time_threshold = 50,
  titre_threshold = 1)

dt_delta_data_full_anti_n <- dt_delta_data_full_anti_n[
  , infection_history := factor(infection_history)]

#--- Removing dodgy observations after discussion with Crick collaborators
dt_delta_data_full_anti_n <- dt_delta_data_full_anti_n[!obs_id %in% c(21615, 560758, 995249)]

# Truncating the dataset at chosen date, for real-time fit
dt_delta_data_trunc_anti_n <- dt_delta_data_full_anti_n[date <= date_delta]

# Preparing data for inference
dt_delta_full_stan_anti_n <- prepare_stan_data(dt_delta_data_full_anti_n)
dt_delta_trunc_stan_anti_n <- prepare_stan_data(dt_delta_data_trunc_anti_n)

# Trimming data down to minimal set required for inference
dt_delta_full_stan_trim_anti_n <- dt_delta_full_stan_anti_n[, ..cols_to_keep]
dt_delta_trunc_stan_trim_anti_n <- dt_delta_trunc_stan_anti_n[, ..cols_to_keep]

# Saving minimal dataset
fwrite(dt_delta_full_stan_trim_anti_n, "data/delta_full_anti_n.rds")
fwrite(dt_delta_trunc_stan_trim_anti_n, "data/delta_trunc_anti_n.rds")

# Running inference -------------------------------------------------------
stan_data_delta_full_anti_n <- retrieve_stan_data(
  dt_delta_full_stan_trim_anti_n, time_type = "relative", covariate_formula)
stan_data_delta_trunc_anti_n <- retrieve_stan_data(
  dt_delta_trunc_stan_trim_anti_n, time_type = "relative", covariate_formula)

# Running inference - only runs if posteriors not yet generated -----------
if(!file.exists("outputs/fits/delta_trunc_anti_n.rds")){

  # Compiling model
  mod <- cmdstan_model(
    "stan/antibody_kinetics_main.stan",
    include_paths = "stan",
    stanc_options = list("O1"),
    cpp_options = list(stan_threads = TRUE))

  # Fitting the model to the real-time dataset, i.e. truncated at chosen date
  fit_delta_trunc_anti_n <- mod$sample(
    data = stan_data_delta_trunc_anti_n,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Fitting the model to the full dataset
  fit_delta_full_anti_n <- mod$sample(
    data = stan_data_delta_full_anti_n,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    threads_per_chain = 4)

  # Saving fits
  fit_delta_trunc_anti_n$save_object("outputs/fits/delta_trunc_anti_n.rds")
  fit_delta_full_anti_n$save_object("outputs/fits/delta_full_anti_n.rds")
}

fit_delta_trunc_anti_n <- readRDS("outputs/fits/delta_trunc_anti_n.rds")
fit_delta_full_anti_n <- readRDS("outputs/fits/delta_full_anti_n.rds")

#---------------#
#--- Panel A ---#
#---------------#

# Processing fits
dt_delta_plot_trunc <- process_fits(
  fit_delta_trunc_anti_n, dt_delta_trunc_stan_trim_anti_n,
  stan_data_delta_trunc_anti_n,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_ba2_plot_trunc <- process_fits(
  fit_ba2_trunc, dt_ba2_trunc, stan_data_ba2_trunc,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_xbb_plot_trunc <- process_fits(
  fit_xbb_trunc, dt_xbb_trunc, stan_data_xbb_trunc,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_delta_plot_full <- process_fits(
  fit_delta_full_anti_n, dt_delta_full_stan_trim_anti_n,
  stan_data_delta_full_anti_n,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_ba2_plot_full <- process_fits(
  fit_ba2_full, dt_ba2_full, stan_data_ba2_full,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"))

dt_xbb_plot_full <- process_fits(
  fit_xbb_full, dt_xbb_full, stan_data_xbb_full,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
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
  `Infection history` != "Previously infected (Omicron)"][
    , `Infection history` := fct_relevel(fct_drop(
      `Infection history`),
      c("Infection naive", "Previously infected (Pre-Omicron)"))] |> unique()

dt_all_fits_full_main <- dt_all_fits_full[
  `Infection history` != "Previously infected (Omicron)"][
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

# Plotting panel A
p_figure_2_anti_n <- build_figure_2(
  dt_all_data_plot_main,
  dt_all_fits_trunc_main,
  dt_all_fits_full_main,
  facet_formula = facet_formula,
  dt_emergence = dt_emergence,
  dt_dominance = dt_dominance,
  dt_dates = dt_dates,
  t_max = 150,
  alpha_data_pre = 0.5,
  alpha_data_post = 0.25,
  alpha_fits_pre = 0.65,
  alpha_fits_post = 0.45,
  manual_facet_order = c("Delta wave", "BA.2 wave", "XBB wave"),
  plot_beyond = TRUE,
  plot_data = FALSE) +
  scale_colour_manual(values = manual_pal) +
  scale_fill_manual(values = manual_pal) +
  scale_linetype_manual("Type", values = c("Real-time" = "dashed", "Retrospective" = "solid")) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9),
        panel.grid = element_line(linewidth = 0.4)) +
  labs(tag = "A", title = "Population-level fits") +
  guides(colour = "none", fill = "none")

ggsave(
  "outputs/figures/supplementary_figures/figure_2_anti_n.png",
  p_figure_2_anti_n,
  width = 12,
  height = 8,
  bg = "white")


