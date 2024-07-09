#---------------#
#--- Panel A ---#
#---------------#

# Processing fits
dt_delta_plot_trunc <- process_fits(
  fit_delta_trunc, dt_delta_trunc, stan_data_delta_trunc,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

dt_ba2_plot_trunc <- process_fits(
  fit_ba2_trunc, dt_ba2_trunc, stan_data_ba2_trunc,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

dt_xbb_plot_trunc <- process_fits(
  fit_xbb_trunc, dt_xbb_trunc, stan_data_xbb_trunc,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

dt_delta_plot_full <- process_fits(
  fit_delta_full, dt_delta_full, stan_data_delta_full,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

dt_ba2_plot_full <- process_fits(
  fit_ba2_full, dt_ba2_full, stan_data_ba2_full,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

dt_xbb_plot_full <- process_fits(
  fit_xbb_full, dt_xbb_full, stan_data_xbb_full,
  covariate_formula, t_max = 150, summarise = TRUE, scale = "natural",
  cleaned_names = c("Infection history", "Titre type"), n_draws = 10)

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

dt_all_data_plot[
  Wave == "Delta wave",
  mean(abs(as.numeric(as.Date(date) - date_delta, units = "days")))]

dt_all_data_plot[
  Wave == "BA.2 wave",
  mean(abs(as.numeric(as.Date(date) - date_ba2, units = "days")))]

dt_all_data_plot[
  Wave == "XBB wave",
  mean(abs(as.numeric(as.Date(date) - date_xbb, units = "days")))]

dt_emergence <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  t = c(60, 76, 49))

dt_dominance <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  t = c(150, 150, 150))

dt_dates <- data.table(
  Wave = factor(dt_all_data_plot[, unique(Wave)]),
  date_emergence = c(date_delta, date_ba2, date_xbb))

facet_formula <- `Infection history` ~ Wave + `Titre type`

# Plotting panel A
p_figure_2_a <- build_figure_2(
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
  scale_linetype_manual("Type", values = c("Retrospective" = "solid", "Real-time" = "dashed")) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9),
        panel.grid = element_line(linewidth = 0.4),
        axis.text = element_text(size = 8)) +
  labs(tag = "A", title = "Population-level fits") +
  guides(colour = "none", fill = "none")

#---------------#
#--- Panel B ---#
#---------------#

# Delta fits
dt_ind_traj_sum_delta <- simulate_and_sum_ind(
  fit_delta_full, dt_delta_full,
  n_draws = 10, wave_manual = "Delta",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, covariate_formula)

dt_delta_full_stan_plot <- convert_log_scale_inverse(
  copy(dt_delta_full), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = covariate_formula,
    cleaned_names = c("Infection history", "Titre type"))

# BA.2 fits
dt_ind_traj_sum_ba2 <- simulate_and_sum_ind(
  fit_ba2_full, dt_ba2_full,
  n_draws = 10, wave_manual = "BA.2",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, covariate_formula)

dt_ba2_full_stan_plot <- convert_log_scale_inverse(
  copy(dt_ba2_full), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = covariate_formula,
    cleaned_names = c("Infection history", "Titre type"))

# XBB fits
dt_ind_traj_sum_xbb <- simulate_and_sum_ind(
  fit_xbb_full, dt_xbb_full,
  n_draws = 10, wave_manual = "XBB",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, covariate_formula)

dt_xbb_full_stan_plot <- convert_log_scale_inverse(
  copy(dt_xbb_full), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = covariate_formula,
    cleaned_names = c("Infection history", "Titre type"))

dt_ind_fits_plot <- rbind(
  dt_ind_traj_sum_delta,
  dt_ind_traj_sum_ba2,
  dt_ind_traj_sum_xbb) |>
  relevel_factors_for_plots()

dt_ind_data_plot <- rbind(
  dt_delta_full_stan_plot[, Wave := "Delta"],
  dt_ba2_full_stan_plot[, Wave := "BA.2"],
  dt_xbb_full_stan_plot[, Wave := "XBB"]) |>
  relevel_factors_for_plots()

#--- Need the individual IDs to build panel B of this figure, which are not
#--- publicly available. We provide the code used to build this panel. Replace
#--- "id", with "stan_id" which is publicly available for a similar figure.
#--- We also provide all of the individual-level fits in the supplementary
#--- material.
# Choosing 3 individuals with exposures in all three waves
# id_wave_count <- dt_ind_fits_plot[, .(NumWaves = uniqueN(Wave)), by = id]

# Hard-coded IDs used in Figure 2. Chosen as they have nice coverage over
# time period. All individual-level trajectories are in the supplementary
# material. These IDs have been removed from the public dataset for anonymity.
# Only exposure-specific IDs are publicly available
# ids_plot <- c(369, 588, 809)

# This extracts the exposure ID (stan_id) and Wave corresponding to the hard-coded
# individual IDs above, to extract the exposures of interest
# dt_id_and_wave_lookup <- dt_ind_data_plot[
#   id %in% ids_plot][, .(id, stan_id, Wave)][
#   , plot_id := .GRP, by = "id"][, id := NULL] |> unique()

# fwrite(dt_id_and_wave_lookup, "data/figure_2_ind_exp_ids.rds")

dt_id_and_wave_lookup <- fread("data/figure_2_ind_exp_ids.rds")

# Choosing 3 random exposure-specific IDs with an exposure in each of the three waves
# ids_plot <- id_wave_count[
#   NumWaves == 3, .(id = stan_id)][stan_id %in% sample(stan_id, 3), id]

dt_next_wave_date <- data.table(
  Wave = c("Delta wave", "BA.2 wave", "XBB wave"),
  current_wave_date = c(date_delta, date_ba2, date_xbb),
  next_wave_date = c(date_ba2, date_xbb, date_xbb))

dt_ind_fits_plot <- merge(dt_ind_fits_plot, dt_next_wave_date, by = "Wave")
dt_ind_data_plot <- merge(dt_ind_data_plot, dt_next_wave_date, by = "Wave")

dt_ind_fits_plot_subset <- merge(
  dt_ind_fits_plot, dt_id_and_wave_lookup, by = c("stan_id", "Wave"))

dt_ind_data_plot_subset <- merge(
  dt_ind_data_plot, dt_id_and_wave_lookup, by = c("stan_id", "Wave"))

dt_ind_fits_plot_subset[, plot_id := factor(paste0("Individual ", plot_id))]
dt_ind_data_plot_subset[, plot_id := factor(paste0("Individual ", plot_id))]

# Plotting panel B
p_ind <- dt_ind_fits_plot_subset |>
  ggplot() +
  geom_line(
    aes(x = calendar_date, y = me,
        colour = `Titre type`,
        group = interaction(`Titre type`, Wave)),
    linetype = "solid") +
  geom_ribbon(
    aes(x = calendar_date,
        ymin = lo, ymax = hi,
        fill = `Titre type`,
        group = interaction(`Titre type`, Wave)),
    alpha = 0.5) +
  geom_point(
    data = dt_ind_data_plot_subset,
    aes(x = date, y = titre, colour = `Titre type`), size = 1.5) +
  geom_vline(data = dt_ind_data_plot_subset,
             aes(xintercept = relevant_last_exp_date),
             linetype = "dashed", colour = "gray30", alpha = 0.5) +
  geom_hline(yintercept = 40, linetype = "dashed", colour = "gray30", alpha = 0.2) +
  geom_hline(yintercept = 2560, linetype = "dashed", colour = "gray30", alpha = 0.2) +
  scale_x_date(
    date_labels = "%b-%y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 160, 640, 2560),
    labels = c("40", "160", "640", "2560")) +
  theme_linedraw() +
  theme(legend.position = "right",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9),
        panel.grid = element_line(linewidth = 0.4)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")")),
       tag = "B",
       title = "Individual-level fits") +
  scale_colour_manual(values = manual_pal) +
  scale_fill_manual(values = manual_pal) +
  guides(colour = guide_legend(
    override.aes = list(alpha = 1, size = 1))) +
  facet_grid(plot_id ~ Wave, scales = "free")

p_figure_2 <- plot_grid(
  p_figure_2_a,
  p_ind,
  rel_heights = c(1, 0.5),
  ncol = 1)

ggsave("outputs/figures/figure_2.svg",
       p_figure_2,
       width = 12,
       height = 10)

ggsave("outputs/figures/figure_2.png",
       p_figure_2,
       width = 12,
       height = 10,
       bg = "white")

ggsave("outputs/figures/figure_2.pdf",
       p_figure_2,
       width = 12,
       height = 10,
       bg = "white")

