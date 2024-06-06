library(Rcpp)
library(parallel)
library(cowplot)

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

#--- Delta fits
dt_ind_traj_sum_delta <- simulate_and_sum_ind(
  fit_delta_full, dt_delta_full_stan,
  n_draws = 500, wave_manual = "Delta",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, formula_delta)

dt_ind_traj_sum_delta_plot <- convert_log_scale_inverse(
  copy(dt_delta_full_stan), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = formula_delta,
    cleaned_names = c("Infection history", "Titre type"))

manual_pal_figure_3 <-
  c("#CC6677",
    "#DDCC77",
    "#88CCEE",
    "#882255",
    "#44AA99",
    "grey",
    "#D95F02",
    "#66A61E")

ids_plot_naive <- dt_ind_traj_sum_delta[
  `Infection history` == "Infection naive"][
    stan_id %in% sample(stan_id, 9), stan_id]

ids_plot_infec_pre <- dt_ind_traj_sum_delta[
  `Infection history` == "Previously infected (Pre-Omicron)"][
    stan_id %in% sample(stan_id, 9), stan_id]

p_ind_delta_naive <- dt_ind_traj_sum_delta[stan_id %in% ids_plot_naive] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_delta_full_stan_plot[stan_id %in% ids_plot_naive],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  # theme(legend.position = "bottom") +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_delta_pre_non <- dt_ind_traj_sum_delta[stan_id %in% ids_plot_infec_pre] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_delta_full_stan_plot[stan_id %in% ids_plot_infec_pre],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_pre_legend <- plot_grid(
  p_ind_delta_naive + theme(legend.position = "none"),
  p_ind_delta_pre_non + theme(legend.position = "none"))

legend_plot <- get_legend(
  p_ind_delta_naive +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin()))

p_ind_delta <- plot_grid(
  p_ind_pre_legend, legend_plot, nrow = 2, rel_heights = c(1, 0.1))

#--- BA.2 fits
dt_ind_traj_sum_ba2 <- simulate_and_sum_ind(
  fit_ba2_full, dt_ba2_full_stan,
  n_draws = 500, wave_manual = "BA.2",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, formula_ba2)

dt_ba2_full_stan_plot <- convert_log_scale_inverse(
  copy(dt_ba2_full_stan), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = formula_ba2,
    cleaned_names = c("Infection history", "Titre type"))

ids_ba2_plot_naive <- dt_ind_traj_sum_ba2[
  `Infection history` == "Infection naive"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

ids_ba2_plot_infec_pre_non <- dt_ind_traj_sum_ba2[
  `Infection history` == "Previously infected (Pre-Omicron)"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

ids_ba2_plot_infec_pre_omc <- dt_ind_traj_sum_ba2[
  `Infection history` == "Previously infected (Omicron)"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

p_ind_ba2_naive <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_naive] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_naive],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  # theme(legend.position = "bottom") +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_ba2_naive_pre_non <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_infec_pre_non] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_infec_pre_non],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_ba2_pre_omc <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_infec_pre_omc] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_infec_pre_omc],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ba2_ind_pre_legend <- plot_grid(
  p_ind_ba2_naive + theme(legend.position = "none"),
  p_ind_ba2_naive_pre_non + theme(legend.position = "none"),
  p_ind_ba2_pre_omc + theme(legend.position = "none"), nrow = 1)

legend_ba2_plot <- get_legend(
  p_ind_ba2_naive +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin()))

p_ind_ba2 <- plot_grid(
  p_ba2_ind_pre_legend, legend_ba2_plot, nrow = 2, rel_heights = c(1, 0.1))

#--- BA.2 fits
dt_ind_traj_sum_ba2 <- simulate_and_sum_ind(
  fit_ba2_full, dt_ba2_full_stan,
  n_draws = 500, wave_manual = "BA.2",
  scale = "log", adjust_dates = FALSE,
  time_shift = 0, t_max = 150, formula_ba2)

dt_ba2_full_stan_plot <- convert_log_scale_inverse(
  copy(dt_ba2_full_stan), vars_to_transform = "titre") |>
  clean_covariate_names(
    formula_val = formula_ba2,
    cleaned_names = c("Infection history", "Titre type"))

ids_ba2_plot_naive <- dt_ind_traj_sum_ba2[
  `Infection history` == "Infection naive"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

ids_ba2_plot_infec_pre_non <- dt_ind_traj_sum_ba2[
  `Infection history` == "Previously infected (Pre-Omicron)"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

ids_ba2_plot_infec_pre_omc <- dt_ind_traj_sum_ba2[
  `Infection history` == "Previously infected (Omicron)"][
    stan_id %in% sample(unique(stan_id), min(9, uniqueN(stan_id))),
    unique(stan_id)]

p_ind_ba2_naive <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_naive] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_naive],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  # theme(legend.position = "bottom") +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_ba2_naive_pre_non <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_infec_pre_non] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_infec_pre_non],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ind_ba2_pre_omc <- dt_ind_traj_sum_ba2[stan_id %in% ids_ba2_plot_infec_pre_omc] |>
  ggplot() +
  geom_line(aes(x = calendar_date, y = me, colour = `Titre type`), linetype = "dashed") +
  geom_ribbon(aes(x = calendar_date, ymin = lo, ymax = hi, fill = `Titre type`), alpha = 0.6) +
  geom_point(
    data = dt_ba2_full_stan_plot[stan_id %in% ids_ba2_plot_infec_pre_omc],
    aes(x = date, y = titre, colour = `Titre type`)) +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280",
               expression(" ">=2560))) +
  theme_minimal() +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_wrap(~stan_id, scales = "free_x")

p_ba2_ind_pre_legend <- plot_grid(
  p_ind_ba2_naive + theme(legend.position = "none"),
  p_ind_ba2_naive_pre_non + theme(legend.position = "none"),
  p_ind_ba2_pre_omc + theme(legend.position = "none"), nrow = 1)

legend_ba2_plot <- get_legend(
  p_ind_ba2_naive +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin()))

p_ind_ba2 <- plot_grid(
  p_ba2_ind_pre_legend, legend_ba2_plot, nrow = 2, rel_heights = c(1, 0.1))


#--- ALL INDIVIDUAL-LEVEL FITS (PROBABLY OVERKILL!)
dt_ind_fits_plot |>
  ggplot() +
  geom_line(
    aes(x = calendar_date, y = me,
        colour = `Titre type`,
        group = interaction(`Titre type`, stan_id)),
    linetype = "dashed") +
  geom_ribbon(
    aes(x = calendar_date,
        ymin = lo, ymax = hi,
        fill = `Titre type`,
        group = interaction(`Titre type`, stan_id)),
    alpha = 0.2) +
  geom_point(
    data = dt_ind_data_plot,
    aes(x = date, y = titre, colour = `Titre type`), size = 0.1) +
  geom_vline(data = dt_ind_data_plot[stan_id %in% ids_plot],
             aes(xintercept = relevant_last_exp_date),
             linetype = "twodash", colour = "gray30", alpha = 0.1) +
  geom_hline(yintercept = 40, linetype = "dashed", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "dashed", colour = "gray30") +
  scale_x_date(
    date_labels = "%Y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c("40", "80", "160", "320", "640", "1280", "2560")) +
  theme_minimal() +
  # theme(legend.position = "bottom") +
  # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = "Date",
       y = expression(paste("Titre value (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal_figure_3) +
  scale_fill_manual(values = manual_pal_figure_3) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1))) +
  facet_nested(Wave~.)

ggsave(
  "outputs/figures/supplementary_figures/ind_delta_fits.png",
  p_ind_delta,
  width = 10,
  height = 10,
  bg = "white")

