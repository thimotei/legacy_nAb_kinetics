#--------------------#
#--- Loading data ---#
#--------------------#

dt_figure_5_data_full <- fread("outputs/data/figure_5_data.rds") |>
  relevel_factors_for_plots(wave = T, infection_history = F, titre_type = T)

#---------------#
#--- Panel A ---#
#---------------#

dt_figure_5_data <- rbind(
  dt_figure_5_data_full[calendar_date == date_delta & Wave == "Delta wave"],
  dt_figure_5_data_full[calendar_date == date_ba2 & Wave == "BA.2 wave"],
  dt_figure_5_data_full[calendar_date == date_xbb & Wave == "XBB wave"])

# Putting data together
dt_all_data_plot <- rbind(
  dt_delta_full[, Wave := "Delta"],
  dt_ba2_full[, Wave := "BA.2"],
  dt_xbb_full[, Wave := "XBB"]) |>
  clean_covariate_names(
    formula_val = covariate_formula,
    cleaned_names = c("Infection history", "Titre type")) |>
  convert_log_scale_inverse(vars_to_transform = "titre") |>
  relevel_factors_for_plots()

min_date_ba2 <- dt_all_data_plot[
  Wave == "BA.2 wave"
  , .(date = min(date)), by = Wave][, date]

min_date_xbb <- dt_all_data_plot[
  Wave == "XBB wave"
  , .(date = min(date)), by = Wave][, date]

max_date_xbb <- dt_all_data_plot[
  Wave == "XBB wave"
  , .(date = max(date)), by = Wave][, date]

dt_dates <- data.table(
  Wave = c("Delta wave", "BA.2 wave", "XBB wave"),
  date_wave_next = c(min_date_ba2, min_date_xbb, max_date_xbb))

# min_date_overall <- dt_all_data_plot[, min(pmin(last_exp_date, date))]
# max_date_overall <- dt_all_data_plot[, max(pmax(last_exp_date, date))]

min_date_overall <- dt_all_data_plot[, min(date)]
max_date_overall <- dt_all_data_plot[, max(date)]

# Define your range of time_shift values
time_shift_values <- seq(-75, 75, by = 15)

dt_model_plot <- dt_figure_5_data_full[time_shift == 0]

dt_model_plot <- merge(
  dt_model_plot, dt_all_data_plot[, .(
    min_date = min(date), max_date = max(date)), by = Wave])[
    , .SD[calendar_date >= min_date & calendar_date <= max_date], by = Wave]

figure_5_panel_a <- plot_figure_5(
  dt_data = dt_all_data_plot,
  dt_model = dt_model_plot,
  dt_dates = dt_dates,
  alpha_splines = 1, alpha_model = 0.2,
  alpha_model_smooth = 0.5, span_model = 0.2,
  plot_data = FALSE, plot_model_splines = TRUE) +
  labs(title = "Population-level titre values",
       tag = "A",
       x = "Date",
       y = expression(paste("Titre (IC"[50], ")"))) +
  scale_colour_manual(
    values = manual_pal) +
  scale_fill_manual(
    values = manual_pal) +
  scale_x_date(
    date_labels = "%b %Y",
    limits = c(min_date_overall, max_date_overall)) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 9, family = "Helvetica"),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9),
        legend.box = "vertical",
        legend.margin = margin()) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1, size = 1)))

#---------------#
#--- Panel B ---#
#---------------#

dt_data_panel_b_plot <- dt_all_data_plot[
  dt_dates, on = "Wave"]

figure_5_panel_b <- dt_all_data_plot |>
  ggplot() +
  geom_histogram(
    aes(x = last_exp_date,
        fill = fct_relevel(Wave, "Delta wave")),
    binwidth = 7) +
  geom_vline(aes(xintercept = date_delta), linetype = "dashed") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dashed") +
  geom_vline(aes(xintercept = date_xbb), linetype = "dashed") +
  # facet_grid(~Wave, scales = "free") +
  scale_x_date(
    date_labels = "%b %Y",
    limits = c(min_date_overall, max_date_overall)) +
  scale_colour_manual(
    values = c(manual_pal[3], manual_pal[5], manual_pal[8])) +
  scale_fill_manual(
    values = c(manual_pal[3], manual_pal[5], manual_pal[8])) +
  labs(title = "Weekly vaccines administered",
       tag = "B",
       x = "Date",
       y = "Number") +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 9, family = "Helvetica"),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9))

#---------------#
#--- Panel C ---#
#---------------#

figure_5_panel_c <- ggplot(
  data = dt_figure_5_data) +
  geom_line(aes(
    x = time_shift, y = me, colour = `Titre type`)) +
  geom_pointrange(aes(
    x = time_shift, y = me, ymin = lo,
    ymax = hi, colour = `Titre type`)) +
  # geom_hline(
  #   data = data.table(y = 2560),
  #   aes(yintercept = y),
  #   linetype = "dotdash", colour = "gray30", alpha = 0.5) +
  scale_x_continuous(
    breaks = time_shift_values) +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120")) +
    # limits = c(NA, 5120)) +
  facet_nested(~Wave, scales = "free", independent = "all") +
  scale_colour_manual(
    values = manual_pal) +
  labs(title = "Population titre values varying vaccine timings",
       x = "Time shift (days)",
       y = expression(paste("Titre (IC"[50], ")"))) +
  theme_linedraw() +
  guides(colour = guide_legend(nrow = 1, override.aes = list(alpha = 1, size = 1))) +
  theme(legend.position = "bottom",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9)) +
  labs(tag = "C",
       title = "Counterfactual population-level titres varying vaccination timings")

#-------------------#
#--- Full figure ---#
#-------------------#

figure_5 <- cowplot::plot_grid(
  figure_5_panel_a,
  figure_5_panel_b,
  figure_5_panel_c,
  nrow = 3, rel_heights = c(1, 0.5, 0.75), align = "v", axis = "l")

ggsave("outputs/figures/figure_5.png",
       figure_5,
       width = 13,
       height = 8.5,
       bg = "white")

ggsave("outputs/figures/figure_5.svg",
       figure_5,
       width = 16,
       height = 10,
       bg = "white")

#-----------------------------------------------------------#
#--- Supplementary verison of Panel A with LOESS splines ---#
#-----------------------------------------------------------#

figure_5_panel_a_supplement <- plot_figure_5(
  dt_data = dt_all_data_plot,
  dt_model = dt_model_plot,
  dt_dates = dt_dates,
  alpha_splines = 1, alpha_model = 0.2,
  alpha_model_smooth = 0.5, span_model = 0.2,
  plot_data = TRUE,
  plot_model = FALSE,
  plot_raw_data_splines = TRUE,
  plot_model_splines = FALSE) +
  labs(x = "Date",
       y = expression(paste("Titre (IC"[50], ")"))) +
  scale_colour_manual(
    values = manual_pal) +
  scale_fill_manual(
    values = manual_pal) +
  scale_x_date(
    date_labels = "%b %Y",
    limits = c(min_date_overall, max_date_overall)) +
  theme_linedraw() +
  theme(legend.position = "none",
        text = element_text(size = 9, family = "Helvetica"),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9),
        legend.box = "vertical",
        legend.margin = margin()) +
  guides(colour = guide_legend(
    nrow = 1,
    override.aes = list(alpha = 1, size = 1)))

ggsave(
  "outputs/figures/supplementary_figures/figure_5_no_model_splines.png",
  figure_5_panel_a_supplement,
  width = 6,
  height = 3.5,
  bg = "white")

