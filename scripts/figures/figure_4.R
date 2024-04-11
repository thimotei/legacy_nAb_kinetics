source("scripts/inference/delta.R")
source("scripts/inference/ba2.R")
source("scripts/inference/xbb.R")

#--------------------#
#--- Loading data ---#
#--------------------#

dt_figure_4_data_full <- fread("outputs/data/figure_4_data.RDS")

dt_figure_4_data_full[Wave == "Delta", Wave := "Delta Wave"]
dt_figure_4_data_full[Wave == "BA.2", Wave := "BA.2 Wave"]
dt_figure_4_data_full[Wave == "XBB", Wave := "XBB Wave"]

#---------------#
#--- Panel A ---#
#---------------#

dt_figure_4_data <- rbind(
  dt_figure_4_data_full[calendar_date == date_delta & Wave == "Delta Wave"],
  dt_figure_4_data_full[calendar_date == date_ba2 & Wave == "BA.2 Wave"],
  dt_figure_4_data_full[calendar_date == date_xbb & Wave == "XBB Wave"])[
    , `Titre type` := fct_relevel(
      `Titre type`, c(
        "Ancestral", "Alpha", "Delta", 
        "BA.2", "BA.5", "BQ.1.1", "XBB"))][
    , Wave := factor(Wave)][, Wave := fct_relevel(Wave, "Delta Wave")][
      `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
        , `Titre type` := paste0(`Titre type`, " Abs")][
          , `Titre type` := fct_relevel(`Titre type`, c(
            "Ancestral Abs", "Alpha Abs", "Delta Abs",
            "BA.1 Abs", "BA.2 Abs",
            "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

# Putting data together
dt_all_data_plot <- rbind(
  dt_delta_data_full[, Wave := "Delta Wave"],
  dt_ba2_data_full[, Wave := "BA.2 Wave"],
  dt_xbb_data_full[, Wave := "XBB Wave"]) |> 
  clean_covariate_names(
    formula_val = formula_delta, 
    cleaned_names = c("Infection history", "Titre type")) |> 
  convert_log_scale_inverse(vars_to_transform = "titre")

dt_all_data_plot[
  `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
    , `Titre type` := paste0(`Titre type`, " Abs")][
      , `Titre type` := fct_relevel(`Titre type`, c(
        "Ancestral Abs", "Alpha Abs", "Delta Abs",
        "BA.1 Abs", "BA.2 Abs",
        "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

min_date_ba2 <- dt_all_data_plot[
  Wave == "BA.2 Wave"
  , .(date = min(date)), by = Wave][, date]

min_date_xbb <- dt_all_data_plot[
  Wave == "XBB Wave"
  , .(date = min(date)), by = Wave][, date]

max_date_xbb <- dt_all_data_plot[
  Wave == "XBB Wave"
  , .(date = max(date)), by = Wave][, date]

dt_dates <- data.table(
  Wave = c("Delta Wave", "BA.2 Wave", "XBB Wave"),
  date_wave_next = c(min_date_ba2, min_date_xbb, max_date_xbb))

min_date_overall <- dt_all_data_plot[, min(pmin(last_exp_date, date))]
max_date_overall <- dt_all_data_plot[, max(pmax(last_exp_date, date))]

custom_cols <-
  c("#CC6677",
    "#DDCC77",
    "#88CCEE",
    "#882255",
    "#44AA99",
    "grey",
    "#D95F02",
    "#66A61E")


# Define your range of time_shift values
time_shift_values <- seq(-75, 75, by = 15)

figure_4_panel_a <- plot_figure_4(
  dt_all_data_plot,
  dt_dates) + 
  labs(title = "Population-level titre values",
       tag = "A",
       x = "Date",
       y = expression(paste("Titre (IC"[50], ")"))) +
  scale_colour_manual(
    values = custom_cols) +
  scale_fill_manual(
    values = custom_cols) +
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

figure_4_panel_b <- dt_all_data_plot |> 
  ggplot() + 
  geom_density(aes(x = last_exp_date, fill = fct_relevel(Wave, "Delta Wave"))) + 
  geom_vline(aes(xintercept = date_delta), linetype = "dashed") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dashed") +
  geom_vline(aes(xintercept = date_xbb), linetype = "dashed") +
  # facet_grid(~Wave, scales = "free") + 
  scale_x_date(
    date_labels = "%b %Y",
    limits = c(min_date_overall, max_date_overall)) +
  scale_colour_manual(
    values = c(custom_cols[3], custom_cols[5], custom_cols[8])) + 
  scale_fill_manual(
    values = c(custom_cols[3], custom_cols[5], custom_cols[8])) + 
  labs(title = "Observed vaccine timings",
       tag = "B",
       x = "Date",
       y = "Density") +
  theme_linedraw() +
  theme(legend.position = "none", 
        text = element_text(size = 9, family = "Helvetica"),
        strip.background = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9)) 

#---------------#
#--- Panel C ---#
#---------------#

figure_4_panel_c <- ggplot(
  data = dt_figure_4_data) + 
  geom_line(aes(
    x = time_shift, y = me, colour = `Titre type`)) +
  geom_pointrange(aes(
    x = time_shift, y = me, ymin = lo,
    ymax = hi, colour = `Titre type`)) + 
  geom_hline(
    data = data.table(y = 2560),
    aes(yintercept = y),
    linetype = "dotdash", colour = "gray30", alpha = 0.5) +
  scale_x_continuous(
    breaks = time_shift_values) + 
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120),
    labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120"),
    limits = c(NA, 5120)) + 
  facet_nested(~Wave) +
  scale_colour_manual(
    values = custom_cols) +
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

figure_4 <- cowplot::plot_grid(
  figure_4_panel_a,
  figure_4_panel_b, 
  figure_4_panel_c,
  nrow = 3, rel_heights = c(1, 0.5, 0.75), align = "v", axis = "l")

ggsave("outputs/figures/figure_4.png",
       figure_4,
       width = 8,
       height = 10,
       bg = "white")

ggsave("outputs/figures/figure_4.pdf",
       figure_4,
       width = 8,
       height = 10,
       bg = "white")


