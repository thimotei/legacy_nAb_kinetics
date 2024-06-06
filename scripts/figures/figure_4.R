#--- Processing data
dt_pop_params_delta <- figure_4_data(
  fit_delta_full, dt_delta_full,
  stan_data_delta_full, covariate_formula,
  wave_manual = "Delta wave",
  cleaned_names = c("Infection history", "Titre type"))

dt_pop_params_ba2 <- figure_4_data(
  fit_ba2_full, dt_ba2_full,
  stan_data_ba2_full, covariate_formula,
  wave_manual = "BA.2 wave",
  cleaned_names = c("Infection history", "Titre type"))

dt_pop_params_xbb <- figure_4_data(
  fit_xbb_full, dt_xbb_full,
  stan_data_xbb_full, covariate_formula,
  wave_manual = "XBB wave",
  cleaned_names = c("Infection history", "Titre type"))

dt_figure_4_data <- rbind(
  dt_pop_params_delta,
  dt_pop_params_ba2,
  dt_pop_params_xbb)

dt_figure_4_data[, Wave := fct_relevel(Wave, "Delta wave")]

dt_figure_4_data_plot <- dt_figure_4_data[
  , rel_drop := mu_s/mu_p,
  by = .(`Infection history`, Wave, `Titre type`)][
    , `:=` (
      rel_drop_me = quantile(rel_drop, 0.5),
      mu_p_me = quantile(mu_p, 0.5),
      mu_s_me = quantile(mu_s, 0.5)),
    by = .(`Infection history`, Wave, `Titre type`)][
      , `Infection history` := fct_relevel(
        `Infection history`,
        "Infection naive",
        "Previously infected (Pre-Omicron)",
        "Previously infected (Omicron)")][
        , Wave := fct_relevel(Wave, "Delta wave")]

dt_figure_4_data_plot[
  , `Titre type` := fct_relevel(
    `Titre type`, c(
      "Ancestral", "Alpha", "Delta",
      "BA.2", "BA.5", "BQ.1.1", "XBB"))][
          `Titre type` == "XBB", `Titre type` := "XBB.1.5"][
            , `Titre type` := paste0(`Titre type`, " Abs")][
              , `Titre type` := fct_relevel(`Titre type`, c(
                "Ancestral Abs", "Alpha Abs", "Delta Abs",
                "BA.1 Abs", "BA.2 Abs",
                "BA.5 Abs", "BQ.1.1 Abs", "XBB.1.5 Abs"))]

# Wave = Titre type condition, for simpler plot
# [Wave == "Delta" & `Titre type` == "Delta" |
#  Wave == "BA.2" & `Titre type` == "BA.2" |
#  Wave == "XBB" & `Titre type` == "XBB"]

dt_figure_4_data_points <- dt_figure_4_data_plot[, .(
  Wave, `Infection history`, `Titre type`,
  rel_drop_me, mu_p_me, mu_s_me)][
  order(`Infection history`)] |>
  unique()

#--- Panel A
p_figure_4 <- dt_figure_4_data_plot |>
  ggplot(aes(
    x = mu_p, y = mu_s,
    colour = `Titre type`)) +
  geom_density_2d(
    aes(
      group = interaction(
        `Infection history`,
        `Titre type`))) +
  geom_point(data = dt_figure_4_data_plot[.draw <= 2000],
             alpha = 0.05, size = 0.2) +
  geom_point(data = dt_figure_4_data_points,
    aes(x = mu_p_me, y = mu_s_me,
        shape = `Infection history`),
    colour = "black") +
  geom_path(data = dt_figure_4_data_points,
    aes(x = mu_p_me, y = mu_s_me,
        group = `Titre type`),
    colour = "black") +
  # geom_vline(xintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_vline(xintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120, 10240),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560", "5120", "10240"),
    limits = c(NA, 10240)) +
  # geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120, 10240),
    labels = c(expression(" "<= 40),
               "80", "160", "320", "640", "1280", "2560", "5120", "10240"),
    limits = c(NA, 5120)) +
  facet_nested(~Wave, scales = "fixed") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  scale_shape_manual(values = c(1, 2, 3)) +
  labs(x = expression(paste("Population-level titre value at peak (IC"[50], ")")),
       y = expression(paste("Population-level titre value at set-point (IC"[50], ")"))) +
  scale_colour_manual(values = manual_pal) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 1)))

# ggsave("outputs/figures/figure_4.png",
#        p_figure_4,
#        width = 7,
#        height = 5,
#        bg = "white")
#
# ggsave("outputs/figures/figure_4.pdf",
#        p_figure_4,
#        width = 7,
#        height = 5)

# ggsave("outputs/figures/figure_4.pdf",
#        p_figure_4,
#        width = 8.5,
#        height = 5.5,
#        bg = "white")

#--- Panel B
process_params <- c(
  "t0_pop", "tp_pop", "ts_pop", "m1_pop", "m2_pop", "m3_pop", "mu_p", "mu_s")

dt_figure_4_data_long <- melt(
  dt_figure_4_data,
  measure.vars = process_params,
  variable.name = "parameter")

dt_figure_4_data_long[
  Wave == "Delta wave" & `Titre type` == "Delta Abs", Wave_type := "focal"]

dt_figure_4_data_long[
  Wave == "BA.2 wave" & `Titre type` == "BA.2 Abs", Wave_type := "focal"]

dt_figure_4_data_long[
  Wave == "XBB wave" & `Titre type` == "XBB.1.5 Abs", Wave_type := "focal"]

dt_figure_4_data_long_focal <- dt_figure_4_data_long[Wave_type == "focal"]

dt_figure_4_data_long_sum <- summarise_draws(
  dt_figure_4_data_long_focal,
  column_name = "value",
  by = c("Infection history",
         "Wave_type", "Titre type", "Wave",
         "parameter"))

dt_figure_4_1 <- dt_figure_4_data_long_focal[
  parameter %like% "m" & !parameter %like% "mu"][
    parameter == "m1_pop", parameter := "Gradient of boost"][
      parameter == "m2_pop", parameter := "Gradient of initial wane"][
        parameter == "m3_pop", parameter := "Gradient of set point dynamic"]

p_figure_4_1 <-  dt_figure_4_1 |>
  ggplot(aes(x = Wave, y = value*100, colour = `Titre type`)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = "twodash", colour = "gray30") +
  labs(y = "Titre value change per 100 days (log2 scale)") +
  facet_grid2(`Infection history` ~ parameter,
              scales = "free", independent = "all") +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'))

dt_figure_4_2_full <- dt_figure_4_data_long_focal[
  parameter %like% "mu"][
    parameter == "mu_p", parameter := "Titre value at peak"][
      parameter == "mu_s", parameter := "Titre value at set point"]

dt_figure_4_2_sum <- dt_figure_4_data_long_sum[
  parameter %like% "mu"][
    parameter == "mu_p", parameter := "Titre value at peak"][
      parameter == "mu_s", parameter := "Titre value at set point"]

# custom_cols <- manual_pal_figure()(9)

p_figure_4_2 <- dt_figure_4_2_sum |>
  ggplot(aes(x = Wave, y = me, ymin = lo, ymax = hi, colour = parameter)) +
  geom_pointrange(aes(shape = `Infection history`)) +
  geom_line(data = dt_figure_4_2_sum,
            aes(x = Wave, y = me,
                group = interaction(`Infection history`, Wave_type, parameter))) +
  # geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  # geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c(expression(""<=40),
               "80", "160", "320", "640", "1280",
               expression("">=2560))) +
  labs(title = "Peak titre values against emerging VOC",
       tag = "B",
       y = expression(paste("Titre value at peak (log"[2], ")"))) +
  # scale_shape_manual(values = c(1, 2, 3)) +
  # scale_colour_manual(values = c(manual_pal_figure[3], manual_pal_figure[5], manual_pal_figure[8])) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  facet_grid(~`Infection history`)

figure_4 <- cowplot::plot_grid(p_figure_4, p_figure_4_2, nrow = 2, rel_heights = c(1, 0.6))

ggsave("outputs/figures/figure_4.png",
       figure_4,
       width = 10,
       height = 8,
       bg = "white")

ggsave("outputs/figures/figure_4.svg",
       figure_4,
       width = 10,
       height = 8,
       bg = "white")
