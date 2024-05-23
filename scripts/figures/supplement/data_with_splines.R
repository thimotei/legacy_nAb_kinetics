#--- Custom variant order and colour scheme
strainOrder <- c("Ancestral", "D614G", "Alpha", "Beta", "Delta",
                 "BA.1",
                 "BA.2", "BA.5",
                 "BQ.1.1", "XBB", "XBB.1.5", "XBB.1.16", "BA.2.86")

manual_pal_figure_1 <-
  c("#332288", "#DDCC77",  "#88CCEE", "#882255", "#44AA99",
    "grey", "#D95F02", "#66A61E")

#--- summarising all infection events
common_theme <- theme(
  strip.text = element_blank(),
  plot.margin = unit(c(1,1,1,1), "lines")
)

####################################
### SPLINES OF TITRES BY VARIANT ###
####################################

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

dt_all_data_plot[, Wave := fct_relevel(Wave, "Delta wave")]
dt_all_data_plot[, `Infection history` := fct_relevel(
  `Infection history`, c("Infection naive", "Previously infected (Pre-Omicron)"))]

p_data_splines <- dt_all_data_plot |>
  ggplot(aes(x = date, y = titre,
             colour = `Titre type`,
             fill = `Titre type`)) +
  geom_point(size = 0.2, alpha = 0.5) +
  geom_smooth() +
  facet_nested(`Infection history` ~ Wave + `Titre type`, scales = "free") +
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30") +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_date(
    date_labels = "%b-%y") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560),
    labels = c("40", "80", "160", "320", "640", "1280", "2560"),
    limits = c(40, 5120)) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical",
        legend.margin = margin(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        legend.title = element_text(size = 6),
        legend.text  = element_text(size = 6)) +
  scale_colour_manual(values = manual_pal) +
  scale_fill_manual(values = manual_pal) +
  guides(colour = guide_legend(
    keywidth = 0.3, keyheight = 0.3),
    override.aes = list(size = 0.1, alpha = 0), nrow = 1) +
  labs(x = "Date", y = expression(paste("Titre value (IC"[50], ")")))

ggsave("outputs/figures/supplementary_figures/figure_data_splines.png",
       p_data_splines,
       height = 7,
       width = 12,
       bg = "white")

