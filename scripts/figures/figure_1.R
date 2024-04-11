#--- Custom variant order and colour scheme

strainOrder <- c("Ancestral", "D614G", "Alpha", "Beta", "Delta",
                 "BA.1",
                 "BA.2", "BA.5",
                 "BQ.1.1", "XBB", "XBB.1.5", "XBB.1.16", "BA.2.86")

# strainOrder <- c(
#   "D614G", "Alpha", "Delta", "BA.1", "BA.2",
#   "BA.5", "BQ.1.1", "XBB", "BA.2.86")

manual_pal_figure_1 <-
  c("#332288", "#DDCC77",  "#88CCEE", "#882255", "#44AA99", 
    "grey", "#D95F02", "#66A61E")

#--- summarising all infection events
# making smaller version of the dataset, for ease and speed
dt_inf <- dt_clean[
  , .(id, inf_num, last_inf_type, last_inf_date)] |> 
  unique()

# dropping BA.1/BA.2 as a group, due to very small numbers
# dt_inf <- dt_inf[last_inf_type != "BA.1/BA.2"]

common_theme <- theme(
  strip.text = element_blank(),
  plot.margin = unit(c(1,1,1,1), "lines")
)

min_date <- dt_inf[, min(last_inf_date, na.rm = TRUE)]
max_date <- dt_inf[, max(last_inf_date, na.rm = TRUE)]

p1 <- ggplot(dt_inf) +
  geom_density(
    aes(x = last_inf_date, fill = reorder(last_inf_type, last_inf_date)),
    position = "fill") +
  geom_vline(aes(xintercept = date_delta), linetype = "dotdash") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dotdash") + 
  geom_vline(aes(xintercept = date_xbb), linetype = "dotdash") + 
  # facet_rep_grid(rows = vars(type), scales = "free", labeller = my_labeller) + 
  theme_classic() +
  theme(text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical", 
        legend.margin = margin(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  scale_fill_manual(values = manual_pal_figure_1) +
  labs(x = "", 
       y = "Proportion", 
       fill = "Variant",
       tag = "A",
       title = "Infections") +
  lims(x = c(min_date, max_date))

p2 <- ggplot(dt_inf) +
  geom_histogram(
    aes(x = last_inf_date, fill = reorder(last_inf_type, last_inf_date))) +
  geom_vline(aes(xintercept = date_delta), linetype = "dotdash") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dotdash") + 
  geom_vline(aes(xintercept = date_xbb), linetype = "dotdash") + 
  theme(legend.position = "right") +
  guides(fill = guide_legend(
    keywidth = 0.3, keyheight = 0.3, col = 1)) + 
  theme_classic() +
  theme(text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical", 
        legend.margin = margin(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  scale_fill_manual(values = manual_pal_figure_1) +
  labs(x = "Date", y = "Number", tag = "C", fill = "Variant") +
  theme(strip.text = element_blank()) +
  lims(x = c(min_date, max_date))

#--- summarising all vaccination events
# making smaller version of the dataset, for ease and speed
dt_vax <- dt_clean[
  !is.na(last_vax_type)][
  last_vax_type != "others"][
  , .(id, vax_num, last_vax_type, last_vax_date)][
  last_vax_date < ymd("2023-10-22")] |> 
  unique()

# removing unused factors after renaming
dt_vax[, last_vax_type := fct_drop(last_vax_type)]
dt_vax[, last_vax_type := factor(last_vax_type)]

# Aggregate data by last_vax_date and last_vax_type
dt_vax_sum <- dt_vax[
  !is.na(last_vax_type)
  , .N, by = .(last_vax_date, last_vax_type)][
    order(last_vax_date)][
      , cumulative_count := cumsum(N), by = last_vax_type]

# Create a sequence of dates from the start to the end
all_dates <- data.table(
  date = seq(
    min_date, 
    max_date, by = "day"))

# Merge and get cumulative count for each day
dt_vax_timeseries <- merge(all_dates, dt_vax_sum, by.x = "date", by.y = "last_vax_date", all.x = TRUE)
dt_vax_timeseries[, cumulative_count := zoo::na.locf(cumulative_count, na.rm = FALSE)]
dt_vax_timeseries[is.na(cumulative_count), cumulative_count := 0]
# dt_vax_timeseries[, Wave := zoo::na.locf(Wave, na.rm = FALSE)]
dt_vax_timeseries[, last_vax_type := zoo::na.locf(last_vax_type, na.rm = FALSE)]

p3 <- dt_vax_timeseries[!is.na(last_vax_type)] |> 
  ggplot() + 
  geom_line(aes(x = date,
                y = cumulative_count,
                colour = last_vax_type)) + 
  geom_vline(aes(xintercept = date_delta), linetype = "dotdash") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dotdash") + 
  geom_vline(aes(xintercept = date_xbb), linetype = "dotdash") + 
  theme_classic() +
  theme(text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical", 
        legend.margin = margin(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  labs(x = "",
       y = "Cumulative total", 
       title = "Vaccinations", 
       tag = "B") +
  lims(x = c(min_date, max_date)) + 
  scale_colour_brewer(palette = "Set1")

p4 <- dt_vax |>
  ggplot() +
  geom_histogram(aes(x = last_vax_date,
                     fill = reorder(last_vax_type, last_vax_date))) +
  geom_vline(aes(xintercept = date_delta), linetype = "dotdash") +
  geom_vline(aes(xintercept = date_ba2), linetype = "dotdash") +
  geom_vline(aes(xintercept = date_xbb), linetype = "dotdash") +
  labs(x = "Date",
       y = "Number",
       tag = "D",
       fill = "Vaccine type") +
  theme(legend.position = "right") +
  guides(fill = guide_legend(
    keywidth = 0.3, keyheight = 0.3, col = 1)) + 
  theme_classic() +
  theme(text = element_text(size = 9, family = "Helvetica"),
        strip.placement = "outside",
        plot.title = element_text(face = "plain", size = 9),
        legend.box = "vertical", 
        legend.margin = margin(),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black')) +
  lims(x = c(min_date, max_date)) + 
  scale_fill_brewer(palette = "Set1") 

#--- putting all the plots together
panel_a_legend <- get_legend(p2)

panel_b_legend <- get_legend(p4)

panel_a_no_leg <- plot_grid(
  p1 + theme(legend.position = "none"),
  p2 + theme(legend.position = "none"),
  ncol = 1, align = "v")

panel_b_no_leg <- plot_grid(
  p3 + theme(legend.position = "none"),
  p4 + theme(legend.position = "none"),
  ncol = 1, align = "v")

panel_a <- plot_grid(
  panel_a_no_leg,
  panel_a_legend,
  rel_widths = c(1, 0.25)
)

panel_b <- plot_grid(
  panel_b_no_leg,
  panel_b_legend,
  rel_widths = c(1, 0.25)
)

p_figure_1_panel_a <- figure_1 <- plot_grid(panel_a, panel_b)

###############################
## PANEL B - MODEL SCHEMATIC ##
###############################

image_path <- "outputs/figures/model_schematic.png" # Change to your image path
img <- magick::image_read(image_path)

# Convert the image to a ggplot
p_figure_1_panel_b <- ggplot() + 
  annotation_custom(
    grob = grid::rasterGrob(img)) +
  theme_void() + 
  theme(
    plot.margin = unit(c(0,0,0,0), "cm"))

############################################
## PANEL C - SPLINES OF TITRES BY VARIANT ##
############################################
dt_all_data_plot <- rbind(
  dt_delta_data_full[
    , Wave := "Delta wave"][
      last_exp_type %in% c("BNT162b2", "AZD1222")],
  dt_ba2_data_full[, Wave := "BA.2 wave"],
  dt_xbb_data_full[, Wave := "XBB wave"])[
    , Wave := factor(Wave)] |> 
  clean_covariate_names(
    formula_val = formula_delta, 
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

manual_pal_figure <-
  c("#CC6677",
    "#DDCC77",
    "#88CCEE",
    "#882255",
    "#44AA99",
    "grey",
    "#D95F02",
    "#66A61E")

p_figure_1_panel_c <- dt_all_data_plot |> 
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
    limits = c(40, 2560)) +
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
  scale_colour_manual(values = manual_pal_figure) +
  scale_fill_manual(values = manual_pal_figure) +
  guides(colour = guide_legend(
    keywidth = 0.3, keyheight = 0.3),
    override.aes = list(size = 0.1, alpha = 0), nrow = 1) + 
  labs(x = "Date", y = expression(paste("Titre value (IC"[50], ")")))

#################
## FULL FIGURE ##
#################

p_figure_1_top <- plot_grid(
  p_figure_1_panel_a,
  p_figure_1_panel_b + labs(tag = "E"),
  rel_widths = c(1, 1))

p_figure_1_all <- plot_grid(
  p_figure_1_top,
  p_figure_1_panel_c + labs(tag = "F"),
  ncol = 1,
  rel_heights = c(1, 1))

ggsave("outputs/figures/figure_1.png",
       p_figure_1_all,
       height = 10,
       width = 16,
       bg = "white")

ggsave("outputs/figures/figure_1.pdf",
       p_figure_1_all,
       height = 10,
       width = 16,
       bg = "white")


