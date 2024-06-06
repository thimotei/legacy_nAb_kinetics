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

p_figure_1_panel_a/p_figure_1_panel_b

#################
## FULL FIGURE ##
#################

p_figure_1 <- plot_grid(
  p_figure_1_panel_a,
  p_figure_1_panel_b + labs(tag = "E"),
  ncol = 1,
  rel_widths = c(1, 1))

ggsave("outputs/figures/figure_1.png",
       p_figure_1,
       height = 10,
       width = 10,
       bg = "white")

ggsave("outputs/figures/figure_1.svg",
       p_figure_1,
       height = 10,
       width = 10,
       bg = "white")


