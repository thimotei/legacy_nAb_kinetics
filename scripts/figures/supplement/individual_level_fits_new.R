# Choosing 4 individuals with exposures in all three waves
id_wave_count <- dt_ind_fits_plot[, .(NumWaves = uniqueN(Wave)), by = id]
ids_plot <- id_wave_count[NumWaves == 3, .(id = id)][, id]

dt_ind_fits_plot_subset <- dt_ind_fits_plot[
  id %in% ids_plot][
    , plot_id := .GRP, by = id][
      , plot_id := factor(paste0("Individual ", plot_id))]

dt_ind_data_plot_subset <- dt_ind_data_plot[
  id %in% ids_plot][
    , plot_id := .GRP, by = id][
      , plot_id := factor(paste0("Individual ", plot_id))]

ids <- dt_ind_fits_plot[stan_id %in% sample(unique(stan_id), 10), unique(stan_id)]

dt_ind_fits_plot <- relevel_factors_for_plots(dt_ind_fits_plot)

# Plotting panel B
p_ind <- dt_ind_fits_plot[Wave == "XBB wave"] |>
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
    data = dt_ind_data_plot[Wave == "XBB wave"],
    aes(x = date, y = titre, colour = `Titre type`), size = 1.5) +
  geom_vline(data = dt_ind_data_plot[Wave == "XBB wave"],
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
  # theme_cowplot(font_size = 7, font_family = "Helvetica", line_size = 0.25) +
  theme_linedraw() +
  theme(legend.position = "right",
        text = element_text(size = 8, family = "Helvetica"),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        strip.placement = "outside",
        plot.title = element_text(face = "bold", size = 9)) +
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
  facet_wrap(Wave ~ stan_id, scales = "free", ncol = 12)

ggsave("outputs/figures/figure_ind_xbb.png",
       p_ind,
       width = 20,
       height = 38.5,
       limitsize = FALSE,
       bg = "white")


