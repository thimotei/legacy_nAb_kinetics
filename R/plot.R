build_figure_2 <- function(
    dt_data, dt_fits_pre, dt_fits_post, facet_formula, 
    dt_emergence, dt_dominance, dt_dates, t_max = 120,
    alpha_data_pre, alpha_data_post, alpha_fits_pre, alpha_fits_post, 
    titre_min = 40, titre_max = 2560, manual_facet_order, 
    plot_beyond = FALSE, plot_data = FALSE) {
  
  # hack to get horizontal lines to plot on log2 scale
  # for some reason, all standard approaches were throwing 
  # an errror
  dt_h1_line <- data.table(
    x = seq(0, t_max, 0.1),
    y = seq(titre_min, titre_min, 0.1)
  )
  
  dt_h2_line <- data.table(
    x = seq(0, t_max, 0.1),
    y = seq(titre_max, titre_max, 0.1)
  )
  
  dt_emergence_lines <- generate_vertical_lines_data(
    dt_data, dt_emergence, facet_formula,
    titre_min, titre_max)
  
  dt_dominance_lines <- generate_vertical_lines_data(
    dt_data, dt_dominance, facet_formula,
    titre_min, titre_max)
  
  dt_emergence_join <- dt_emergence_lines[
    , .(Wave, t_emergence = t)] |> unique()
  
  dt_dominance_join <- dt_dominance_lines[
    , .(Wave, t_dominance = t)] |> unique()
  
  dt_data_plot <- dt_data[
    titre >= titre_min][titre <= titre_max][
    dt_emergence_join, on = "Wave"][
    dt_dominance_join, on = "Wave"][
    dt_dates, on = "Wave"][
    , Wave := fct_relevel(Wave, manual_facet_order)]

  # dt_data_pre_plot <- dt_data_plot[
  #   t_since_last_exp <= t_max,
  #   .SD[t_since_last_exp <= t_emergence + (t_dominance - t_emergence)/2],
  #   by = "Wave"]
  
  dt_data_pre_plot <- dt_data_plot[,
    # t_since_last_exp <= t_max,
    .SD[date <= date_emergence],
    by = "Wave"]

  dt_data_post_plot <- dt_data_plot[
    t_since_last_exp <= t_max,
    .SD[t_since_last_exp >= t_emergence &
        t_since_last_exp <= t_emergence + (t_dominance - t_emergence)/2],
    by = "Wave"]
  
  dt_data_post_plot <- dt_data_plot[
    t_since_last_exp <= t_max,
    .SD[t_since_last_exp >= t_emergence],
    by = "Wave"]
    
  dt_fits_pre_plot <- dt_fits_pre[
    dt_emergence_join, on = "Wave"][
    dt_dominance_join, on = "Wave"]
  
  dt_fits_post_plot <-  dt_fits_post[
    dt_emergence_join, on = "Wave"][
    dt_dominance_join, on = "Wave"]
  
  # dt_fits_pre_plot <- dt_fits_pre_plot[
  #   t <= t_max, .SD[t <= t_emergence],
  #   by = "Wave"][, Wave := fct_relevel(Wave, manual_facet_order)]
  
  # Trying version where we plot the entire real-time fit and
  # just the median of the hindsight fit
  dt_fits_pre_plot <- dt_fits_pre_plot[t <= t_max][
    , Wave := fct_relevel(Wave, manual_facet_order)]
  
  # dt_fits_post_plot <- dt_fits_post_plot[
  #   t <= t_max,
  #   .SD[t >= t_emergence & t <= t_dominance],
  #   by = "Wave"][, Wave := fct_relevel(Wave, manual_facet_order)]
   
  # Trying version where we plot the entire real-time fit and
  # just the median of the hindsight fit
  dt_fits_post_plot <- dt_fits_post_plot[
    t <= t_max,
    .SD[t <= t_dominance],
    by = "Wave"][, Wave := fct_relevel(Wave, manual_facet_order)]
  
  p_out <- ggplot() + 
    geom_line(data = dt_fits_pre_plot,
      aes(x = t,
          y = me,
          colour = `Titre type`)) +
    geom_ribbon(data = dt_fits_pre_plot,
      aes(x = t,
          ymin = lo,
          ymax = hi,
          fill = `Titre type`), alpha = alpha_fits_pre) + 
    geom_line(data = dt_h1_line, aes(x = x, y = y),
              linetype = "dotdash", colour = "gray30", alpha = 0.4) +
    geom_line(data = dt_h2_line, aes(x = x, y = y),
              linetype = "dotdash", colour = "gray30", alpha = 0.4) +
    geom_line(data = dt_emergence_lines, aes(x = t, y = y),
              linetype = "twodash", colour = "gray30") +
    # geom_line(data = dt_dominance_lines, aes(x = t, y = y),
    #           linetype = "dotdash", colour = "gray30") +
    scale_y_continuous(
      trans = "log2",
      breaks = c(40, 80, 160, 320, 640, 1280,
                 2560, 5120),
      labels = c("40", "80", "160", "320", "640", "1280", "2560", "5120"),
      limits = c(40, 20480)) +
    scale_x_continuous(breaks = c(0, 30, 60, 90, 120), 
                       labels = c("0", "30", "60", "90", "120"),
                       expand = c(0,0)) + 
    coord_cartesian(clip = "off") + 
    labs(x = "Time since last exposure (days)",
         y = expression(paste("Titre (IC"[50], ")"))) +
    # scale_x_date(NULL, date_labels = "%b %y") + 
    facet_nested(
      facet_formula,
      nest_line = element_line(linewidth = 0.5)) +
    # theme_cowplot(font_size = 7, font_family = "Helvetica", line_size = 0.25) + 
    theme(
      ggh4x.facet.nestline = element_line(),
      legend.position = "bottom",
      strip.text.x.top = element_text(size = 8, family = "Helvetica"),
      strip.text.x = element_text(size = 8, family = "Helvetica"))
  
  if (plot_data == TRUE) {
    p_out <- p_out + 
      geom_point(
        data = dt_data_pre_plot,
        aes(x = t_since_last_exp,
            y = titre,
            colour = `Titre type`),
        alpha = alpha_data_pre,
        size = 0.4) +
      geom_point(
        data = dt_data_post_plot,
        aes(x = t_since_last_exp,
            y = titre,
            colour = `Titre type`),
        alpha = alpha_data_post,
        size = 0.2, shape = 1) 
  }
  
  if (plot_beyond == TRUE) {
    p_out <- p_out +
      geom_line(
        data = dt_fits_post_plot,
        aes(x = t,
            y = me,
            colour = `Titre type`),
        linetype = "dashed") + 
      geom_ribbon(
        data = dt_fits_post_plot,
        aes(x = t,
            ymin = lo,
            ymax = hi,
            fill = `Titre type`),
        alpha = alpha_fits_post)
  }

  return(p_out)
}

generate_vertical_lines_data <- function(
    dt_data, dt_times, facet_formula, y_min, y_max) {
  
  # Extract facet variables
  facet_vars <- all.vars(facet_formula)
  
  # Get unique combinations of factor variables that exist in dt_data
  combinations <- unique(dt_data[, ..facet_vars])
  
  # Join combinations with dt_times on the 'Wave' column
  dt_out <- combinations[dt_times, on = "Wave"]
  
  # Expand for y min and max
  dt_out <- dt_out[
    , .(t = t, y = c(y_min, y_max)), by = facet_vars]
  
  return(dt_out)
}


plot_raw_data <- function(
    dt_data, 
    date_set, 
    facet_formula = `last_exp_type + infection_history ~ titre_type`) {
  
  p_out <- dt_data |>
    ggplot(aes(x = date, y = titre, colour = titre_type)) +
    geom_point(alpha = 0.5) +
    geom_line(aes(group = id), alpha = 0.1) + 
    geom_smooth(method = "lm") +
    geom_vline(
      aes(xintercept = date_set),
      linetype = "dotdash") + 
    geom_hline(
      aes(yintercept = 40),
      linetype = "twodash") +
    geom_hline(
      aes(yintercept = 5120),
      linetype = "twodash") +
    scale_x_date(
      date_breaks = waiver(),
      date_labels = "%b-%y",
    ) + 
    scale_y_continuous(
      trans = "log2",
      breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120),
      labels = c(expression(""<=40),
                 "80", "160", "320", "640", "1280", "2560",
                 expression("">=5120)), 
      limits = c(40, 5120)) +
    facet_nested(facet_formula) +
    labs(x = "Date", y = "Titre", colour = "Titre type") + 
    # scale_color_lancet() + 
    theme_minimal() + 
    theme(legend.position = "none")
  
  return(p_out)
  
}

custom_theme_function <- function() {
  
  theme_out <- list(
  geom_hline(yintercept = 40, linetype = "twodash", colour = "gray30"),
    geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30"),
    scale_y_continuous(
      trans = "log2",
      breaks = c(40, 80, 160, 320, 640, 1280, 2560),
      labels = c(expression(" "<= 40),
                 "80", "160", "320", "640", "1280",
                 expression(" ">=2560))),
    theme_minimal(),
    labs(x = "Time (days)",
         y = expression(paste("Titre value at set-point (IC"[50], ")")))
  )
  
  return(theme_out)
}
