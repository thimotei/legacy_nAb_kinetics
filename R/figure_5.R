plot_figure_5 <- function(
    dt_data, dt_model, dt_dates, plot_data = TRUE,
    plot_model_splines = TRUE,
    alpha_splines = 0.1, alpha_model = 1,
    alpha_model_smooth = 1, span_model) {

  #--- Munging raw data for plot
  dt_data <- dt_data[dt_dates, on = "Wave"]

  dt_data_plot <- dt_data[
    , .SD[date <= date_wave_next], by = Wave][
          titre > 40][, Type := "Data"][
            , titre_min := 40][, titre_max := 2560][
              Wave == "Delta", date_wave := date_delta][
                Wave == "BA.2", date_wave := date_ba2][
                  Wave == "XBB", date_wave := date_xbb]

  min_date_ba2_model <- dt_model[Wave == "BA.2 wave", min(calendar_date)]
  min_date_xbb_model <- dt_model[Wave == "XBB wave", min(calendar_date)]
  max_date_xbb_model <- dt_model[Wave == "XBB wave", max(calendar_date)]

  dt_dates_model <- data.table(
    Wave = c("Delta wave", "BA.2 wave", "XBB wave"),
    date_wave_next = c(
      min_date_ba2_model, min_date_xbb_model, max_date_xbb_model))

  dt_model_plot <- dt_model[
    dt_dates_model, on = "Wave"][
      , .SD[calendar_date <= date_wave_next], by = Wave]

  p_out <- ggplot() +
    geom_hline(
      data = dt_data_plot,
      aes(yintercept = titre_min),
      linetype = "twodash", colour = "gray30", alpha = 0.5) +
    geom_hline(
      data = dt_data_plot,
      aes(yintercept = titre_max),
      linetype = "twodash", colour = "gray30", alpha = 0.5) +
    geom_vline(aes(xintercept = date_delta), linetype = "dashed") +
    geom_vline(aes(xintercept = date_ba2), linetype = "dashed") +
    geom_vline(aes(xintercept = date_xbb), linetype = "dashed") +
    scale_y_continuous(
      trans = "log2",
      breaks = c(40, 80, 160, 320, 640, 1280, 2560),
      labels = c(expression(""<=40),
                 "80", "160", "320", "640", "1280",
                 expression("">=2560)),
      limits = c(30, 5000)) +
    geom_line(
      data = dt_model_plot,
      aes(x = calendar_date,
          y = me,
          group = interaction(`Titre type`, Wave)),
      colour = "grey",
      alpha = alpha_model) +
    geom_ribbon(
      data = dt_model_plot,
      aes(x = calendar_date,
          ymin = lo,
          ymax = hi,
          group = interaction(`Titre type`, Wave)),
      alpha = alpha_model)

  if(plot_model_splines == TRUE) {
    p_out <- p_out +
      geom_smooth(
        data = dt_model_plot,
        aes(x = calendar_date,
            y = me,
            fill = `Titre type`,
            colour = `Titre type`,
            group = interaction(`Titre type`, Wave)),
        alpha = alpha_model_smooth, span = span_model)
  }

  if(plot_data == TRUE) {
    p_out <- p_out +
      geom_point(
        data = dt_data_plot,
        aes(x = date, y = titre, colour = `Titre type`),
        size = 0.2, alpha = 0.5)
  }

  return(p_out)
}




