plot_figure_4 <- function(dt_data, dt_dates) {
  
  # dt_fits <- dt_fits[
  #   dt_dates, on = "Wave"]
  
  #--- Munging raw data for plot
  dt_data <- dt_data[
    dt_dates, on = "Wave"]
  
  # dt_data_plot <- dt_data[
  #   , .SD[date <= date_wave_next], by = Wave][
  #     Wave == "Delta" & `Titre type` == "Delta" |
  #       Wave == "BA.2" & `Titre type` == "BA.2" |
  #       Wave == "XBB" & `Titre type` == "XBB"][
  #         titre > 40][, Type := "Data"][
  #           , titre_min := 40][, titre_max := 2560][
  #             Wave == "Delta", date_wave := date_delta][
  #               Wave == "BA.2", date_wave := date_ba2][
  #                 Wave == "XBB", date_wave := date_xbb]
  
  dt_data_plot <- dt_data[
    , .SD[date <= date_wave_next], by = Wave][
          titre > 40][, Type := "Data"][
            , titre_min := 40][, titre_max := 2560][
              Wave == "Delta", date_wave := date_delta][
                Wave == "BA.2", date_wave := date_ba2][
                  Wave == "XBB", date_wave := date_xbb]
  
  #--- Munging fits for plot
  # dt_fits_plot <- dt_fits[
  #   , .SD[calendar_date <= date_wave_next], by = Wave][
  #     Wave == "Delta" & `Titre type` == "Delta" |
  #       Wave == "BA.2" & `Titre type` == "BA.2" |
  #       Wave == "XBB" & `Titre type` == "XBB"][
  #         lo > 30][, Type := "Modelled"][
  #           , titre_min := 40][, titre_max := 2560][
  #             Wave == "Delta", date_wave := date_delta][
  #               Wave == "BA.2", date_wave := date_ba2][
  #                 Wave == "XBB", date_wave := date_xbb]
  
  figure_4 <- ggplot() + 
    geom_point(
      data = dt_data_plot,
      aes(x = date, y = titre, colour = `Titre type`), size = 0.2, alpha = 0.5) + 
    geom_smooth(
      data = dt_data_plot,
      aes(x = date, y = titre, 
          colour = `Titre type`, fill = `Titre type`,
          group = interaction(Wave, `Titre type`)),
      alpha = 0.6) +
    geom_hline(
      data = dt_data_plot, 
      aes(yintercept = titre_min),
      linetype = "twodash", colour = "gray30", alpha = 0.5) +
    geom_hline(
      data = dt_data_plot, 
      aes(yintercept = titre_max),
      linetype = "twodash", colour = "gray30", alpha = 0.5) +
    # geom_vline(
    #   data = dt_data_plot, 
    #   aes(xintercept = date_wave_next),
    #   linetype = "dashed") +
    geom_vline(aes(xintercept = date_delta), linetype = "dashed") +
    geom_vline(aes(xintercept = date_ba2), linetype = "dashed") +
    geom_vline(aes(xintercept = date_xbb), linetype = "dashed") +
    scale_y_continuous(
      trans = "log2",
      breaks = c(40, 80, 160, 320, 640, 1280, 2560),
      labels = c(expression(""<=40),
                 "80", "160", "320", "640", "1280",
                 expression("">=2560)))
  
  return(figure_4)
}




