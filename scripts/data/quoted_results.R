# Comparing the real-time fits to the retrospective fits
# This subsection relies on the results from the figure_3 script

# 1: Find time when abs(real-time - retrospective) > log(titre)
dt_fits_wide_sum[me < 1, .N]/dt_fits_wide_sum[, .N]
dt_fits_wide_sum[lo < 1, .N]/dt_fits_wide_sum[, .N]
dt_fits_wide_sum[hi < 1, .N]/dt_fits_wide_sum[, .N]

# 2: Find the difference between real-time and retrospective fits at the peak
# across all panels
dt_peak_switch_wide <- dcast(
  dt_peak_switch,
  .draw + `Infection history` + `Titre type` + `Wave` ~ `Type`,
  value.var = c("mu_p")) |>
  convert_log_scale(vars_to_transform = c("Real-time", "Retrospective"))

# Calculate the differences between the real-time and retrospective fits for 'mu_p'
dt_peak_switch_wide[, mu_p_diff := Retrospective - `Real-time`]

summarise_draws(dt_peak_switch_wide, column_name = "mu_p_diff")

# 3: Compare how much closer the real-time fits are to the retrospective than
# the two simpler approaches
dt_model_comparison[`Time point` == "Time of peak"][
  !(Type == "Real-time" & `Model Type` == "lm" &
      `Infection history` == "Previously infected (Omicron)" &
      Wave == "BA.2 wave")]

# Comparing peak titres of infection naive individuals to previously infected
# This subsection relies on the results from the figure_4 script
summarise_draws(
  dt_figure_4_data_plot, column_name = "mu_p",
  by = c("Infection history", "Wave"))



