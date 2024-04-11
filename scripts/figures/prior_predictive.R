library(data.table)
library(truncnorm)
library(magrittr)
library(ggplot2)

source("scripts/setup/setup.R")

#--- Population-level prior predictive
dt_sample <- sample_pop_priors(n_samples = 10000)

dt_prior_pred <- simulate_trajectories(
  dt_sample, 
  by = c("t", "sample_id"),
  time_range = seq(0, 75, 1)) |> 
  convert_log_scale_inverse(vars_to_transform = "mu")

dt_prior_pred_sum <- summarise_draws(dt_prior_pred, "mu", by = "t")

dt_prior_pred <- dt_prior_pred_sum |> 
  ggplot(aes(x = t)) + 
  geom_line(aes(y = me)) + 
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.5, fill = "dodgerblue") + 
  custom_theme_function()

ggsave(
  "outputs/figures/supplementary_figures/prior_predictive.png", 
  dt_prior_pred,
  width = 8,
  height = 6,
  bg = "white")

#--- Individual-level prior predictive

# structure of procedure:
# supply vector of sigmas
# construct data.table in a format where performing 
# the calculation of each individuals prior predictive is easy

dt_sample[, .SD[rep(1:.N, each = 10)], by = sample_id] |> View()

sample_pop_priors()
