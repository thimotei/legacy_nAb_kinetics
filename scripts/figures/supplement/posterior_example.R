library(tidybayes)
library(data.table)

source("R/adjust.R")

#--- Read in fit object
fit <- readRDS("outputs/fits/delta_full.rds")

#--- Extract posterior samples using tidybayes function spread_draws()
#--- Hard-coded parameter values (can generalise this later)
dt_samples_wide <- spread_draws(
  fit, 
  t0_pop[k],
  tp_pop[k],
  ts_pop[k],
  m1_pop[k],
  m2_pop[k],
  m3_pop[k],
  beta_t0[p],
  beta_tp[p],
  beta_ts[p],
  beta_m1[p],
  beta_m2[p],
  beta_m3[p]) |> data.table()

#--- Removing unused columns
dt_samples_wide[, `:=` (.chain = NULL, .iteration = NULL)]

#--- Changing column order
setcolorder(dt_samples_wide, c("k", "p", ".draw"))

#--- Adjusting for the beta coefficients
dt_samples_wide_adj <- adjust_parameters(
  dt_samples_wide)

#--- Creating time range
time_range <- seq(0, 150, 0.5)

#--- Adding time range to data.table
dt_times <- data.table(t = time_range)

#--- Adding artificial time IDs
dt_times[, t_id := 1, by = t]
dt_samples_wide_adj[, t_id := 1]

#--- Merging times with posterior samples, to create trajectory for
#--- each set of parameters
dt_trajectories <- merge(
  dt_samples_wide_adj, dt_times,
  by = "t_id",
  allow.cartesian = TRUE
)

#--- Simulating trajectories using posterior values
dt_trajectories[, mu := simulate_trajectory(
  t, t0_pop, tp_pop, ts_pop, m1_pop, m2_pop, m3_pop), 
  by = .(t, p, k, .draw)
]

#--- Calculate median and credible interval over time
dt_summary <- dt_trajectories[, .(
  me = quantile(mu, 0.5),
  lo = quantile(mu, 0.025),
  hi = quantile(mu, 0.975)
), by = .(t, p, k)
]

#--- Plot the fits 
