#--- Delta wave
dt_delta_ind <- extract_ind_params_clean(
  fit = fit_delta_full,
  dt_data = dt_delta_full_stan, 
  scale = "natural",
  wave_manual = "Delta")

dt_delta_ind_sum <- summarise_draws_vector(
  dt_delta_ind, 
  column_names = c("t0_ind", "tp_ind", "ts_ind", "m1_ind", "m2_ind", "m3_ind"), 
  by = c("stan_id", "titre_type", "infection_history"),
  format = "long")

dt_delta_ind_sum |> 
  ggplot() + 
  geom_pointrange(
    aes(y = stan_id, 
        x = me, xmin = lo, xmax = hi,
        colour = titre_type)) + 
  facet_nested(infection_history ~ parameter, independent = "all", scale = "free")

#--- BA.2 wave
dt_ba2_ind <- extract_ind_params_clean(
  fit = fit_ba2_full,
  dt_data = dt_ba2_full_stan, 
  scale = "log",
  wave_manual = "BA.2")

# removing NaNs for now, think its just missing observations
# being merged with full list of IDs
dt_ba2_ind <- dt_ba2_ind[!is.nan(t0_ind)]

dt_ba2_ind_sum <- summarise_draws_vector(
  dt_ba2_ind, 
  column_names = c("t0_ind", "tp_ind", "ts_ind", "m1_ind", "m2_ind", "m3_ind"), 
  by = c("stan_id", "titre_type", "infection_history"),
  format = "long")

dt_ba2_ind_sum |> 
  ggplot() + 
  geom_pointrange(
    aes(y = stan_id, 
        x = me, xmin = lo, xmax = hi,
        colour = titre_type)) + 
  facet_nested(infection_history ~ parameter, independent = "all", scale = "free")

#--- XBB wave
dt_xbb_ind <- extract_ind_params_clean(
  fit = fit_xbb_full,
  dt_data = dt_xbb_full_stan, 
  scale = "log",
  wave_manual = "XBB")

dt_xbb_ind <- dt_xbb_ind[!is.nan(t0_ind)]

dt_xbb_ind_sum <- summarise_draws_vector(
  dt_xbb_ind, 
  column_names = c("t0_ind", "tp_ind", "ts_ind", "m1_ind", "m2_ind", "m3_ind"), 
  by = c("stan_id", "titre_type", "infection_history"),
  format = "long")

dt_xbb_ind_sum |> 
  ggplot() + 
  geom_pointrange(
    aes(y = stan_id, 
        x = me, xmin = lo, xmax = hi,
        colour = titre_type)) + 
  facet_nested(infection_history ~ parameter, independent = "all", scale = "free")
