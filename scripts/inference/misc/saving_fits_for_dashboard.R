#--- saving fits and titre data for dashboard

#--- FITS
# neater data for saving
dt_2_dose_sum_save <- dt_2_dose_sum[
  , .(time = t, 
      me_trans = me,
      lo_95_trans = lo,
      hi_95_trans = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

# neater data for saving
dt_3_dose_sum_save <- dt_3_dose_sum[
  , .(time = t, 
      me_trans = me,
      lo_95_trans = lo,
      hi_95_trans = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

# neater data for saving
dt_4_dose_sum_save <- dt_4_dose_sum[
  , .(time = t, 
      me_trans = me,
      lo_95_trans = lo,
      hi_95_trans = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

# neater data for saving
dt_2_dose_sum_save <- dt_2_dose_sum_nat[
  , .(time = t, 
      me_nat = me,
      lo_95_nat = lo,
      hi_95_nat = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

# neater data for saving
dt_3_dose_sum_nat_save <- dt_3_dose_sum_nat[
  , .(time = t, 
      me_nat = me,
      lo_95_nat = lo,
      hi_95_nat = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

# neater data for saving
dt_4_dose_sum_nat_save <- dt_4_dose_sum_nat[
  , .(time = t, 
      me_nat = me,
      lo_95_nat = lo,
      hi_95_nat = hi,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_2_dose_save <- merge(dt_2_dose_sum_nat_save, dt_2_dose_sum_save)
dt_3_dose_save <- merge(dt_3_dose_sum_nat_save, dt_3_dose_sum_save)
dt_4_dose_save <- merge(dt_4_dose_sum_nat_save, dt_4_dose_sum_save)

#--- DATA
dt_stan_2_dose_save <- dt_stan_2_dose[
  , .(time = t_since_last_exp,
      titre_obs = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_3_dose_save <- dt_stan_3_dose[
  , .(time = t_since_last_exp,
      titre_obs = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_4_dose_save <- dt_stan_4_dose[
  , .(time = t_since_last_exp,
      titre_obs = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_2_dose_nat_save <- dt_stan_2_dose_nat[
  , .(time = t_since_last_exp,
      titre_obs_nat = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_3_dose_nat_save <- dt_stan_3_dose_nat[
  , .(time = t_since_last_exp,
      titre_obs_nat = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_4_dose_nat_save <- dt_stan_4_dose_nat[
  , .(time = t_since_last_exp,
      titre_obs_nat = titre,
      last_exposure = `Last exposure`,
      inf_history = `Infection history`,
      titre_type = `Titre type`)]

dt_stan_2_dose_save <- dt_stan_2_dose_save[
  , titre_obs_nat := dt_stan_2_dose_nat_save$titre_obs_nat][
  , .(time, titre_obs,
      titre_obs_nat, last_exposure, 
      inf_history, titre_type)]

dt_stan_3_dose_save <- dt_stan_3_dose_save[
  , titre_obs_nat := dt_stan_3_dose_nat_save$titre_obs_nat][
  , .(time, titre_obs,
        titre_obs_nat, last_exposure, 
        inf_history, titre_type)]

dt_stan_4_dose_save <- dt_stan_4_dose_save[
  , titre_obs_nat := dt_stan_4_dose_nat_save$titre_obs_nat][
  , .(time, titre_obs, titre_obs_nat,
      last_exposure, inf_history, titre_type)]

fwrite(dt_2_dose_save, "outputs/fit_2nd_vax.csv")
fwrite(dt_3_dose_save, "outputs/fit_3rd_vax.csv")
fwrite(dt_4_dose_save, "outputs/fit_4th_vax.csv")

fwrite(dt_stan_2_dose_save, "outputs/data_2nd_vax.csv")
fwrite(dt_stan_3_dose_save, "outputs/data_3rd_vax.csv")
fwrite(dt_stan_4_dose_save, "outputs/data_4th_vax.csv")

fread("outputs/fit_2nd_vax.csv")

