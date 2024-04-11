# Loading data
source("scripts/inference/delta.R")
source("scripts/inference/ba2.R")
source("scripts/inference/xbb.R")
source("scripts/inference/no_exposures.R")

#------------------------------------------------#
#----- Extracting data and parameter values -----#
#------------------------------------------------#

dt_all_data_table <- copy(rbind(
  dt_delta_data_full[, Wave := "Delta"],
  dt_ba2_data_full[, Wave := "BA.2"],
  dt_xbb_data_full[, Wave := "XBB"]))

# Loading the full data object - NOT publicly available
load("data/Legacy_DataAnnotatedDateSeries_2023-11-09.RData")

# Convert the data.frame to a data.table
dt_raw <- data.table(chrono.df)

# Extracting the extra bits not in public data file
dt_comorbidity <- dt_raw[
  , .(id = factor(as.numeric(elig_study_id)),
      date = lubridate::ymd(calendar_date),
      age = as.numeric(age),
      sex = sex,
      bmi = as.numeric(BMI),
      ache = factor(ache),
      cold = factor(cold),
      diarrhoea = factor(diarrhoea),
      fatigue = factor(fatigue),
      fever = factor(fever),
      hosp = factor(hosp),
      lossSmell = factor(lossSmell),
      oth = factor(oth),
      shrtbrth = factor(shrtbrth),
      description = factor(description),
      regular_meds = factor(regular_meds),
      smoking_status = factor(smoking_status),
      daily_smoker = factor(daily_smoker),
      symptoms = episode_symptoms_only,
      severity = factor(Sx_severity),
      site = centre)]

# Merging extra covariates with data we fit to
dt_comorbidity_trim <- merge(
  dt_all_data_table[, .(
    id = factor(id), date, Wave, titre_type)],
  dt_comorbidity,
  all.x = TRUE,
  by = c("id", "date"))

#------------------------#
#----- Table values -----#
#------------------------#

# Total number of exposures (N)
dt_comorbidity_trim[, .(exposures = uniqueN(id))]

# Total number of exposures by wave
dt_comorbidity_trim[, .(exposures = uniqueN(id)),
  by = .(Wave)]

# Total number of exposures by wave and titre type
dt_comorbidity_trim[, .(exposures = uniqueN(id)),
                 by = .(Wave, titre_type)]

# Total number of observations (n)
dt_all_data_table[
  , .(obs = uniqueN(date)), by = id][
    , sum(obs)]

# Total numbers of observations by wave
dt_obs <- dt_all_data_table[
  !is.na(titre), .N, by = .(Wave, titre_type)][
  , total := sum(N), by = Wave][
  , percentage := N/total * 100]

print(dt_obs)

# Types of vaccine
dt_vax_totals <- dt_all_data_table[, .(N = uniqueN(id)),
  by = .(last_vax_type, Wave, titre_type)]

dt_exp_totals <- dt_all_data_table[, .(total = uniqueN(id)), by = .(titre_type, Wave)]

dt_vax_totals <- merge(dt_vax_totals, dt_exp_totals, by = c("Wave", "titre_type"))

dt_vax_totals[order(
  fct_relevel(Wave, "Delta"),
  fct_relevel(
    titre_type,
    "Ancestral", "Alpha", "Delta",
    "BA.1", "BA.2", "BA.5", "BQ.1.1", "XBB")),
  .(last_vax_type, N, percentage = signif(N/total * 100, 3)), by = c("Wave", "titre_type")]

# Age
dt_age <- dt_comorbidity_trim[
  , .(id, age, Wave)][
  , .SD[age == unique(age)], by = id][
  , .(mean = signif(mean(age, na.rm = TRUE), 2),
      iqr = signif(IQR(age, na.rm = TRUE), 2)),
      by = Wave]

dt_all_data_table[, uniqueN(id)]

dt_age[, lo := mean - iqr]
dt_age[, hi := mean + iqr]

dt_age

# Sex
dt_comorbidity_trim[
  , .(sex = unique(sex)), by = .(id, Wave)][
  , .N, by = .(sex, Wave)][order(Wave, sex)]

dt_comorbidity_trim[, uniqueN(id)]

# Site
dt_comorbidity_trim[, total_by_wave := uniqueN(id), by = .(titre_type, Wave)]

dt_sites <- dt_comorbidity_trim[, .(
  total = unique(total_by_wave),
  N = uniqueN(id)), by = .(site, titre_type, Wave)][
  , .(paste0(N, " (",percentage = signif(N/total*100, 3),"%)")), by = .(site, titre_type, Wave)][
  order(fct_relevel(Wave, "Delta"))]

dt_sites |> gt::gt(groupname_col = c("titre_type"))

#------------------------------------------#
#----- Calculating peak times by wave -----#
#------------------------------------------#

# Read in fit with number of exposures as covariate
fit_exp <- readRDS("outputs/fits/exposures_new_red.rds")

# Extracting parameters
dt_params <- extract_parameters_pop_clean(
  fit_exp,
  dt_data = dt_exp_data,
  dt_data_stan = dt_exp_stan,
  stan_data = stan_data_exp,
  formula = formula_exp,
  cleaned_names = c("Exposure number", "Titre type"))

dt_params[variable %in% c("tp_pop", "ts_pop"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]


#-----------------------------------------------------#
#----- Calculating peak times by exposure number -----#
#-----------------------------------------------------#

# Read in fit with number of exposures as covariate
fit_exp <- readRDS("outputs/fits/exposures_new_red.rds")

# Extracting parameters
dt_params <- extract_parameters_pop_clean(
  fit_exp,
  dt_data = dt_exp_data,
  dt_data_stan = dt_exp_stan,
  stan_data = stan_data_exp,
  formula = formula_exp,
  cleaned_names = c("Exposure number", "Titre type"))

dt_params[variable %in% c("tp_pop", "ts_pop"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("tp_pop", "ts_pop") &
            `Titre type` %in% c("Alpha"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("tp_pop", "ts_pop") &
            `Titre type` %in% c("Delta"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("tp_pop", "ts_pop") &
            `Titre type` %in% c("XBB"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

#------------------------------------------------------#
#----- Calculating differences between peak times -----#
#------------------------------------------------------#

dt_peak_diff_1 <- dt_params[
  `Exposure number` %in% c(2, 3, 4, 5, 6) &
    variable %in% c("tp_pop") &
    `Titre type` %in% c("Alpha", "Delta")]

dt_peak_diff_1_wide <- dcast(
  dt_peak_diff_1,
  `Exposure number` + .draw ~ `Titre type`,
  value.var = "value")

dt_peak_diff_1_wide[, value :=  Delta - Alpha]

dt_peak_diff_1_wide[, .(
  me = signif(quantile(value, 0.5), 3),
  lo = signif(quantile(value, 0.025), 3),
  hi = signif(quantile(value, 0.975), 3))]

dt_peak_diff_2 <- dt_params[
  `Exposure number` %in% c(2, 3, 4, 5, 6) &
    variable %in% c("tp_pop") &
    `Titre type` %in% c("Delta", "XBB")]

dt_peak_diff_2_wide <- dcast(
  dt_peak_diff_2,
  `Exposure number` + .draw ~ `Titre type`,
  value.var = "value")

dt_peak_diff_2_wide[, value := XBB - Delta]

dt_peak_diff_2_wide[, .(
  me = signif(quantile(value, 0.5), 3),
  lo = signif(quantile(value, 0.025), 3),
  hi = signif(quantile(value, 0.975), 3))]


#---------------------------------------#
#----- Calculating set point times -----#
#---------------------------------------#

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("ts_pop") &
            `Titre type` %in% c("Ancestral"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("ts_pop") &
            `Titre type` %in% c("BA.2"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]

dt_params[`Exposure number` %in% c(2, 3, 4, 5, 6) &
            variable %in% c("ts_pop") &
            `Titre type` %in% c("XBB"),
          .(me = signif(quantile(value, 0.5), 3),
            lo = signif(quantile(value, 0.025), 3),
            hi = signif(quantile(value, 0.975), 3)),
          by = .(variable)]


#------------------------------------------------------#
#----- Calculating differences between peak times -----#
#------------------------------------------------------#

dt_setpoint_diff_1 <- dt_params[
  `Exposure number` %in% c(2, 3, 4, 5, 6) &
    variable %in% c("ts_pop") &
    `Titre type` %in% c("Ancestral", "Alpha")]

dt_setpoint_diff_1_wide <- dcast(
  dt_setpoint_diff_1,
  `Exposure number` + .draw ~ `Titre type`,
  value.var = "value")

dt_setpoint_diff_1_wide[, value :=  Alpha - Ancestral]

dt_setpoint_diff_1_wide[, .(
  me = signif(quantile(value, 0.5), 3),
  lo = signif(quantile(value, 0.025), 3),
  hi = signif(quantile(value, 0.975), 3))]

dt_setpoint_diff_2 <- dt_params[
  `Exposure number` %in% c(2, 3, 4, 5, 6) &
    variable %in% c("ts_pop") &
    `Titre type` %in% c("BA.2", "XBB")]

dt_setpoint_diff_2_wide <- dcast(
  dt_setpoint_diff_2,
  `Exposure number` + .draw ~ `Titre type`,
  value.var = "value")

dt_setpoint_diff_2_wide[, value := XBB - BA.2]

dt_setpoint_diff_2_wide[, .(
  me = signif(quantile(value, 0.5), 3),
  lo = signif(quantile(value, 0.025), 3),
  hi = signif(quantile(value, 0.975), 3))]
