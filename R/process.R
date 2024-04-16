preprocess_data <- function(
    dt_in, trim_nas = TRUE,
    convert_scale = TRUE,
    simplify_voc_names = TRUE) {

  dt_copy <- copy(dt_in)

  dt_proc <- dt_copy[, .(original_id = factor(as.numeric(elig_study_id)),
                         date = lubridate::ymd(calendar_date),
                         dose_1_date = lubridate::ymd(date_dose_1),
                         dose_2_date = lubridate::ymd(date_dose_2),
                         dose_3_date = lubridate::ymd(date_dose_3),
                         dose_4_date = lubridate::ymd(date_dose_4),
                         dose_5_date = lubridate::ymd(date_dose_5),
                         dose_1_type = factor(dose_1),
                         dose_2_type = factor(dose_2),
                         dose_3_type = factor(dose_3),
                         dose_4_type = factor(dose_4),
                         dose_5_type = factor(dose_5),
                         Ancestral = ic50_wildtype,
                         Alpha = ic50_Alpha,
                         Delta = ic50_Delta,
                         BA.1 = ic50_Omicron_BA1,
                         BA.2 = ic50_Omicron_BA2,
                         BA.5 = ic50_Omicron_BA5,
                         BQ.1.1 = ic50_BQ.1.1,
                         XBB = ic50_XBB,
                         inf_date = lubridate::ymd(episode_start),
                         inf_num = as.numeric(episode_number),
                         voc = factor(episode_variant_summarised),
                         vax_num = as.numeric(dose))][order(original_id, date)]

  #--- Changing IDs
  # Shuffle the data.table rows to jumble up the original IDs
  dt_proc <- dt_proc[sample(.N)]

  # Assign new IDs from 1 to N in this new shuffled order
  dt_proc <- dt_proc[, id := .GRP, by = original_id][order(date, id)]

  # Remove original ID
  dt_proc[, original_id := NULL]

  setcolorder(dt_proc, "id")

  #--- Infection history
  # Fill in missing values in inf_date with the next available value within each id
  dt_proc[, last_inf_date := zoo::na.locf(inf_date, na.rm = FALSE), by = id]

  # Fill in missing values in voc with the next available value within each id
  dt_proc[, last_inf_type := zoo::na.locf(voc, na.rm = FALSE), by = id]

  # Fill in missing values in inf_date with the next available value within each id
  dt_proc[, inf_num := zoo::na.locf(inf_num, na.rm = FALSE), by = id]

  # Changing NAs in the inf_num to zeros, now that the rest of the values are padded
  dt_proc[is.na(inf_num), inf_num := 0]

  # Adding total number of infections to each ID
  dt_proc[, total_inf := max(inf_num, na.rm = TRUE), by = id]

  # Calculating the time since the last infection
  dt_proc[, t_since_last_inf := as.numeric(date - last_inf_date), by = c("id", "inf_num")]

  #--- Vaccine history

  # (our) ID = 802 has a strange 1st vaccine date. Motivated me to remove anyone with spurious
  # vaccination dates (vaccine 2 before vaccine 1 for example)

  id_1 <- dt_proc[, .SD[dose_2_date < dose_1_date], by = id][, unique(id)] # returns one ID
  id_2 <- dt_proc[, .SD[dose_3_date < dose_2_date], by = id][, unique(id)] # returns one ID
  dt_proc[, .SD[dose_4_date < dose_3_date], by = id] # this returns no-one
  dt_proc[, .SD[dose_5_date < dose_4_date], by = id] # this returns no-one

  ids_to_remove <- c(id_1, id_2)

  dt_proc <- dt_proc[!id %in% ids_to_remove]

  # Filling in all dose numbers
  dt_proc[, vax_num := nafill(vax_num, "locf"), by = "id"]

  # Replacing NAs with zeros for the rolling total of the number of vaccines
  dt_proc[is.na(vax_num), vax_num := 0, by = "id"]

  # Filling in a single column with the date of each individuals next vaccine
  dt_proc[date >= dose_1_date & (date <= dose_2_date | is.na(dose_2_date)), last_vax_date := dose_1_date, by = "id"]
  dt_proc[date >= dose_2_date & (date <= dose_3_date | is.na(dose_3_date)), last_vax_date := dose_2_date, by = "id"]
  dt_proc[date >= dose_3_date & (date <= dose_4_date | is.na(dose_4_date)), last_vax_date := dose_3_date, by = "id"]
  dt_proc[date >= dose_4_date & (date <= dose_5_date | is.na(dose_5_date)), last_vax_date := dose_4_date, by = "id"]
  dt_proc[date >= dose_5_date, last_vax_date := dose_5_date, by = "id"]

  # Doing the NAs last, as this sets the class to numeric of the new variable, which complains with the other
  # vaccine commands
  dt_proc[date < dose_1_date, last_vax_date := NA, by = "id"]

  # Filling in a single column with the type of each individuals next vaccine
  dt_proc[date >= dose_1_date & (date < dose_2_date | is.na(dose_2_date)), last_vax_type := dose_1_type, by = "id"]
  dt_proc[date >= dose_2_date & (date < dose_3_date | is.na(dose_3_date)), last_vax_type := dose_2_type, by = "id"]
  dt_proc[date >= dose_3_date & (date < dose_4_date | is.na(dose_4_date)), last_vax_type := dose_3_type, by = "id"]
  dt_proc[date >= dose_4_date & (date < dose_5_date | is.na(dose_5_date)), last_vax_type := dose_4_type, by = "id"]
  dt_proc[date >= dose_5_date, last_vax_type := dose_5_type, by = "id"]

  # Doing the NAs last, as this sets the class to numeric of the new variable, which complains with the other
  # vaccine commands
  dt_proc[date < dose_1_date, last_vax_type := NA, by = "id"]

  # Adding total number of vaccines to each ID
  dt_proc[, total_vax := max(vax_num, na.rm = TRUE), by = id]

  # Calculating the time since the last vaccination
  dt_proc[, t_since_last_vax := as.numeric(date - last_vax_date), by = c("id", "vax_num")]

  #--- Now attempting to summarise the total number of exposures for each individual, by
  #--- Summing the vaccines and infections at each time point

  # Calculating the rolling total number of exposures
  dt_proc[, exp_num := inf_num + vax_num, by = id]

  # Adding total number of exposures for each individual
  dt_proc[, total_exp := max(exp_num), by = id]

  # Filling in a single column with the date of each individuals next vaccine
  dt_proc[last_inf_date >= last_vax_date, last_exp_date := last_inf_date, by = c("id", "date", "exp_num")]
  dt_proc[last_vax_date >= last_inf_date, last_exp_date := last_vax_date, by = c("id", "date", "exp_num")]

  dt_proc[last_inf_date >= last_vax_date, last_exp_type := last_inf_type, by = c("id", "date", "exp_num")]
  dt_proc[last_vax_date >= last_inf_date, last_exp_type := last_vax_type, by = c("id", "date", "exp_num")]

  dt_proc[is.na(last_inf_date) & !is.na(last_vax_date), last_exp_date := last_vax_date, by = c("id", "date", "exp_num")]
  dt_proc[!is.na(last_inf_date) & is.na(last_vax_date), last_exp_date := last_inf_date, by = c("id", "date", "exp_num")]

  dt_proc[is.na(last_inf_date) & !is.na(last_vax_date), last_exp_type := last_vax_type, by = c("id", "date", "exp_num")]
  dt_proc[!is.na(last_inf_date) & is.na(last_vax_date), last_exp_type := last_inf_type, by = c("id", "date", "exp_num")]

  # Adding the time since the last exposure for each individual
  dt_proc[, t_since_last_exp := as.numeric(date - last_exp_date), by = c("id", "exp_num")]

  titre_types <- c("Ancestral", "Alpha", "Delta", "BA.1", "BA.2", "BA.5",  "BQ.1.1", "XBB")

  # Summarising last exposure in general
  dt_proc[
    !is.na(last_exp_date) & last_exp_date == last_inf_date,
    exp_type_sum := "Infection"]

  dt_proc[
    !is.na(last_exp_date) & last_exp_date == last_vax_date,
    exp_type_sum := "Vaccination"]

  if(simplify_voc_names == TRUE) {

    # Simplifying VOC names by default
    dt_proc <- simplify_voc_names(dt_proc, condense_waves = TRUE)

    # Then calculating infection history
    #----- TO-DO: add an option to be able to do this without condensing first
    dt_proc <- calculate_infection_history(dt_proc)
  }

  dt_out <- melt(dt_proc, measure.vars = titre_types,
                 variable.name = "titre_type",
                 value.name = "titre")

  # Tweaking dodgy titre value - over upper censored limit
  dt_out[!is.na(titre) &
         titre > 2560 &
         titre < 5120, titre := 2560]

  # Adding dates of last bleeds, so we can safely remove any missing titre rows
  # without removing exposures
  dt_out[!is.na(titre), last_bleed_date := date]
  dt_out[, last_bleed_date := nafill(last_bleed_date, type = "locf"),
    by = .(id, titre_type)]

  if(trim_nas == TRUE) {
    dt_out <- dt_out[
      !(is.na(titre) &
      (last_exp_date > last_bleed_date & last_exp_date < date))]
  }

  # Removing possible duplicate rows after the many melts!
  dt_out <- dt_out |> unique()

  # Adding observation ID
  dt_out[, obs_id := .I]

  if(convert_scale == TRUE) {
    dt_out <- convert_log_scale(dt_out, vars_to_transform = "titre")
  }

  return(dt_out)
}

process_fits <- function(
    fit, dt_stan, stan_data,
    formula, time_type = "relative", t_max,
    by = c("Infection history", "Titre type"),
    cleaned_names = c("Infection history", "Titre type")) {

  dt_sum <- summarise_pop_fit(
    fit, time_range = seq(0, t_max))

  dt_sum_nat <- convert_log_scale_inverse(
    copy(dt_sum))

  dt_sum_nat <- recover_covariate_names(
    dt_sum_nat, dt_stan, stan_data, formula)

  dt_out <- clean_covariate_names(
    dt_sum_nat, formula, cleaned_names = cleaned_names)

  if(time_type == "absolute") {
    dt_out[, date := dt_stan[, unique(min(date))] + t,
           by = by]
  }

  dt_out <- dt_out[
    , lapply(.SD, function(x) if (is.factor(x)) fct_drop(x) else x)]

  return(dt_out)
}


