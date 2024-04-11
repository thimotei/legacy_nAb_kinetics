library(data.table)

load("data/Legacy_DataAnnotatedDateSeries_2023-09-15.RData")
dt_raw <- data.table(chrono.df)

# isolating exposure data
dt_all <- dt_raw[, .(id = factor(as.numeric(elig_study_id)),
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
                     inf_date = lubridate::ymd(episode_start), 
                     inf_num = as.numeric(episode_number),
                     voc = factor(episode_variant_summarised),
                     vax_num = as.numeric(dose))][order(id, date)]

#--- sorting out infections first
dt_inf <- dt_all[, .(id, date, inf_num, voc)]

#--- munging the infection dates into wide format
# setting the new columns
dt_inf[!is.na(inf_num), inf_1_date := as.Date(NA)]
dt_inf[!is.na(inf_num), inf_2_date := as.Date(NA)]
dt_inf[!is.na(inf_num), inf_3_date := as.Date(NA)]
dt_inf[!is.na(inf_num), inf_4_date := as.Date(NA)]
dt_inf[!is.na(inf_num), inf_5_date := as.Date(NA)]

# adding the date for each infection number
dt_inf[inf_num == 1, inf_1_date := date]
dt_inf[inf_num == 2, inf_2_date := date]
dt_inf[inf_num == 3, inf_3_date := date]
dt_inf[inf_num == 4, inf_4_date := date]
dt_inf[inf_num == 5, inf_5_date := date]

# calculating the total number of infections by ID
dt_inf[, total_inf := max(inf_num, na.rm = TRUE), by = "id"]
dt_inf[is.na(total_inf), total_inf := 0, by = id]
dt_inf[is.infinite(total_inf), total_inf := 0, by = id]

dt_inf[, inf_1_date := min(inf_1_date, na.rm = TRUE), by = id]
dt_inf[, inf_2_date := min(inf_2_date, na.rm = TRUE), by = id]
dt_inf[, inf_3_date := min(inf_3_date, na.rm = TRUE), by = id]
dt_inf[, inf_4_date := min(inf_4_date, na.rm = TRUE), by = id]
dt_inf[, inf_5_date := min(inf_5_date, na.rm = TRUE), by = id]

dt_inf[is.infinite(inf_1_date), inf_1_date := NA]
dt_inf[is.infinite(inf_2_date), inf_2_date := NA]
dt_inf[is.infinite(inf_3_date), inf_3_date := NA]
dt_inf[is.infinite(inf_4_date), inf_4_date := NA]
dt_inf[is.infinite(inf_5_date), inf_5_date := NA]

#--- now adding a voc for each infection
# setting the new columns
dt_inf[!is.na(voc), inf_1_type := as.character(NA)]
dt_inf[!is.na(voc), inf_2_type := as.character(NA)]
dt_inf[!is.na(voc), inf_3_type := as.character(NA)]
dt_inf[!is.na(voc), inf_4_type := as.character(NA)]
dt_inf[!is.na(voc), inf_5_type := as.character(NA)]

dt_inf[inf_num == 1, inf_1_type := voc]
dt_inf[inf_num == 2, inf_2_type := voc]
dt_inf[inf_num == 3, inf_3_type := voc]
dt_inf[inf_num == 4, inf_4_type := voc]
dt_inf[inf_num == 5, inf_5_type := voc]

dt_inf[, inf_1_type := na.omit(inf_1_type)[1], by = id]
dt_inf[, inf_2_type := na.omit(inf_2_type)[1], by = id]
dt_inf[, inf_3_type := na.omit(inf_3_type)[1], by = id]
dt_inf[, inf_4_type := na.omit(inf_4_type)[1], by = id]
dt_inf[, inf_5_type := na.omit(inf_5_type)[1], by = id]

dt_inf[, inf_num := NULL]
dt_inf[, voc := NULL]

dt_inf <- dt_inf |> unique()

#--- now sorting out vaccines
dt_vax <- dt_all[, .(
  id, date,
  dose_1_date, dose_2_date, dose_3_date, dose_4_date, dose_5_date,
  dose_1_type, dose_2_type, dose_3_type, dose_4_type, dose_5_type, 
  vax_num)]

# filling in all dose numbers
dt_vax[, vax_num := nafill(vax_num, "locf"), by = "id"]

# replacing NAs with zeros for the rolling total of the number of vaccines
dt_vax[is.na(vax_num), vax_num := 0, by = "id"]

dt_vax[, total_vax := max(vax_num, na.rm = TRUE), by = id]
dt_vax[is.na(total_vax), total_vax := 0, by = id]
dt_vax[is.infinite(total_vax), total_vax := 0, by = id]

id_1 <- dt_all[, .SD[dose_2_date < dose_1_date], by = id][, unique(id)] # returns one ID
id_2 <- dt_all[, .SD[dose_3_date < dose_2_date], by = id][, unique(id)] # returns one ID
ids_to_remove <- c(id_1, id_2)
dt_vax <- dt_vax[!id %in% ids_to_remove]

dt_vax <- dt_vax |> unique()

#--- merging infection and vaccine data
dt_exp <- merge(dt_inf, dt_vax, by = c("id", "date"))


# For treatment group:
# dt_exp[treatment == 1, time_to_event_treatment := {
#   start_time <- dose_2_date
#   end_time <- pmin(inf_1_date, inf_2_date, inf_3_date, inf_4_date, dose_4_date, na.rm = TRUE)
#   as.numeric(end_time - start_time, units = "days")
# }, by = id]

# For placebo group:
# dt_exp[treatment == 0, time_to_event_placebo := {
#   start_time <- dose_2_date
#   end_time <- pmin(inf_1_date, dose_3_date, na.rm = TRUE)
#   as.numeric(end_time - start_time, units = "days")
# }, by = id]
# 
# # Creating the event columns:
# dt_exp[treatment == 1, event_treatment := as.integer(!is.na(inf_1_date) & inf_1_date <= dose_4_date)]
# dt_exp[treatment == 0, event_placebo := as.integer(!is.na(inf_1_date) & inf_1_date <= dose_3_date)]
# 
# # For the treatment group
# treatment_data <- dt_exp[treatment == 1, .(
#   id,
#   time_to_event = as.numeric(time_to_event_treatment, units = "days"),
#   event = as.integer(!is.na(time_to_event_treatment) & (time_to_event_treatment <= pmin(inf_1_date, inf_2_date, inf_3_date, inf_4_date, na.rm=TRUE) - dose_3_date))
# )]
# 
# placebo_data <- dt_exp[treatment == 0, .(
#   id,
#   time_to_event = as.numeric(pmin(inf_1_date, dose_3_date, na.rm = TRUE) - min(), units = "days"),
#   event = as.integer(!is.na(inf_1_date) & inf_1_date <= dose_3_date)
# )]
# 
# # Combine the datasets
# combined_data <- rbindlist(list(treatment_data, placebo_data))
# 
# # Create list for Stan
# stan_data <- list(
#   N = nrow(combined_data),
#   time = combined_data$time_to_event,
#   event = combined_data$event
# )
# 
total_individuals <- dt_exp[, uniqueN(id)]

# For vaccine types:
vaccine_types <- unique(c(na.omit(dt_exp$dose_1_type), na.omit(dt_exp$dose_2_type),
                          na.omit(dt_exp$dose_3_type), na.omit(dt_exp$dose_4_type),
                          na.omit(dt_exp$dose_5_type)))

# For infection types:
infection_types <- unique(c(na.omit(dt_exp$inf_1_type), na.omit(dt_exp$inf_2_type),
                            na.omit(dt_exp$inf_3_type), na.omit(dt_exp$inf_4_type),
                            na.omit(dt_exp$inf_5_type)))

# # Calculating remaining individuals stratified by vaccine type for each dose:
# 
for (vax in vaccine_types) {
  dt_exp[, paste0(vax) := total_individuals -
           sum((dose_1_type == vax & !is.na(dose_1_date) & dose_1_date <= date) |
                 (dose_2_type == vax & !is.na(dose_2_date) & dose_2_date <= date) |
                 (dose_3_type == vax & !is.na(dose_3_date) & dose_3_date <= date) |
                 (dose_4_type == vax & !is.na(dose_4_date) & dose_4_date <= date) |
                 (dose_5_type == vax & !is.na(dose_5_date) & dose_5_date <= date)),
         by = date]
}

# Calculating remaining individuals stratified by infection type:

for (inf in infection_types) {
  dt_exp[, paste0(inf) := total_individuals -
           sum((inf_1_type == inf & !is.na(inf_1_date) & inf_1_date <= date) |
                 (inf_2_type == inf & !is.na(inf_2_date) & inf_2_date <= date) |
                 (inf_3_type == inf & !is.na(inf_3_date) & inf_3_date <= date) |
                 (inf_4_type == inf & !is.na(inf_4_date) & inf_4_date <= date) |
                 (inf_5_type == inf & !is.na(inf_5_date) & inf_5_date <= date)),
         by = date]
}

desired_columns <- c(
  "date", 
  as.character(vaccine_types), 
  as.character(infection_types))

dt_exp_trim <- dt_exp[, ..desired_columns]

dt_exp_trim_long <- melt(
  dt_exp_trim,
  measure.vars = names(dt_exp_trim)[
    !names(dt_exp_trim) %in% "date"],
  variable.name = "exposure_type") |> unique()

dt_exp_trim_long[, min_by_type := min(value), by = exposure_type]
dt_exp_trim_long[, max_by_type := max(value), by = exposure_type]

dt_exp_trim_long[, `:=` (
  surv_me = binom.test(value, max_by_type)$estimate[[1]],
  surv_lo = binom.test(value, max_by_type)$conf.int[[1]],
  surv_hi = binom.test(value, max_by_type)$conf.int[[2]]), 
  by = c("date", "exposure_type")]

cols_to_exclude <- 
  dt_exp_trim_long[, min_by_type |> unique(),
                   by = exposure_type][order(V1)][, exposure_type][15:24]

vaccine_types_plot <- setdiff(vaccine_types, cols_to_exclude)
infection_types_plot <- setdiff(infection_types, cols_to_exclude)

p_vax <- dt_exp_trim_long[exposure_type %in% vaccine_types_plot] |> 
  ggplot() + 
  geom_line(
    aes(x = date,
        y = surv_me,
        colour = exposure_type)) + 
  geom_ribbon(
    aes(x = date,
        ymin = surv_lo,
        ymax = surv_hi,
        fill = exposure_type), alpha = 0.5) + 
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(x = "Date",
       y = "Survival probability",
       title = "Survival by vaccine type")

p_inf <- dt_exp_trim_long[exposure_type %in% infection_types_plot] |> 
  ggplot() + 
  geom_line(
    aes(x = date,
        y = surv_me,
        colour = exposure_type)) + 
  geom_ribbon(
    aes(x = date,
        ymin = surv_lo,
        ymax = surv_hi,
        fill = exposure_type), alpha = 0.5) + 
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date",
       y = "Survival probability",
       title = "Survival by infection type")

cowplot::plot_grid(p_vax, p_inf, nrow = 2)

dt_all[is.na(dose_3_date), uniqueN(id)]

  