library(ggh4x)
library(cmdstanr)
library(data.table)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggsci)
library(tidybayes)
library(stringr)

# This is the full datafile, which is not publicly available
load("data_raw/Legacy_DataAnnotatedDateSeries_2023-11-09.RData")

# Loading Roche N data
dt_roche_n <- readRDS(
  "data_raw/Roche_N_and_S_results_for_LSHTM_2024-05-21.RData") |>
  data.table()

# Convert the data.frame to a data.table
dt_raw <- data.table(chrono.df)

# Merging new anti-N data with original Legacy object
dt_raw <- merge(
  dt_raw, dt_roche_n, by = c("calendar_date", "elig_study_id"),
  all.x = TRUE)

# Setting seed, so jumbling of IDs is the same each time
set.seed(123)

# Preprocess full data.table
dt_clean <- preprocess_data(
  dt_raw, trim_nas = FALSE, convert_scale = TRUE,
  add_infection_history = TRUE,
  simplify_voc_names = TRUE, use_anti_n = FALSE)

# Removing old voc names after simplification and consolidation
dt_clean[, voc := NULL]

# However, this processed data file is publicly available
saveRDS(dt_clean, "data_raw/processed_data.rds")

