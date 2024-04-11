library(ggh4x)
library(cmdstanr)
library(data.table)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggsci)
library(tidybayes)
library(stringr)

source("scripts/setup/setup.R")

# This is the full datafile, which is not publicly available
load("data_raw/Legacy_DataAnnotatedDateSeries_2023-11-09.RData")

# Convert the data.frame to a data.table
dt_raw <- data.table(chrono.df)

# Setting seed, so jumbling of IDs is the same each time
set.seed(123)

# Preprocess full data.table
dt_clean <- preprocess_data(
  dt_raw, trim_nas = TRUE, convert_scale = TRUE,
  simplify_voc_names = TRUE, range = "original")

# Removing old voc names after simplification and consolidation
dt_clean[, voc := NULL]

# However, this processed data file is publicly available
saveRDS(dt_clean, "data/processed_data.rds")

