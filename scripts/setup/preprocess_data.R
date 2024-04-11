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

load("data/Legacy_DataAnnotatedDateSeries_2023-11-09.RData")

# Convert the data.frame to a data.table
dt_raw <- data.table(chrono.df)

# Preprocess full data.table
dt_clean <- preprocess_data(
  dt_raw, trim_nas = TRUE, convert_scale = TRUE,
  simplify_voc_names = TRUE, range = "original") 

# Saving processed data
saveRDS(dt_clean, "data/processed_data_condensed.rds")

