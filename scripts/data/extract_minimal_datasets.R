# Load full data and extract minimal datasets ----------------------------------
# Load data
dt_clean <- readRDS("data_raw/processed_data.rds")

# Defining columns to be kept across all three wave datasets
cols_to_keep <- c(
  "id", "stan_id", "date", "obs_id", "t_since_last_exp",
  "infection_history", "titre", "titre_type", "titre_type_num",
  "last_exp_date", "relevant_last_exp_date", "censored",
  "last_vax_type", "exp_num")

# Extract and save minimal datasets
source("scripts/data/exposures.R")
source("scripts/data/delta.R")
source("scripts/data/ba2.R")
source("scripts/data/xbb.R")
