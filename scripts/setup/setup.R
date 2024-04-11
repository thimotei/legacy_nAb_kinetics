library(ggh4x)
library(cmdstanr)
library(data.table)
library(ggplot2)
library(lubridate)
library(forcats)
library(ggsci)
library(tidybayes)
library(stringr)
library(truncnorm)

# Loading in processed data - using condensed wave data by default
dt_clean <- readRDS("data/processed_data_condensed.rds")

# Listing all of the functions in the R folder
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

# Sourcing all of the functions in the R folder
invisible(lapply(r_files, source))

# Same covariate formula for whole analysis for the time being
covariate_formula <- ~ 0 + infection_history:last_exp_type
