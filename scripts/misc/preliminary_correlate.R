# Very rough analysis of COP based on a single wave

# Run data loading script first:
source("preliminary_plots.R ")

# NOTE: Needs to be refactored to ensure only one unique entry per individual

library(mgcv)

# 1. Create a binary column indicating infection episode occurrence
date_1 <- "2022-12-01"
date_2 <- "2023-03-01"
  
dt_clean[, infection_during_period := as.integer(
  (episode_1_start >= date_1 & episode_1_start <= date_2) |
    (episode_2_start >= date_1 & episode_2_start <= date_2) |
    (episode_3_start >= date_1 & episode_3_start <= date_2) |
    (episode_4_start >= date_1 & episode_4_start <= date_2) |
    (episode_5_start >= date_1 & episode_5_start <= date_2)
), by = id]

# Extract unique IDs of those in the dataset
infected_ids <- unique(dt_clean[, id])

# 2. Extract the most recent antibody titre against titre_type="XBB" before 1st December 2022 for each infected individual
most_recent_titre <- dt_clean[id %in% infected_ids & date < "2022-12-01" & titre_type == "Wildtype", 
                              .(most_recent_titre = last(titre)), by = id]

# Ensure the 'most_recent_titre' data.table has unique IDs.
most_recent_titre <- unique(most_recent_titre, by = "id")

# Get the infection status for each unique id
infection_status <- dt_clean[, .(infection_during_period = max(infection_during_period, na.rm = TRUE)), by = id]

# Merge the two datasets
final_dt <- most_recent_titre[infection_status, on = "id"]

# 3. Logistic regression using glm
model <- gam(infection_during_period ~ s(most_recent_titre), family = "binomial", data = dt_out)

# Print the model summary
summary(model)
