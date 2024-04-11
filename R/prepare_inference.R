extract_subjects <- function(dt) {
  subjects <- data.table::copy(dt)
  subjects <- subjects[, .SD[1,], by = "id"]
  
  return(subjects[])
}

prepare_stan_data <- function(dt) {
  
  dt_out <- copy(dt)
  
  # dt_out[titre == 9, titre := 8]
  dt_out[titre == 1, titre := 0]
  
  # uncensored values at zero - might change this later on
  dt_out[titre == 0, censored := -2]
  
  # censored between 1 and 3 - REMOVED THIS FOR NOW
  # dt_out[titre == 1, censored := -1]
  
  # uncensored values between 3 and 9
  dt_out[titre > 3 & titre < 9, censored := 0]
  
  # censored values above 9
  dt_out[titre == 9, censored := 1]
  
  dt_out[, censored := as.integer(censored)]
  
  # Assign original stan_id based on id and exp_num
  dt_out[, stan_id := .GRP, by = .(id, exp_num)]
  
  # Create obs_id based on censored and stan_id
  dt_out[, obs_id := .I]
  
  # Order by obs_id and then other relevant columns
  setorder(dt_out, obs_id, date, titre_type)
  
  dt_out[, titre_type_num := .GRP, by = titre_type]
  
  return(dt_out)
}

retrieve_stan_data <- function(
    dt, time_type = "relative", formula,
    prior_mu_values, prior_sigma_values,
    default_prior_values = TRUE, 
    log_parameter_scale = TRUE,
    preds_sd = 0.25) {
  
  stan_data <- list(
    N = dt[, .N],
    # N_ind = dt[, uniqueN(id)],
    N_events = dt[, uniqueN(stan_id)],
    id = dt[, stan_id],
    titre = dt[, titre],
    censored = dt[, censored],
    titre_type = dt[, titre_type_num],
    preds_sd = preds_sd,
    K = dt[, uniqueN(titre_type)],
    N_uncens = dt[censored == 0, .N],
    N_lo = dt[censored == -2, .N],
    N_me = dt[censored == -1, .N],
    N_hi = dt[censored == 1, .N],
    uncens_idx = dt[censored == 0, obs_id],
    cens_lo_idx = dt[censored == -2, obs_id],
    cens_me_idx = dt[censored == -1, obs_id],
    cens_hi_idx = dt[censored == 1, obs_id])
  
  if(time_type == "relative") {
    stan_data$t <- dt[, t_since_last_exp]
  } else if (time_type == "absolute") {
    stan_data$t <- dt[, t_since_min_date]
  }
  
  # adding design matrix to Stan data list
  all_formula_variables <- all.vars(formula)
  dt_design_matrix <- dt[
    , .SD, .SDcols = all_formula_variables, by = stan_id] |> 
    unique()
  
  X <- construct_design_matrix(formula, data = dt_design_matrix)
  stan_data$X <- X
  stan_data$P <- ncol(X)
  # stan_data$formula <- formula
  
  # prior values
  # initial titre value
  # stan_data$mu_t0 <- 4.0
  # stan_data$sigma_t0 <- 2.0
  # 
  # # timing of peak
  # stan_data$mu_tp <- 10
  # stan_data$sigma_tp <- 2
  # 
  # # timing of switch
  # stan_data$mu_ts <- 60
  # stan_data$sigma_ts <- 3
  # 
  # # gradient of boost
  # stan_data$mu_m1 <- 0.25
  # stan_data$sigma_m1 <- 0.01
  # 
  # # gradient of 1st wane
  # stan_data$mu_m2 <- -0.02
  # stan_data$sigma_m2 <- 0.01
  # 
  # # gradient of 2nd wane
  # stan_data$mu_m3 <- 0
  # stan_data$sigma_m3 <- 0.01
  
  # Adding default prior values to Stan data list
  if(default_prior_values == TRUE) {
    stan_data <- c(
    stan_data, create_list_priors(
      set_prior_values(log_scale = log_parameter_scale)))
  } else {
    stan_data <- c(
      stan_data, 
      create_list_priors(
        set_prior_values(
          sigma_values = prior_sigma_values)))
  }
  
  return(stan_data)
}

model_matrix_with_dummy <- function(formula, data) {
  
  # Identify columns that are factors with one level
  single_level_factors <- sapply(data, function(col) {
    is.factor(col) && length(levels(col)) == 1
  })
  
  # If any such columns are found, add a dummy level and row
  if (any(single_level_factors)) {
    dummy_row <- data[1, , drop = FALSE] # Create a dummy row based on the first row of data
    for (colname in names(single_level_factors)[single_level_factors]) {
      dummy_level <- paste0(levels(data[[colname]])[1], "_dummy")
      levels(data[[colname]]) <- c(levels(data[[colname]]), dummy_level)
      dummy_row[[colname]] <- dummy_level
    }
    data <- rbind(data, dummy_row) # Append the dummy row to data
  }
  
  # Compute the model matrix
  mm <- model.matrix(formula, data)
  
  # If dummy row was added, remove the corresponding row from the model matrix
  if (any(single_level_factors)) {
    mm <- mm[-nrow(mm), , drop = FALSE]
  }
  
  return(mm)
}

construct_design_matrix <- function(formula, data, ...) {
  
  # Build the full design matrix using model.matrix
  mm <- model_matrix_with_dummy(formula, data = data, ...)
  
  # Identify columns with no variance and remove them
  variance_per_column <- apply(mm, 2, var)
  relevant_columns <- which(variance_per_column != 0)
  mm_reduced <- mm[, relevant_columns]
  
  return(mm_reduced)
}

# Function which accepts arbitrary parameter names and mu and sigma values
# to flexibly set prior values, to be used for inference and prior predictive
# functions
set_prior_values <- function(
    names = c("t0", "tp", "ts", "m1", "m2", "m3"),
    mu_values = c(4.0, 10, 60, 0.25, -0.02, 0),
    sigma_values = c(2.0, 2.0, 3.0, 0.01, 0.01, 0.01),
    log_scale = TRUE) {
  
  # Check if the lengths of all vectors match
  if (length(names) != length(mu_values) || 
      length(names) != length(sigma_values)) {
    stop("The lengths of the vectors do not match.")
  }
  
  # if (log_scale) {
  #   # Apply log transformation to mu_values, excluding negative or zero values
  #   mu_values <- ifelse(mu_values <= 0, mu_values, log(mu_values))
  #   # Apply log transformation to sigma_values, but keep zeros as is
  #   sigma_values <- ifelse(sigma_values == 0, 0, log(sigma_values))
  # }
  # Construct vectors for columns of output data.table
  names_proc <- rep(names, times = 2)
  types <- rep(c("mu", "sigma"), each = length(names))
  values <- c(mu_values, sigma_values)
  
  # Creating the data.table
  dt_out <- data.table(
    name = names_proc, 
    type = types,
    value = values)
  
  return(dt_out)
}

# Function which accepts a data.table and outputs a named list, used to 
# transform output of set_prior_values() function and return a list for Stan
create_list_priors <- function(dt_in) {
  
  # Combine parameter_type and parameter_name to create the list names
  combined_names <- paste0(dt_in$type, "_", dt_in$name)
  
  # Create the list with names and values
  stan_data <- setNames(as.list(dt_in$value), combined_names)
  
  return(stan_data)
}
