# covariate_lookup_table <- function(
#     stan_data, covariate_formula) {
#   # Extract column names
#   col_names <- colnames(stan_data$X)
#
#   # Split column names based on the ':' delimiter
#   split_data <- str_split(col_names, ":", simplify = TRUE)
#
#   # Convert the matrix to a data.table
#   dt <- as.data.table(split_data)
#
#   # Extract category names from formula
#   formula_vars <- all.vars(covariate_formula)
#
#   # Set the new column names
#   setnames(dt, formula_vars)
#
#   # Create patterns to remove using the formula_vars
#   patterns_to_remove <- paste0("^", formula_vars, "[[:digit:]]*")
#
#   for (col_name in formula_vars) {
#     pattern_to_remove <- paste0("^", col_name)
#     dt[, (col_name) := str_remove_all(get(col_name), pattern_to_remove)]
#   }
#
#   # .I is a special symbol in data.table for row number
#   dt[, p := .I]
#   # Reorder columns to have 'i' first
#   setcolorder(dt, "p")
#
#   return(dt)
# }

covariate_lookup_table <- function(stan_data, covariate_formula) {
  # Extract column names
  col_names <- colnames(stan_data$X)

  # Split column names based on the ':' delimiter
  split_data <- str_split(col_names, ":", simplify = TRUE)

  # Convert the matrix to a data.table
  dt <- as.data.table(split_data)

  # Extract category names from formula
  formula_vars <- all.vars(covariate_formula)

  # Set the new column names
  setnames(dt, formula_vars)

  for (col_name in names(dt)) {
    # Find the matching formula variable for current column
    matching_formula_var <- formula_vars[which(startsWith(col_name, formula_vars))]
    if (length(matching_formula_var) > 0) {
      pattern_to_remove <- paste0("^", matching_formula_var)
      dt[, (col_name) := str_remove_all(get(col_name), pattern_to_remove)]
    }
  }

  # .I is a special symbol in data.table for row number
  dt[, p := .I]

  # Reorder columns to have 'i' first
  setcolorder(dt, "p")

  return(dt)
}

lm_wrapper <- function(dt_in, model_formula, t_max = 100) {
  # Run lm
  model <- lm(model_formula, data = dt_in)

  # Create a data frame for predictions
  dt_pred <- data.table(t_since_last_exp = seq(
    min(dt_in$t_since_last_exp), t_max,
    length.out = t_max * 5))

  # Predict outcome for new data
  model_out <- predict(
    model, newdata = dt_pred, interval = "confidence", level = 0.95)

  dt_out <- cbind(dt_pred, model_out)

  convert_log_scale_inverse(dt_out, vars_to_transform = c("fit", "lwr", "upr"))

  return(dt_out)
}

loess_wrapper <- function(dt_in, model_formula, t_max = 100) {
  # Run loess
  model <- loess(model_formula, data = dt_in)

  # Create a data frame for predictions
  dt_out <- data.table(t_since_last_exp = seq(
    min(dt_in$t_since_last_exp), t_max,
    length.out = t_max * 5))

  # Predict outcome for new data
  model_out <- predict(
    model, newdata = dt_out, se = TRUE)

  dt_out[, `:=` (
    fit = model_out$fit,
    lwr = model_out$fit - critical_value * model_out$se.fit,
    upr = model_out$fit + critical_value * model_out$se.fit)]

  convert_log_scale_inverse(dt_out, vars_to_transform = c("fit", "lwr", "upr"))

  return(dt_out)
}

lm_wrapper_dt <- function(dt, formula, points) {
  lm_wrapper(as.data.frame(dt), formula, points)
}

loess_wrapper_dt <- function(dt, formula, points) {
  loess_wrapper(as.data.frame(dt), formula, points)
}