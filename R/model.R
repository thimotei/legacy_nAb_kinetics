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
