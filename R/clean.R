recover_covariate_names <- function(
    dt_in, dt_stan_data, stan_data, formula_val) {
  
  dt_proc <- copy(dt_in)
  
  dt_titre_lookup <- data.table(
    k = 1:dt_stan_data[, length(unique(titre_type))], 
    titre_type = dt_stan_data[, unique(titre_type)])
  
  dt_covariate_lookup <- covariate_lookup_table(
    stan_data, formula_val)
  
  dt_out <- dt_proc[
    dt_covariate_lookup, on = "p"][
    dt_titre_lookup, on = "k"]

  return(dt_out)
}

recover_covariate_names_ind <- function(
    dt_samples, dt_stan_data, formula){
  
  all_formula_variables <- all.vars(formula)
  dt_design_matrix <- dt_stan_data[
    , .SD, .SDcols = all_formula_variables, by = .(id = stan_id)] |> 
    unique()
  
  dt_titre_lookup <- data.table(
    titre_type = 1:dt_stan_data[, length(unique(titre_type))], 
    titre_type_new = dt_stan_data[, unique(titre_type)])
  
  dt_out <- dt_samples[
    dt_design_matrix, on = "id"][
    dt_titre_lookup, on = "titre_type"]
  
  dt_out[, titre_type := NULL]
  
  setnames(dt_out, "titre_type_new", "titre_type")
  
  return(dt_out)
}

clean_covariate_names <- function(
    dt_in, formula_val, 
    cleaned_names = c("Infection history",
                      "Last exposure", 
                      "Titre type")) {
  
  dt_out <- setnames(
    copy(dt_in),
    c(all.vars(formula_val), "titre_type"),
    cleaned_names)
  
  return(dt_out)
}
