convert_log_scale <- function(
    dt_in, vars_to_transform = "titre",
    simplify_limits = TRUE) {
  
  for(var in vars_to_transform) {
    if(simplify_limits == TRUE) {
      # dt_in[get(var) < 40, (var) := 40]
      dt_in[get(var) > 2560, (var) := 2560]
    }
    dt_in[, (var) := log2(get(var)/5)]
  }
  return(dt_in)
}

convert_log_scale_inverse <- function(
    dt_in, vars_to_transform = c("me", "lo", "hi")) {
  
  for(var in vars_to_transform) {
    # # Reverse the log2 transformation and multiplication by 5.
    # dt_in[, (var) := ifelse(
    #   get(var) <= 1, 5*2^(get(var)), 5*2^(get(var) + 1))]
    dt_in[, (var) := 5*2^(get(var))]
    
  } 
  return(dt_in)
}
