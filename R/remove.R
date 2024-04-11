remove_missed_infections <- function(dt_in, time_threshold, titre_threshold) {
  dt_proc <- copy(dt_in)
  
  dt_proc <- dt_proc[order(id, titre_type, date)]
  
  # Calculate titre difference by id and titre_type
  dt_proc[, titre_diff := titre - shift(titre, type = "lag"),
          by = .(id, titre_type)]
  
  # Mark rows where a missed infection is detected
  dt_proc[, missed_infection := (
    titre_diff > titre_threshold & t_since_last_exp > time_threshold),
          by = .(id, titre_type)]
  
  # Propagate the missed infection flag forward for each individual
  dt_proc[, carry_over_missed := cummax(missed_infection),
          by = .(id, titre_type)]
  
  # Remove all subsequent observations for individuals with a missed infection
  dt_out <- dt_proc[carry_over_missed == 0]
  
  return(dt_out)
}
