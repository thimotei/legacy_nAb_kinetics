simplify_voc_names <- function(dt_in, condense_waves = FALSE) {
  
  dt_out <- copy(dt_in)
  
  # simplifying infection exposure categories
  
  dt_out[, last_inf_date_me := median(last_inf_date), by = last_inf_type]
  
  #--- Manually renaming the VOC names
  
  vocs_to_remove <- c("B", "B.1.1.529", "Unassigned")
  
  # Removing small categories
  dt_out <- dt_out[!last_inf_type %in% vocs_to_remove]
  
  # Consolidating antigenically similar variants
  dt_out[last_inf_type == "Omicron (XBB-like)", last_inf_type := "XBB"]
  dt_out[last_inf_type == "Omicron (XBB.1-like)", last_inf_type := "XBB"]
  dt_out[last_inf_type == "Omicron (XBB.1.16-like)", last_inf_type := "XBB"]
  dt_out[last_inf_type == "Omicron (XBB.1.5-like)", last_inf_type := "XBB"]
  dt_out[last_inf_type == "Omicron-BA.4", last_inf_type := "BA.5"]
  dt_out[last_inf_type == "Omicron-BA.4/5", last_inf_type := "BA.5"]
  
  # Simplifying names
  dt_out[last_inf_type == "Omicron-BA.1", last_inf_type := "BA.1"]
  dt_out[last_inf_type == "Omicron-BA.2", last_inf_type := "BA.2"]
  dt_out[last_inf_type == "Omicron-BA.5", last_inf_type := "BA.5"]
  dt_out[last_inf_type == "unknown: Alpha_Delta", last_inf_type := "Alpha/Delta"]
  dt_out[last_inf_type == "unknown: D614G_Alpha", last_inf_type := "D614G/Alpha"]
  dt_out[last_inf_type == "unknown: Delta_Omicron-BA.1", last_inf_type := "Delta/BA.1"]
  dt_out[last_inf_type == "unknown: Omicron-BA.1_Omicron-BA.2", last_inf_type := "BA.1/BA.2"]
  dt_out[last_inf_type == "unknown: Omicron-BA.2_Omicron-BA.4/5", last_inf_type := "BA.2/BA.5"]
  dt_out[last_inf_type == "unknown: Omicron-BA.5_BQ.1.1_CH.1.1_XBB.1.5", last_inf_type := "BQ.1.1/XBB"]
  
  
  if(condense_waves == TRUE) {
    
    dt_out[last_inf_type %like% "/", last_inf_type := {
      splits <- unlist(strsplit(as.character(last_inf_type[1]), "/")) # Split the string by '/'
      ifelse(last_inf_date <= last_inf_date_me, splits[1], splits[2])
    }, by = last_inf_type]
    
  }
  
  # removing unused factors after renaming
  dt_out[, last_inf_type := factor(last_inf_type)]
  dt_out[, last_inf_type := fct_drop(last_inf_type)]
  
  # manually reclassifying some infection events. Ed mentioned this on the call
  # last week, that some infections had been misattributed. Check this is now 
  # correct
  dt_out[last_inf_date >= "2023-01-01" & last_inf_type == "BA.2", last_inf_type := "BQ.1.1"]
  
  dt_out[, last_inf_date_me := median(last_inf_date), by = last_exp_type]
  
  # Removing small categories
  dt_out <- dt_out[!last_exp_type %in% vocs_to_remove]
  
  # Consolidating antigenically similar variants
  dt_out[last_exp_type == "Omicron (XBB-like)", last_exp_type := "XBB"]
  dt_out[last_exp_type == "Omicron (XBB.1-like)", last_exp_type := "XBB"]
  dt_out[last_exp_type == "Omicron (XBB.1.16-like)", last_exp_type := "XBB"]
  dt_out[last_exp_type == "Omicron (XBB.1.5-like)", last_exp_type := "XBB"]
  dt_out[last_exp_type == "Omicron-BA.4", last_exp_type := "BA.5"]
  dt_out[last_exp_type == "Omicron-BA.4/5", last_exp_type := "BA.5"]
    
  # Simplifying names
  dt_out[last_exp_type == "Omicron-BA.1", last_exp_type := "BA.1"]
  dt_out[last_exp_type == "Omicron-BA.2", last_exp_type := "BA.2"]
  dt_out[last_exp_type == "Omicron-BA.5", last_exp_type := "BA.5"]
  dt_out[last_exp_type == "unknown: Alpha_Delta", last_exp_type := "Alpha/Delta"]
  dt_out[last_exp_type == "unknown: D614G_Alpha", last_exp_type := "D614G/Alpha"]
  dt_out[last_exp_type == "unknown: Delta_Omicron-BA.1", last_exp_type := "Delta/BA.1"]
  dt_out[last_exp_type == "unknown: Omicron-BA.1_Omicron-BA.2", last_exp_type := "BA.1/BA.2"]
  dt_out[last_exp_type == "unknown: Omicron-BA.2_Omicron-BA.4/5", last_exp_type := "BA.2/BA.5"]
  dt_out[last_exp_type == "unknown: Omicron-BA.5_BQ.1.1_CH.1.1_XBB.1.5", last_exp_type := "BQ.1.1/XBB"]
  
  dt_out[last_exp_type == "BNT162b2+BA1", last_exp_type := "BNT162b2+BA.1"]
  dt_out[last_exp_type == "BNT162b2+BA4/5", last_exp_type := "BNT162b2+BA.5"]
  
  if(condense_waves == TRUE) {
    
    dt_out[last_exp_type %like% "/", last_exp_type := {
      splits <- unlist(strsplit(as.character(last_exp_type[1]), "/")) # Split the string by '/'
      ifelse(last_inf_date <= last_inf_date_me, splits[1], splits[2])
    }, by = last_exp_type]
    
  }  
  # removing unused factors after renaming
  dt_out[, last_exp_type := fct_drop(last_exp_type)]
  dt_out[, last_exp_type := factor(last_exp_type)]
  
  
  # manually reclassifying some infection events. Ed mentioned this on the call
  # last week, that some infections had been mis-attributed. Check this is now 
  # correct
  dt_out[last_inf_date >= "2023-01-01" & last_exp_type == "BA.2", last_exp_type := "BQ.1.1"]
  
  return(dt_out)
}
