#--- Isolate and save exposure data

# Isolate infection data
dt_inf <- dt_clean[
  , .(inf_num, last_inf_type, last_inf_date)] |>
  unique()

# Isolate vaccine data
dt_vax <- dt_clean[
  !is.na(last_vax_type)][
    last_vax_type != "others"][
      , .(vax_num, last_vax_type, last_vax_date)][
        last_vax_date < ymd("2023-10-22")] |>
  unique()

fwrite(dt_inf, "data/infections.rds")
fwrite(dt_vax, "data/vaccines.rds")
