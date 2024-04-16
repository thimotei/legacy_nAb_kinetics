source_all_functions <- function() {
  # Listing all of the functions in the R folder
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

  # Sourcing all of the functions in the R folder
  invisible(lapply(r_files, source))
}
