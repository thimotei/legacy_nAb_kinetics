create_time_series <- function(dt_in, date_column = "dates", group_column = NULL) {
  # Ensure the input is a data.table
  dt_in <- as.data.table(dt_in)
  
  # Validate the date column exists
  if (!date_column %in% names(dt_in)) {
    stop("The specified date column does not exist in the data table.")
  }
  
  # Validate the group column exists if provided
  if (!is.null(group_column) && !group_column %in% names(dt_in)) {
    stop("The specified group column does not exist in the data table.")
  }
  
  # Set up the 'by' clause correctly
  by_clause = if (is.null(group_column)) {
    date_column
  } else {
    c(date_column, group_column)
  }
  
  # Count the events per day, possibly by group
  daily_count <- dt_in[, .(count = .N), by = by_clause]
  
  # Adjust names for clarity and subsequent operations
  setnames(daily_count, date_column, "date")
  
  # Generate a sequence of dates from the first to the last
  date_range <- seq(min(daily_count$date, na.rm = TRUE), max(daily_count$date, na.rm = TRUE), by = "day")
  
  # Create a data table for all dates in the range
  all_dates <- data.table(date = date_range)
  
  # If grouping, expand all_dates to include all group combinations
  if (!is.null(group_column)) {
    unique_groups <- unique(dt_in[[group_column]])
    all_dates <- CJ(date = date_range, group = unique_groups)
    setnames(all_dates, "group", group_column)
  }
  
  # Define merge columns for consistency
  merge_columns = c("date")
  if (!is.null(group_column)) {
    merge_columns <- c("date", group_column)
  }
  
  # Merge daily counts with all dates, filling missing days with zeros
  time_series <- merge(all_dates, daily_count, by = merge_columns, all.x = TRUE)
  time_series[is.na(time_series$count), count := 0]
  
  total_events <- sum(time_series$count)
  # Compute cumulative counts and proportions
  if (!is.null(group_column)) {
    time_series[, cumulative := cumsum(count), by = group_column]
    time_series[, `:=` (total_events = sum(count), proportion = cumulative / total_events), by = group_column]
    setnames(time_series, group_column, "group")  # Rename the group column to 'group' before returning
  } else {
    time_series[, cumulative := cumsum(count)]
    time_series[, proportion := cumulative / total_events]
  }
  
  return(time_series)
}
