#' Helper function: split year
#'
#' @param year  year (e.g. 2020)
#' @param num_periods number of periods to separate the year
#'
#' @return data frame with columns "start" and "end"
#' @export
#' @examples
#' split_year(2024, 12)
#' split_year(2018, 6)
split_year <- function(year, num_periods) {

  stopifnot(num_periods %in% c(1:4,6,12))
  # Initialize vectors to store start and end dates of each period
  period_starts <- vector(mode = "list", length = num_periods)
  period_ends <- vector(mode = "list", length = num_periods)

  # Get current date
  current_date <- Sys.Date()

  # Loop through each period
  for (i in 1:num_periods) {
    # Calculate start date of the period
    if (i == 1) {
      period_start <- as.Date(paste(year, "-01-01", sep = ""))
    } else {
      period_start <- as.Date(paste(year, "-", sprintf("%02d", 1 + (12 / num_periods) * (i - 1)), "-01", sep = ""))
    }

    # Calculate end date of the period
    if (i < num_periods) {
      period_end <- as.Date(paste(year, "-", sprintf("%02d", 1 + (12 / num_periods) * i), "-01", sep = "")) - 1
    } else {
      period_end <- as.Date(paste(year + 1, "-01-01", sep = "")) - 1
    }

    # Check if start date is after current date
    if (period_start > current_date) {
      # Set both start and end dates to the current date
      period_start <- current_date
      period_end <- current_date
    }

    # Store start and end dates in vectors
    period_starts[[i]] <- as.character(period_start)
    period_ends[[i]] <- as.character(period_end)
  }

  # Combine start and end dates into a data frame
  periods <- data.frame(start = unlist(period_starts), end = unlist(period_ends))

  # Remove rows where both start and end dates are "current_date"
  periods <- periods[!(periods$start == current_date & periods$end == current_date), ]

  return(periods)
}
