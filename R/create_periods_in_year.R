#' Helper function: create periods in year
#'
#' @param year year (e.g. 2020)
#' @param n_periods number of periods into which to separate the year.
#'   Default: 4L
#' @return data frame with columns "start" and "end"
#' @keywords internal
#' @importFrom kwb.utils startsToEnds
#' @examples
#' create_periods_in_year(2018, n_periods = 4L)
create_periods_in_year <- function(year, n_periods = 4L)
{
  stopifnot(length(n_periods) == 1L, n_periods > 0L)
  stopifnot(is.numeric(year), length(year) == 1L)

  year <- as.integer(year)

  as_date <- function(x) as.Date(sprintf("%04d-%s", year, x))
  n_dates <- n_periods + 1L

  today <- Sys.Date()
  this_year <- as.integer(format(today, format = "%Y"))

  dates <- seq.Date(
    from = as_date("01-01"),
    to = ifelse(year == this_year, today, as_date("12-31")),
    length.out = n_dates
  )

  starts <- dates[-n_dates]
  ends <- kwb.utils::startsToEnds(starts, lastStop = dates[n_dates])

  data.frame(
    start = as.character(starts),
    end = as.character(ends)
  )
}
