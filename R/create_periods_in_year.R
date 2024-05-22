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

  as_date <- function(x) as.Date(sprintf("%04d-%s", as.integer(year), x))

  n_dates <- n_periods + 1L
  current_date <- Sys.Date()

  end_date <-  if(as.integer(year) != as.integer(format(Sys.Date(), format = "%Y"))) {
     "12-31"
  } else {
    format(current_date, format = "%m-%d")
  }

  dates <- seq.Date(as_date("01-01"), as_date(end_date), length.out = n_dates)

  starts <- dates[-n_dates]

  data.frame(
    start = starts,
    end = kwb.utils::startsToEnds(starts, lastStop = dates[n_dates])
  )

}
