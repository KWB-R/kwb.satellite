#' Helper function: create periods in year
#'
#' @param year year (e.g. 2020)
#' @param n_periods number of periods into which to separate the year
#' @return data frame with columns "start" and "end"
#' @keywords internal
#' @importFrom kwb.utils startsToEnds
#' @examples
#' create_periods_in_year(2018, n_periods = 4L)
create_periods_in_year <- function(year, n_periods)
{
  as_date <- function(x) as.Date(sprintf("%04d-%s", as.integer(year), x))
  from <- as_date("01-01")
  to <- as_date("12-31")

  starts <- seq(from, to, length.out = n_periods + 1L)
  ends <- kwb.utils::startsToEnds(starts, lastStop = to)

  data.frame(start = starts, end = ends)
}
