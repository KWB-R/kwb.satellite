#' Helper function: create periods in year
#'
#' @param year year (e.g. 2020)
#' @param n_year_splits number of periods to separate the year
#' @return data frame with columns "start" and "end"
#' @keywords internal
#' @examples
#' create_periods_in_year(2018, 4L)

create_periods_in_year <- function(year, n_year_splits) {
  year <- as.integer(year)

  as_date <- function(year, day_string) {
    as.Date(sprintf("%04d-%s", year, day_string))
  }

  starts <- seq(
    from = as_date(year, "01-01"),
    to = as_date(year, "12-31"),
    length.out = n_year_splits + 1L
  )

  ends <- kwb.utils::startsToEnds(
    starts = starts,
    lastStop = as_date(year, "12-31")
  )

  data.frame(start = starts, end = ends)
}

