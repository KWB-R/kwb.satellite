# is_this_year -----------------------------------------------------------------
is_this_year <- function(year)
{
  stopifnot(is.numeric(year), length(year) == 1L)
  as.integer(year) == this_year()
}

# this_year --------------------------------------------------------------------
this_year <- function()
{
  as.integer(format(Sys.Date(), format = "%Y"))
}
