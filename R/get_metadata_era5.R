#' Copernicus: get metadata for ERA5
#'
#' @param grib_file path to GRIB file
#'
#' @return metadata
#' @export
#' @importFrom gdalUtilities gdalinfo
#' @importFrom tidyr pivot_wider
#' @importFrom gdata startsWith
#' @importFrom dplyr case_when mutate
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @seealso Code taken from https://gis.stackexchange.com/a/360652
#'
get_metadata_era5 <- function(grib_file) {

  ### Code copied from:
  ### https://gis.stackexchange.com/questions/360547/era5-grib-file-how-to-know-what-each-band-means

  info <- gdalUtilities::gdalinfo(grib_file)

  select_pattern <- paste0(c("Band",
                             "GRIB_COMMENT",
                             "GRIB_ELEMENT",
                             "GRIB_FORECAST_SECONDS",
                             "GRIB_REF_TIME",
                             "GRIB_VALID_TIME",
                             "GRIB_SHORT_NAME",
                             "GRIB_UNIT"),
                           collapse = "|")

  #Filter and retrieve the information of interest (as seen here).
  grib1 <- as.data.frame(info)
  grib1 <- as.data.frame(
    grib1[grep(pattern = select_pattern, x = info), ]
  )

  colnames(grib1) <- c('raw_')

  # Clean a little bit (with the help of this post, among others).
  raw_starts_with <- function(what) {
    gdata::startsWith(as.character(grib1[["raw_"]]), what, trim = TRUE)
  }

  is_band <- raw_starts_with('Band')
  is_grib_c <- raw_starts_with('GRIB_COMMENT')
  is_grib_e <- raw_starts_with('GRIB_ELEMENT')
  is_grib_f <- raw_starts_with('GRIB_FORECAST_SECONDS')
  is_grib_r <- raw_starts_with('GRIB_REF_TIME')
  is_grib_v <- raw_starts_with('GRIB_VALID_TIME')
  is_grib_s <- raw_starts_with('GRIB_SHORT_NAME')
  is_grib_u <- raw_starts_with('GRIB_UNIT')

  grib1 <- grib1 %>%
    dplyr::mutate(
      content = dplyr::case_when(
        is_band ~ gsub(".*Band (.+) Block.*", "\\1", raw_),
        is_grib_c ~ sub(".*=", "", raw_),
        is_grib_e ~ sub(".*=", "", raw_),
        is_grib_f ~ gsub(".*=(.+) sec.*", "\\1", raw_),
        is_grib_r ~ gsub(".*= (.+) sec.*", "\\1", raw_),
        is_grib_v ~ gsub(".*= (.+) sec.*", "\\1", raw_),
        is_grib_s ~ sub(".*=", "", raw_),
        is_grib_u ~ sub(".*=", "", raw_) %>%
                    stringr::str_remove("\\[") %>%
                    stringr::str_remove("\\]")
      ),
      column = dplyr::case_when(
        is_band ~ 'band',
        is_grib_c ~ 'variable_unit',
        is_grib_e ~ 'variable_short',
        is_grib_f ~ 'forecast_seconds',
        is_grib_r ~ 'time_ref',
        is_grib_v ~ 'time_valid',
        is_grib_s ~ 'short_name',
        is_grib_u ~ 'unit'
      )
    )

  grib1 <- grib1[, -1]

  #Variables to columns and time readable (as seen here).

  n_variables <- as.integer(length(unique(grib1$column)))

  max_id <- as.integer(nrow(grib1) / n_variables)

  grib1$id <- sort(rep(seq_len(max_id), n_variables))

  grib1 <- grib1 %>%  tidyr::pivot_wider(
    names_from = "column",
    values_from = "content"
  ) %>%
    dplyr::mutate(
      time_ref = as.POSIXct(as.numeric(.data$time_ref), origin = "1970-01-01", tz = "UTC"),
      time_valid = as.POSIXct(as.numeric(.data$time_valid), origin = "1970-01-01", tz = "UTC"),
      time_forecast = .data$time_ref + as.numeric(.data$forecast_seconds),
      variable = stringr::str_remove(.data$variable_unit,
                                     pattern = "\\s+\\[.*\\]$"))

  grib1 <- grib1[, -1]

  grib1
}
