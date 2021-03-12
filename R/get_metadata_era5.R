#' Copernicus: get metadata for ERA5
#'
#' @param grib_file path to GRIB file
#'
#' @return metadata
#' @export
#' @importFrom gdalUtils gdalinfo
#' @importFrom tidyr pivot_wider
#' @importFrom gdata startsWith
#' @importFrom dplyr case_when mutate
#' @seealso Code taken from https://gis.stackexchange.com/a/360652
#'
get_metadata_era5 <- function(grib_file) {

  ### Code copied from:
  ### https://gis.stackexchange.com/questions/360547/era5-grib-file-how-to-know-what-each-band-means

  info <- gdalUtils::gdalinfo(grib_file)

  #Filter and retrieve the information of interest (as seen here).
  grib1 <- as.data.frame(info)
  grib1 <- as.data.frame(
    grib1[grep(pattern = 'Band|GRIB_COMMENT|GRIB_REF_TIME', x = info), ]
  )

  colnames(grib1) <- c('raw_')

  # Clean a little bit (with the help of this post, among others).
  raw_starts_with <- function(what) {
    gdata::startsWith(as.character(grib1[["raw_"]]), what, trim = TRUE)
  }

  grib1 <- grib1 %>%
    dplyr::mutate(
      content = dplyr::case_when(
        raw_starts_with('Band') ~ gsub(".*Band (.+) Block.*", "\\1", raw_),
        raw_starts_with('GRIB_C') ~ sub(".*=", "", raw_),
        raw_starts_with('GRIB_R') ~ gsub(".*= (.+) sec.*", "\\1", raw_)
      ),
      column = dplyr::case_when(
        raw_starts_with('Band') ~ 'Band',
        raw_starts_with('GRIB_C') ~ 'Variable',
        raw_starts_with('GRIB_R') ~ 'Time'
      )
    )

  grib1 <- grib1[, -1]

  #Variables to columns and time readable (as seen here).

  max_id <- nrow(grib1) / 3

  grib1$id <- sort(rep(seq_len(max_id), 3L))

  grib1 <- grib1 %>%  tidyr::pivot_wider(
    names_from = "column",
    values_from = "content"
  ) %>%
    dplyr::mutate(
      Time = as.POSIXct(as.numeric(Time), origin = "1970-01-01")
    )

  grib1 <- grib1[, -1]

  grib1
}
