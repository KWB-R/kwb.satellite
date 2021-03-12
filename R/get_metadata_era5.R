#' Copernicus: get metadata for ERA5
#'
#' @param grib_file path to GRIB file
#'
#' @return metadata
#' @export
#' @importFrom gdalUtils gdalinfo
#' @importFrom maditr dcast
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
grib1 <- as.data.frame(grib1[grep(pattern = 'Band|GRIB_COMMENT|GRIB_REF_TIME',
                                  x = info),])
colnames(grib1) <- c('raw_')

#Clean a little bit (with the help of this post, among others).

grib1 <- grib1 %>%
  dplyr::mutate(content = dplyr::case_when(
    gdata::startsWith(as.character(raw_),'Band', trim=TRUE) ~ gsub(".*Band (.+) Block.*", "\\1",raw_),
    gdata::startsWith(as.character(raw_),'GRIB_C', trim=TRUE) ~ sub(".*=", "", raw_),
    gdata::startsWith(as.character(raw_),'GRIB_R', trim=TRUE) ~ gsub(".*= (.+) sec.*", "\\1",raw_)
  )
  )

grib1 <- grib1 %>%
  dplyr::mutate(column = dplyr::case_when(
    gdata::startsWith(as.character(raw_),'Band', trim=TRUE) ~ 'Band',
    gdata::startsWith(as.character(raw_),'GRIB_C', trim=TRUE) ~ 'Variable',
    gdata::startsWith	(as.character(raw_),'GRIB_R', trim=TRUE) ~ 'Time'
  )
  )
grib1<-grib1[,-1]

#Variables to columns and time readable (as seen here).

grib1$id <- sort(rep(1:72,3))
grib1 <- maditr::dcast(grib1, formula = id ~ column, value.var='content')
grib1 <- grib1 %>% dplyr::mutate(
  Time = as.POSIXct(as.numeric(as.character("Time")), origin="1970-01-01")
)
grib1 <- grib1[,-1]

grib1
}
