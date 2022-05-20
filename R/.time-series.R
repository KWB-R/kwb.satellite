library(kwb.satellite)

grib_file <- "reanalysis-era5-single-levels_total_precipitation_2010-2020.grib"

meta <- kwb.satellite::get_metadata_era5(grib_file)

preci_stars <- stars::read_stars(grib_file)
names(preci_stars) <- "era5_grib"


preci_stars_cube <- cubelyr::as.tbl_cube(preci_stars)
str(preci_stars_cube$dims)

nbands <- length(preci_stars_cube$dims$band)
has_data <- sapply(seq_len(nbands), function(band) {sum(preci_stars_cube$mets[[1]][,,band])>0})
max_data <- sapply(seq_len(nbands), function(band) {max(preci_stars_cube$mets[[1]][,,band])})

bands <- which(has_data)[1:100]

val_max <- ceiling(max(max_data)*1000)

max_col <- 5

cols <- data.frame(breaks = c(0, seq_len(max_col)*val_max/max_col),
                   col_name = heat.colors(max_col))

animation::saveGIF(expr = for(band in bands) {
label <- sprintf("%s [mm] (%s UTC)", meta$variable[band], meta$time_forecast[band])
preci_stars %>%
  dplyr::slice(index = band, along = "band") %>%
  ### Convert units from [m] to [mm]
  dplyr::mutate(era5_grib = era5_grib * 1000) %>%
  raster::plot(zlim=c(0,max(max_data)*1000), main = label)
},
movie.name = "era5-precipitation.gif",
interval = 0.1
)

preci_stars_points <- sf::st_as_sf(preci_stars, as_points = TRUE, merge = FALSE)


#summary <- rgdal::GDALinfo(grib_file, silent = TRUE)

# preci_sf <- sf::st_read(grib_file)
# preci <- rgdal::readGDAL(grib_file)
# head(preci@data)
# class(preci)
# raster::image(preci, zlim = c(0, 0.01))
# ggplot2::ggplot(preci)
# image(preci)
# preci_df <- as(preci, "SpatialGridDataFrame")
#
# preci@bbox
#
# plot(preci$band1)
