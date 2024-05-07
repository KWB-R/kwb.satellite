
get_results <- function(sat_list, data_type = "data") {

setNames(lapply(names(sat_list), function(x) sat_list[[x]][[data_type]]),
                nm = names(sat_list)) %>%
  dplyr::bind_rows(.id = "id") %>%
  tidyr::separate("id",
                  into = c("spatial_type", "spatial_agg_func", "year"),
                  sep = "_")

}


dat <- get_results(sat_list = lakes_01_centroid)
View(dat)

dat_dat <- tidyr::unnest(dat, cols = satellite_data)

dat_meta <- dplyr::bind_cols(dat[1,]$satellite_data, dat[1,]$satellite_metadata)
View(dat_meta)



convert_to_list <- function(coords) {

tmp_mat <- lapply(seq_along(coords),
                            FUN = function(idx) {

  t(as.matrix(coords[[idx]]))
})

list(do.call(rbind, tmp_mat))
}

tmp <- dat[1, ]$satellite_metadata[[1]][[84]][[1]]

coords_list <- convert_to_list(tmp$`system:footprint`$coordinates)
sat_meta <- sf::st_polygon(coords_list)



sat_data_list <- kwb.satellite::import_rds(rds_dir = "C:/Users/mrustl/AppData/Local/Temp/Rtmpmsvbqy")
sat_data <- dplyr::bind_rows(sat_data_list)
View(sat_data)

sat_data_unnest1 <- tidyr::unnest(sat_data, satellite_data)
sat_data_unnest2 <- tidyr::unnest(sat_data, c(satellite_data, satellite_metadata),
                                  names_sep = ".")


archive::archive_extract("https://data.geobasis-bb.de/geofachdaten/Wasser/Hydrologie/seen25.zip",
                         dir = "lakes_bb")
lakes_bb <- sf::read_sf("lakes_bb/Seen25_20211105/seen25.shp")

sat_data_unnest2_meta <- dplyr::left_join(sat_data_unnest2,
                 lakes_bb[,c("SEE_KZ", "geometry")] %>%
                   dplyr::rename(geometry_bb = geometry) %>%
                   dplyr::mutate(SEE_KZ = as.double(SEE_KZ)) %>%
                   as.data.frame(),
                 by = "SEE_KZ")

seq_images <- seq_along(sat_data_unnest2_meta$satellite_metadata.geometry)

sat_multi_coors <- lapply(seq_images, function(i) {
  convert_to_list(
  sat_data_unnest2_meta$satellite_metadata.geometry[[i]]$`system:footprint`$coordinates
  ) %>% sf::st_polygon()
  })

View(sat_multi_coors)

for(i in seq_images) {
sat_multi_coors[[i]] %>%
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(
    color = "red"
  ) %>%
  leaflet::addCircles(color = "blue",
                       data = sat_data_unnest2_meta$satellite_data.geometry_filter$geometry[[i]]) %>%
  leaflet::addPolygons(color = "green",
                       data = sf::st_transform(sat_data_unnest2_meta$geometry_bb[i], crs = 4326)) %>%
  print()
  kwb.base::hsWait(0.5)
}


meta <- get_results(sat_list = lakes_01_centroid, data_type = "metadata")


lakes_bb[lakes_bb$SEE_NAME == "Plötzensee",] %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons()

lakes_bb[stringr::str_detect(lakes_bb$SEE_NAME, ".*Wummsee.*"),]%>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons()

lakes_bb[stringr::str_detect(lakes_bb$SEE_NAME, ".*Beetzsee.*"),] %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons()

lakes_bb[stringr::str_detect(lakes_bb$SEE_NAME, ".*Stechlinsee.*"),] %>%
  sf::st_transform(4326) %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons()
