
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


dirs <- fs::dir_ls("test",
                   type = "directory")

dirs <- dirs[2]

lake <- tmp$`Großer Baalsee_point_mean_2017-2024`
year <- 2020

tmp <- kwb.satellite::import_rds(dirs)
tmp_flatten <- lapply(seq_along(tmp),
                      function(i) {
                        print(i)
                        kwb.satellite::flatten_results(tmp[[i]])})


sat_meta_list <- stats::setNames(lapply(dirs, ),
                                 basename(dirs)
                                 )


import_and_flatten <- function(dir) {

kwb.satellite::import_rds(dir) %>%
  kwb.satellite::flatten_results()
}


tmp <- import_and_flatten("vignettes/gee/lakes_bb_selected_polygon/")
tmp_flatten <- lapply(seq_along(length(tmp)), function(i) try(flatten_results[[i]]))
is_try_error <- lapply(seq_along(tmp_flatten), function (i) inherits(tmp_flatten[[i]],'try-error'))
tmp_flatten_error <- tmp[which(unlist(is_try_error))]


lapply(1:nrow(tmp_flatten_error$`Senftenberger See_point_mean_2017-2024`),
       function(i) {
         nrow(tmp_flatten_error$`Senftenberger See_point_mean_2017-2024`[i,]$satellite_data[[1]]) -
         nrow(tmp_flatten_error$`Senftenberger See_point_mean_2017-2024`[i,]$satellite_metadata[[1]])
       }
       )


tmp$`Großer Beutelsee_point_mean_2017-2024`[1,]$satellite_metadata[[1]] %>%
  tidyr::separate(col = "id", into = c("provider_name", "provider_collection", "id_short"),
                  sep = "/",
                  remove = FALSE) %>%
  tidyr::separate(
    col = "id_short",
    into = c("datetime_start",
             "datetime_end",
             "tile_id"),
    sep = "_"
  ) %>%
  dplyr::mutate(
    datetime_start = lubridate::ymd_hms(datetime_start),
    datetime_end = lubridate::ymd_hms(datetime_end)
  ) %>%
  dplyr::select(- tidyselect::all_of(c("provider_name","provider_collection")))


dirs <- fs::dir_ls("vignettes/gee/lakes_bb_point_on_surface/", type = "directory")

tmp <- lapply(dirs, function(dir) try(import_and_flatten(dir)))

archive::archive_extract("https://data.geobasis-bb.de/geofachdaten/Wasser/Hydrologie/seen25.zip",
                         dir = "lakes_bb")
lakes_bb <- sf::read_sf("lakes_bb/Seen25_20211105/seen25.shp")

sat_meta_unnest <- dplyr::left_join(sat_meta_unnest,
                 lakes_bb[,c("SEE_KZ", "geometry")] %>%
                   dplyr::rename(geometry_bb = geometry) %>%
                   dplyr::mutate(SEE_KZ = as.double(SEE_KZ)) %>%
                   as.data.frame(),
                 by = "SEE_KZ")

sat_meta_unnest$geometry_bb_point_on_surface <- sf::st_point_on_surface(sat_meta_unnest$geometry_bb) %>%
  sf::st_transform(4326)

sat_meta_unnest$geometry_bb <- sf::st_transform(sat_meta_unnest$geometry_bb,
                                                crs = 4326)

sat_meta_unnest$geometry_bb_centroid <- sf::st_centroid(sat_meta_unnest$geometry_bb)

seq_images <- seq_along(sat_meta_unnest$satellite_metadata.geometry)

see_name <- unique(sat_meta_unnest$SEE_NAME)[order(unique(sat_meta_unnest$SEE_NAME))]

csv_path <- system.file("extdata/lakes_bb_malte.csv", package = "kwb.satellite")

lakes_malte <- readr::read_csv(csv_path) %>%
  sf::st_as_sf(coords = c("long", "lat"),  crs = 4326)

# for(i in seq_images) {
for(see in see_name) {
dat <- sat_meta_unnest[sat_meta_unnest$SEE_NAME == see,] %>%
  dplyr::first()

see_malte <- lakes_malte$geometry[lakes_malte$SEE_NAME == see]

i <- 1

dat %>%
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addPolygons(
    color = "yellow",
    opacity = 0.1,
    fillOpacity = 0.1,
    data = dat$satellite_metadata.geometry_coords[[i]]
    ) %>%
  leaflet::addPolygons(color = "blue",
                       opacity = 0.1,
                       fillOpacity = 0.1,
                       data = dat$geometry_bb[[i]]) %>%
  leaflet::addCircles(color = "red",
                      data = dat$geometry[[i]]) %>%
  leaflet::addCircles(color = "black",
                      data = dat$geometry_bb_point_on_surface[[i]]) %>%
  leaflet::setView(lng = dat$geometry_bb_point_on_surface[[i]][1],
                   lat = dat$geometry_bb_point_on_surface[[i]][2],
                   zoom = 12) %>%
  leaflet::addLegend(position = "topright",
                     title = see,
                     colors = c("#000000", "#ff0000"),
                     labels = c("point_on_surface", "malte")
                     ) %>%
  # leaflet::addCircles(color = "orange",
  #                      data = see_malte) %>%
  print()
  kwb.base::hsWait(0.5)
}



meta <- get_results(sat_list = lakes_01_centroid, data_type = "metadata")


lakes_bb[lakes_bb$SEE_NAME == "Plötzensee", ] %>%
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
