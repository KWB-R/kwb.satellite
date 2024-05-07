#' Google Earth Engine: get data for years
#'
#' @param years years vector of years for which satellite data should be downloaded
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param image_collection image collection (default: "COPERNICUS/S2_SR_HARMONIZED")
#' @param bands bands
#' @param centroid use centroid or polygon? (default: FALSE)
#' @param spatial_fun spatial aggregation function (default: "mean")
#' @param scale scale parameter (default: 10), for details, see
#' \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud (google drive
#' or google cloud storage)
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for surface
#' water bodies)
#' @param debug print debug messages? (default: TRUE)
#'
#' @param col_lakename col_lakename (default: "GEWNAME")
#' @param debug show debug messages (default: TRUE)
#' @param ee_print show debug messages for "ee" (default: FALSE)
#' @param convert_to_tibble converts list to tibble (default: TRUE)
#' @return list with data and metadata, each of them tibbles
#' @export
#' @importFrom rgee ee sf_as_ee ee_print
#' @importFrom stringr str_remove
#' @importFrom sf st_transform st_bbox st_as_sfc st_as_sf
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
gee_get_data_for_years <- function(years = 2018,
                                   lakes,
                                   image_collection = "COPERNICUS/S2_SR_HARMONIZED",
                                   bands = as.list(c("QA60", paste0("B", 1:6))),
                                   centroid = FALSE,
                                   spatial_fun = "mean",
                                   scale = 10,
                                   via = "getInfo",
                                   col_lakename = "GEWNAME",
                                   debug = TRUE,
                                   ee_print = FALSE,
                                   convert_to_tibble = TRUE) {

  stopifnot(spatial_fun %in% names(rgee::ee$Reducer))

  lakes_obj <- deparse(substitute(lakes))

  if(! "sf" %in% class(lakes))  {
    message(sprintf("Converting object 'lakes' = '%s'", lakes_obj))
    lakes <- sf::st_as_sf(lakes)
  }

  shape_type <- if(centroid) { "centroid"} else { "polygon"}

  lakes <- sf::st_transform(lakes, crs = 4326)

  lakes_boundary <- lakes %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  sat_dat <- stats::setNames(lapply(years, function(year) {
    start_date <- sprintf("%d-01-01", year)
    end_date <- sprintf("%d-12-31", year)

    collection <- rgee::ee$ImageCollection(image_collection)

    if(!is.null(bands)) collection <- collection$select(bands)

    collection <- collection$
      filterBounds(rgee::sf_as_ee(lakes_boundary))$
      filterDate(start_date, end_date)

    if (debug && ee_print) {
      rgee::ee_print(collection) # Useful for debugging.
    }

    dat <- collection$getInfo()

    n_images <-  length(dat$features)
    n_bands <- length(dat$features[[1]]$bands)

    msg_txt <- sprintf(paste0("Downloading data for %d lake(s) for year '%d' and",
                              " spatial aggregation function '%s' (number_of_images:",
                              "%d, number_of_bands: %d)"),
                       nrow(lakes),
                       year,
                       spatial_fun,
                       n_images,
                       n_bands)

    kwb.utils::catAndRun(messageText = msg_txt,
                         expr = {
                           stopifnot(n_images * n_bands <= 5000)
                           gee_get_data(collection = collection,
                                        lakes = lakes,
                                        centroid = centroid,
                                        spatial_fun = spatial_fun,
                                        scale = scale,
                                        via = via,
                                        col_lakename = col_lakename)
                         },
                         dbg = debug,
                         newLine = 1L)}),
    nm = sprintf("%s_%s_%s", shape_type, spatial_fun, years))

  if(convert_to_tibble) {
    sat_dat <- dplyr::bind_rows(sat_dat, .id = "id") %>%
    tidyr::separate(id, into = c("shape_type", "spatial_function", "year"))
  }

  sat_dat

}


#' Google Earth Engine: get data for years
#'
#' @param collection collection satellite collection
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param centroid use centroid or polygon? (default: FALSE)
#' @param spatial_fun spatial aggregation function (default: "mean")
#' @param scale scale parameter (default: 10), for details, see
#' \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud storage
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for surface
#' water bodies)
#' @param debug print debug messages? (default: TRUE)
#'
#' @return tibble
#' @export
#'
#' @importFrom kwb.utils catAndRun
#' @importFrom rgee sf_as_ee
#' @importFrom sf st_centroid st_bbox st_point
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange select mutate bind_cols
#' @importFrom tidyselect all_of
#' @importFrom lubridate ymd_hms
#' @importFrom tidyr pivot_longer separate nest
gee_get_data <- function (collection,
                          lakes,
                          centroid = FALSE,
                          spatial_fun = "mean",
                          scale = 10,
                          via = "getInfo",
                          col_lakename = "GEWNAME",
                          debug = TRUE) {

  stopifnot(spatial_fun %in% names(rgee::ee$Reducer))

  lakes_obj <- deparse(substitute(lakes))

  if(! "sf" %in% class(lakes))  {
    message(sprintf("Converting object 'lakes' = '%s'", lakes_obj))
    lakes <- sf::st_as_sf(lakes)
  }

  lapply(seq_len(nrow(lakes)), function(idx) {
    lake <- lakes[idx, ]

    res <- kwb.utils::catAndRun(
      messageText = sprintf(
        "Getting data for '%s' (%3d/%3d)",
        lake[[col_lakename]],
        idx,
        nrow(lakes)
      ),
      expr = {
        lake_gee <- if (centroid) {
          print("centroid")
          x <- sf::st_centroid(lake)
          x <- sf::st_bbox(x)[1:2]
          rgee::sf_as_ee(sf::st_point(x))
        } else {
          print("polygon")
          rgee::sf_as_ee(lake)
        }

        collection_lake <- collection$filterBounds(lake_gee)

        metadata <- gee_get_metadata(collection_lake)

        image_extract <- rgee::ee_extract(
          x = collection,
          y =  lake_gee,
          fun = rgee::ee$Reducer[[spatial_fun]](),
          scale = scale,
          sf = TRUE,
          via = via
        )

        sat_col_ids <- startsWith(names(image_extract), "X")
        sat_cols <- names(image_extract)[sat_col_ids]

        band_timeseries <- image_extract %>%
          tibble::as_tibble() %>%
          dplyr::select(tidyselect::all_of(sat_cols)) %>%
          tidyr::pivot_longer(cols = sat_cols) %>%
          dplyr::mutate(name = stringr::str_remove(name, "X")) %>%
          tidyr::separate(
            name,
            into = c("datetime_start",
                     "datetime_end",
                     "tile_id",
                     "band"),
            sep = "_"
          ) %>%
          dplyr::mutate(
            datetime_start = lubridate::ymd_hms(datetime_start),
            datetime_end = lubridate::ymd_hms(datetime_end)
          ) %>%
          dplyr::arrange(datetime_start,
                         band)


        band_timeseries_wide <- band_timeseries %>%
          tidyr::pivot_wider(names_from = band,
                             values_from = value) %>%
          dplyr::mutate(geometry_filter = rgee::ee_as_sf(lake_gee))

        dplyr::bind_cols(lake,
                         tidyr::nest(band_timeseries_wide,
                                     .key = "satellite_data")) %>%
          dplyr::bind_cols(tidyr::nest(metadata,
                                       .key = "satellite_metadata"))
      },
      dbg = debug,
      newLine = 1L
    )

    return(res)
  }) %>%
   dplyr::bind_rows()

}
