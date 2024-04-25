#' Google Earth Engine: get data for years
#'
#' @param years years
#' @param lakes lakes
#' @param bands bands
#' @param centroid centroid
#' @param ee_fun ee_fun
#' @param via via
#' @param col_lakename col_lakename (default: "GEWNAME")
#' @param debug show debug messages (default: TRUE)
#' @param ee_print show debug messages for "ee" (default: FALSE)
#'
#' @return tibble
#' @export
#' @importFrom rgee ee sf_as_ee ee_print
#' @importFrom stringr str_remove
#' @importFrom sf st_transform st_bbox st_as_sfc
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
gee_get_data_for_years <- function(years = 2018,
                                   lakes,
                                   bands = as.list(c("QA60", paste0("B", 1:6))),
                                   centroid = FALSE,
                                   ee_fun = rgee::ee$Reducer$mean(),
                                   via = "getInfo",
                                   col_lakename = "GEWNAME",
                                   debug = TRUE,
                                   ee_print = FALSE) {

  shape_type <- if(centroid) { "centroid"} else { "polygon"}

  reducer_function_name <-
    stringr::str_remove(ee_fun$getInfo()$type, pattern = "Reducer\\.")

  # Definieren der Koordinaten für den Punkt (z.B. Berlin)
  # Define an area of interest.
  lakes <- sf::st_transform(lakes, crs = 4326)

  lakes_boundary <- lakes %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  stats::setNames(lapply(years, function(year) {
    start_date <- sprintf("%d-01-01", year)
    end_date <- sprintf("%d-12-31", year)

    kwb.utils::catAndRun(sprintf("Downloading data for %d lakes for year '%d' and spatial aggregation function '%s'",
                                 nrow(lakes),
                                 year,
                                 reducer_function_name),
                         expr = {
                           # Definieren der Sentinel-2 Kollektion und Filtern nach Wolkenbedeckung
                           collection <- rgee::ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")

                           if(!is.null(bands)) collection <- collection$select(bands)

                           collection <- collection$
                             filterBounds(rgee::sf_as_ee(lakes_boundary))$
                             filterDate(start_date, end_date)

                           if (debug && ee_print) {
                             rgee::ee_print(collection) # Useful for debugging.
                           }

                           get_data(collection, lakes, centroid, ee_fun, via, col_lakename)

                         })
  }),
  nm = sprintf("%s_%s_%s", shape_type, reducer_function_name, years))
}


#' Google Earth Engine: get data for years
#'
#' @param collection collection
#' @param lakes lakes
#' @param centroid centroid
#' @param ee_fun ee_fun
#' @param via via
#' @param col_lakename col_lakename
#' @param debug debug
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
gee_get_data <- function (collection = collection,
                          lakes,
                          centroid = FALSE,
                          ee_fun = rgee::ee$Reducer$mean(),
                          via = "getInfo",
                          col_lakename = "GEWNAME",
                          debug = TRUE) {


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

        image_extract <- rgee::ee_extract(
          x = collection,
          y =  lake_gee,
          fun = ee_fun,
          scale = 10,
          sf = TRUE,
          via = via
        )#},
        #times = 10)})

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
                             values_from = value) #%>%
        #dplyr::filter(QA60 == 0)


        dplyr::bind_cols(lake,
                         tidyr::nest(band_timeseries, .key = "satellite_data"))
      },
      dbg = debug
    )

    return(res)
  })

}
