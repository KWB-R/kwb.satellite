#' Google Earth Engine: get data for years
#'
#' @param years years vector of years for which satellite data should be
#'   downloaded
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param image_collection image collection (default:
#'   "COPERNICUS/S2_SR_HARMONIZED")
#' @param bands bands (defualt: NULL), for selection provide in the following
#'   format: as.list(c("QA60", paste0("B", 1:6)))
#' @param point_on_surface use sf::st_point_on_surface or polygon? (default:
#'   FALSE)
#' @param spatial_fun spatial aggregation function (default: "mean")
#' @param scale scale parameter (default: 10), for details, see
#'   \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud (google
#'   drive or google cloud storage)
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for
#'   surface water bodies)
#' @param debug print debug messages? (default: TRUE)
#' @param ee_print show debug messages for "ee" (default: FALSE)
#' @param n_year_splits  number of year splits per request. Required in case
#'   request uses too much images > 400-500 per year (default: NULL, determined
#'   automatically within function. In case it should be overwritten by the user
#'   provide a meaningful integer number)
#' @return list with data and metadata, each of them tibbles
#' @export
#' @importFrom rgee ee sf_as_ee ee_print
#' @importFrom stringr str_remove
#' @importFrom sf st_transform st_bbox st_as_sfc st_as_sf
#' @importFrom kwb.utils catAndRun
#' @importFrom stats setNames
#' @importFrom lubridate yday
gee_get_data_for_years <- function(
    years = 2018,
    lakes,
    image_collection = "COPERNICUS/S2_SR_HARMONIZED",
    bands = NULL, #as.list(c("QA60", paste0("B", 1:6))),
    point_on_surface = FALSE,
    spatial_fun = "mean",
    scale = 10,
    via = "getInfo",
    col_lakename = "GEWNAME",
    debug = TRUE,
    ee_print = FALSE,
    n_year_splits = NULL
)
{
  stopifnot(spatial_fun %in% names(rgee::ee$Reducer))

  lakes_obj <- deparse(substitute(lakes))

  if (! "sf" %in% class(lakes))  {
    message(sprintf("Converting object 'lakes' = '%s'", lakes_obj))
    lakes <- sf::st_as_sf(lakes)
  }

  lakes <- sf::st_transform(lakes, crs = 4326)

  lakes_boundary <- lakes %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  lapply(years, function(year) {

    collection_year <- rgee::ee$ImageCollection(image_collection)

    if (!is.null(bands)) {
      collection_year <- collection_year$select(bands)
    }

    collection_year <- collection_year$
      filterBounds(rgee::sf_as_ee(lakes_boundary))$
      filterDate(
        sprintf("%d-01-01", as.integer(year)),
        sprintf("%d-12-31", as.integer(year))
      )

    dat_year <- collection_year$getInfo()

    n_images_year <- length(dat_year$features)
    n_bands <- length(dat_year$features[[1]]$bands)

    if (is.null(n_year_splits)) {
      n_periods <- ceiling(n_bands * n_images_year / 5000) + 1L
      if (as.integer(year) == as.integer(format(Sys.Date(), format = "%Y"))) {
        n_periods <- ceiling(n_periods * lubridate::yday(Sys.Date()) / 365)
      }
    }

    dates <- create_periods_in_year(year, n_periods)

    kwb.utils::catAndRun(
      messageText = sprintf(
        "Available images for year %d: %d",
        year,
        n_images_year
      ),
      dbg = debug,
      newLine = 1L,
      expr = {

        lapply(seq_len(nrow(dates)), function(idx) {

          collection_split <- rgee::ee$ImageCollection(image_collection)

          if (!is.null(bands)) {
            collection_split <- collection_split$select(bands)
          }

          collection_split <- collection_split$
            filterBounds(rgee::sf_as_ee(lakes_boundary))$
            filterDate(dates$start[idx], dates$end[idx])

          if (debug && ee_print) {
            rgee::ee_print(collection_split)
          }

          dat <- collection_split$getInfo()

          n_images <- length(dat$features)
          n_bands <- length(dat$features[[1]]$bands)

          stopifnot(n_images > 0L)
          stopifnot(n_bands > 0L)

          msg_txt <- sprintf(
            paste0(
              "Downloading data for %d lake(s) for year '%d' (%s - %s) and",
              " spatial aggregation function '%s' with scale %d m (number_of_images:",
              "%d, number_of_bands: %d)"
            ),
            nrow(lakes),
            year,
            dates$start[idx],
            dates$end[idx],
            spatial_fun,
            scale,
            n_images,
            n_bands
          )

          kwb.utils::catAndRun(
            messageText = msg_txt,
            dbg = debug,
            newLine = 1L,
            expr = {
              stopifnot(n_images * n_bands <= 5000)
              gee_get_data(
                collection = collection_split,
                lakes = lakes,
                point_on_surface = point_on_surface,
                spatial_fun = spatial_fun,
                scale = scale,
                via = via,
                col_lakename = col_lakename
              ) %>%
                dplyr::bind_cols(tibble::tibble(
                  year = year,
                  date_start = dates$start[idx],
                  date_end = dates$end[idx]
                ))
            })

        })
      }
    ) %>%
      dplyr::bind_rows()

  }) %>%
    dplyr::bind_rows()
}


#' Google Earth Engine: get data for years
#'
#' @param collection collection satellite collection
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param point_on_surface use sf::st_point_on_surface() or polygon? (default:
#'   FALSE)
#' @param spatial_fun spatial aggregation function (default: "mean")
#' @param scale scale parameter (default: 10), for details, see
#'   \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud storage
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for
#'   surface water bodies)
#' @param debug print debug messages? (default: TRUE)
#'
#' @return tibble
#' @export
#'
#' @importFrom kwb.utils catAndRun
#' @importFrom rgee sf_as_ee
#' @importFrom sf st_point_on_surface st_bbox st_point st_geometry
#' @importFrom tibble as_tibble
#' @importFrom dplyr arrange select mutate bind_cols
#' @importFrom tidyselect all_of
#' @importFrom lubridate ymd_hms
#' @importFrom tidyr pivot_longer separate nest
gee_get_data <- function (
    collection,
    lakes,
    point_on_surface = FALSE,
    spatial_fun = "mean",
    scale = 10,
    via = "getInfo",
    col_lakename = "GEWNAME",
    debug = TRUE
)
{
  stopifnot(spatial_fun %in% names(rgee::ee$Reducer))

  lakes_obj <- deparse(substitute(lakes))

  if (! "sf" %in% class(lakes)) {
    message(sprintf("Converting object 'lakes' = '%s'", lakes_obj))
    lakes <- sf::st_as_sf(lakes)
  }

  lapply(seq_len(nrow(lakes)), function(idx) {

    lake <- lakes[idx, ]

    shape_type <- tolower(sf::st_geometry_type(lake))

    kwb.utils::catAndRun(
      messageText = sprintf(
        "Getting data for '%s' (%3d/%3d)",
        lake[[col_lakename]],
        idx,
        nrow(lakes)
      ),
      dbg = debug,
      newLine = 1L,
      expr = {
        lake_gee <- if (point_on_surface & shape_type != "point") {
          kwb.utils::catAndRun(
            messageText = sprintf(
              "convert '%s' to point with 'sf::st_point_on_surface()'",
              shape_type
            ),
            dbg = debug,
            expr = {
              shape_type <- sprintf("%s_to_point-on-surface", shape_type)
              x <- lake %>%
                sf::st_transform(25833) %>%
                sf::st_point_on_surface() %>%
                sf::st_transform(4326)

              x <- sf::st_bbox(x)[1:2]
              rgee::sf_as_ee(sf::st_point(x))
            }
          )

        } else {

          message(sprintf(
            "using '%s' geometry provided in 'lakes' argument",
            shape_type
          ))

          lake %>%
            sf::st_geometry() %>%
            rgee::sf_as_ee()
        }

        collection_lake <- if (nrow(lakes) > 1L) {
          collection$filterBounds(lake_gee)
        } else {
          collection
        }

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
            into = c(
              "datetime_start",
              "datetime_end",
              "tile_id",
              "band1",
              "band2"
            ),
            sep = "_",
            fill = "right"
          ) %>%
          dplyr::mutate(
            band = dplyr::if_else(
              is.na(band2),
              band1,
              paste0(band1, "_", band2)
            ),
            datetime_start = lubridate::ymd_hms(datetime_start),
            datetime_end = lubridate::ymd_hms(datetime_end)
          ) %>%
          dplyr::select(! tidyselect::all_of(c("band1", "band2"))) %>%
          dplyr::relocate("band", .before = "value") %>%
          dplyr::arrange(datetime_start, band)

        band_timeseries_wide <- band_timeseries %>%
          tidyr::pivot_wider(names_from = band, values_from = value) %>%
          dplyr::mutate(geometry_filter = rgee::ee_as_sf(lake_gee))

        dat_meta <- dplyr::left_join(
          band_timeseries_wide,
          metadata,
          by = c("datetime_start", "datetime_end", "tile_id")
        )

        dplyr::bind_cols(lake, tidyr::nest(
          dat_meta, .key = "satellite_data_metadata"
        )) %>%
          dplyr::bind_cols(tidyr::nest(
            band_timeseries_wide,
            .key = "satellite_data"
          )) %>%
          dplyr::bind_cols(tidyr::nest(
            metadata,
            .key = "satellite_metadata"
          )) %>%
          dplyr::bind_cols(tibble::tibble(
            satellite_data.nrow = nrow(band_timeseries_wide),
            satellite_metadata.nrow = nrow(metadata),
            shape_type = shape_type,
            spatial_fun = spatial_fun,
            scale = scale
          ))
      }
    )

  }) %>%
    dplyr::bind_rows()
}
