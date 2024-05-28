#' OpenEO Helper function: get bands metadata
#'
#' @param collection_id collection id
#'
#' @return tibble with bands metadata
#' @export
#' @importFrom openeo describe_collection
#' @examples
#' \dontrun{
#' openeo_get_bands_meta(collection_id = "SENTINEL2_L2A")
#' }
#'
openeo_get_bands_meta <- function(collection_id) {
  suppressMessages(collection_details <- openeo::describe_collection(collection = collection_id))
  tibble::as_tibble(collection_details$summaries$`eo:bands`)
}

#' OpenEO: get data
#'
#' @param lakes sf object with lake(s) to get data for
#' @param collection_id collection id used by OpenEO (default: "SENTINEL2_L2A")
#' @param date_start date start (default:  "2017-01-01")
#' @param date_end date end (default: "2023-12-31")
#' @param point_on_surface (default: "FALSE)
#' @param spatial_fun function for spatial aggregation (default: "mean"). If NULL,
#' raw data are returned in "netCDF" format
#' @param bands (default: NULL). Be aware that naming differs compared to Google
#' Earth Engine (e.g. B01 -> openEO, B1 -> GEE)
#' @param output_format default: JSON, in case spatially unaggregated data (i.e.
#' spatial_fun == NULL) are returned, it is outmatically set to "netCDF"
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for surface
#' water bodies) use "SEE_NAME" for Brandenburg lakes (default: "SEE_NAME")
#' @param col_lakeid ("GEWRNEU", used by Berlin authority for surface
#' water bodies) use "SEE_KZ" for Brandenburg lakes (default: "SEE_KZ")
#' @param start_job should job be started (default: FALSE)
#' @return list with job object with job id for processing results data or starting
#' job afterwards and metadata for selected bands
#' @importFrom openeo processes list_collections list_file_formats create_job
#' start_job
#' @export
#' @examples
#' \dontrun{
#' openeo_con <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
#' openeo::login(openeo_con)
#' test_001002 <- openeo_get_data(lakes = lakes_bb_selected[1:2,])
#' test_001010 <- lapply(11:13, function(i) openeo_get_data(lakes = lakes_bb_selected[i,]))
#' }
openeo_get_data <- function(lakes,
                            collection_id = "SENTINEL2_L2A",
                            date_start = "2017-01-01",
                            date_end = "2023-12-31",
                            point_on_surface = FALSE,
                            spatial_fun = "mean",
                            bands = NULL,
                            output_format = "CSV",
                            col_lakename = "SEE_NAME",
                            col_lakeid = "SEE_KZ",
                            start_job = FALSE
                            ) {


p <- openeo::processes()
colls <- openeo::list_collections()

bands_meta_tibble <- openeo_get_bands_meta(collection_id)

if(!is.null(bands)) {
  stopifnot(all(bands %in% bands_meta_tibble$name))
  bands_meta_tibble <- bands_meta_tibble %>% dplyr::filter(name %in% bands)
}

formats <- openeo::list_file_formats()

stopifnot(collection_id %in% names(colls))
stopifnot(is.null(spatial_fun) || spatial_fun %in% names(p))
stopifnot(output_format %in% names(formats$output))

geos <- tolower(sf::st_geometry_type(lakes))

shape_type <- if(all(geos == "point")) {
  "point"
} else if (all(geos == "polygon") & point_on_surface == FALSE) {
  "polygon"
} else if (all(geos == "polygon") & point_on_surface == TRUE) {
  "point-on-surface"
} else {
  "unclear"
}

if(is.null(spatial_fun) && output_format != "netCDF") {
  message("Setting 'output_format' to 'netCDF, because '%s' is not supported by OpenEO",
          output_format)
  output_format <- "netCDF"
}


lake_name <- lakes[[col_lakename]]

lake_id <- if(!is.null(lakes[[col_lakeid]])) {
  paste0(lakes[[col_lakeid]], "_")
} else {
  ""
}

lakes <- if(point_on_surface) {
  lakes %>%
    sf::st_transform(crs = 25833) %>%
    sf::st_point_on_surface() %>%
    sf::st_transform(crs = 4326)
} else {
  lakes %>% sf::st_transform(crs = 4326)
}

lakes_boundary <- lakes %>%
  sf::st_bbox() %>%
  sf::st_as_sfc()


# load first datacube
cube <- p$load_collection(
  id = collection_id,
  spatial_extent = lakes_boundary,
  temporal_extent = c(date_start, date_end),
  bands = bands
)

# aggregate spatially
if(!is.null(spatial_fun)) {
cube <- p$aggregate_spatial(data = cube,
                            reducer = function(data, context) {p[[spatial_fun]](data) },
                                      geometries = sf_to_geojson(lakes))
}


# save result as JSON
res <- p$save_result(data = cube, format = output_format)

batch_name <- sprintf("%s_%s%s_%s_%s_%s-%s_%s",
                      lake_name,
                      lake_id,
                      shape_type,
                      collection_id,
                      if(is.null(spatial_fun)) {"raw"} else {spatial_fun},
                      date_start,
                      date_end,
                      output_format) %>% paste0(collapse = "_")

# send job to back-end
job <- openeo::create_job(graph = res, title = batch_name)

if(start_job) openeo::start_job(job$id)

list(job = job,
     metadata_bands = bands_meta_tibble)
}


