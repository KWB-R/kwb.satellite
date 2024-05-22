#' Helper function: convert to list
#'
#' @param coords column with satellite metadata coordinates
#'   (system:footprint`$coordinates)
#' @return list of coordinates
#' @keywords internal
#'
convert_to_list <- function(coords)
{
  lapply(coords, function(x) t(as.matrix(x))) %>%
    do.call(what = rbind) %>%
    list()
}

#' Google Earth Engine: get metadata for collection
#'
#' @param collection collection
#' @return tibble with metadata. In addition also the complex column
#'   "geometry_meta_org" is simplified and stored in the new column
#'   "geometry_meta_cleaned"
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom kwb.utils removeColumns
#' @importFrom tidyr nest
#' @importFrom tidyselect matches
gee_get_metadata <- function(collection)
{
  metadata_list <- collection$size()$getInfo() %>%
    collection$toList() %>%
    collection$fromImages()

  metadata_list <- metadata_list$getInfo()

  metadata_features_df <- metadata_list$features %>%
    lapply(function(x) tibble::as_tibble(x$properties)) %>%
    dplyr::bind_rows() %>%
    tidyr::nest(geometry_meta_org = tidyselect::matches("system:footprint"))

  geometries <- metadata_features_df$geometry_meta_org

  coords <- lapply(seq_len(nrow(metadata_features_df)), function(i) {
    geometries[[i]]$`system:footprint`$coordinates %>%
      convert_to_list() %>%
      sf::st_polygon() %>%
      sf::st_sfc() %>%
      sf::st_set_crs(value = 4326)
  })

  metadata_features_df$geometry_meta_cleaned <- coords

  tibble::tibble(
    id = sapply(metadata_list$features, function(x) x$id)
  ) %>%
    dplyr::bind_cols(metadata_features_df) %>%
    tidyr::separate(
      col = "id",
      into = c("provider_name", "provider_collection", "id_short"),
      sep = "/",
      remove = FALSE
    ) %>%
    tidyr::separate(
      col = "id_short",
      into = c("datetime_start", "datetime_end", "tile_id"),
      sep = "_"
    ) %>%
    dplyr::mutate(
      datetime_start = lubridate::ymd_hms(datetime_start),
      datetime_end = lubridate::ymd_hms(datetime_end)
    ) %>%
    kwb.utils::removeColumns(c("provider_name", "provider_collection"))
}
