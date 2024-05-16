#' Helper function: convert to list
#'
#' @param coords column with satellite metadata coordinates (system:footprint`$coordinates)
#'
#' @return list of coordinates
#' @keywords internal
#'
convert_to_list <- function(coords) {

  tmp_mat <- lapply(seq_along(coords),
                    FUN = function(idx) {

                      t(as.matrix(coords[[idx]]))
                    })

  list(do.call(rbind, tmp_mat))
}


#' Google Earth Engine: get metadata for collection
#'
#' @param collection collection
#'
#' @return tibble with metadata. In addition also the complex column
#' "geometry_meta_org" is simplified and stored in the new column "geometry_meta_cleaned"
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tidyr nest
#' @importFrom tidyselect matches all_of
gee_get_metadata <- function(collection) {

  nImages <- collection$size()$getInfo()

  collectionList <- collection$toList(nImages)

  metadata_list <- collection$fromImages(collectionList)
  metadata_list <- metadata_list$getInfo()


  metadata_features_df <- lapply(seq_along(metadata_list$features), function(idx) {
    tibble::as_tibble(metadata_list$features[[idx]]$properties)
  }) %>% dplyr::bind_rows() %>%
    tidyr::nest(geometry_meta_org = tidyselect::matches("system:footprint"))


  coords <- lapply(seq_len(nrow(metadata_features_df)), function(idx) {
    convert_to_list(metadata_features_df$geometry_meta_org[[idx]]$`system:footprint`$coordinates) %>%
      sf::st_polygon() %>%
      sf::st_sfc() %>%
      sf::st_set_crs(value = 4326)
  })

  metadata_features_df$geometry_meta_cleaned <- coords

  metadata_ids <- tibble::tibble(
    id = sapply(seq_along(metadata_list$features), function(idx) metadata_list$features[[idx]]$id)
  )

  dplyr::bind_cols(metadata_ids, metadata_features_df) %>%
    tidyr::separate(col = "id",
                    into = c("provider_name", "provider_collection", "id_short"),
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

}
