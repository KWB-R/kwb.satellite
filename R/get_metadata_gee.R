#' Google Earth Engine: get metadata for collection
#'
#' @param collection collection
#'
#' @return tibble with metadata (to do: improve format of "geometry" column)
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tidyr nest
#' @importFrom tidyselect matches
gee_get_metadata <- function(collection) {

  nImages <- collection$size()$getInfo()

  collectionList <- collection$toList(nImages)

  metadata_list <- collection$fromImages(collectionList)
  metadata_list <- metadata_list$getInfo()


  metadata_features_df <- lapply(seq_along(metadata_list$features), function(idx) {
    tibble::as_tibble(metadata_list$features[[idx]]$properties)
  }) %>% dplyr::bind_rows() %>%
    tidyr::nest(geometry = tidyselect::matches("system:footprint"))

  # to do: convert coordinates in nested tibble into sf object
  # tmp <- matrix(unlist(metadata_features_df$geometry[[1]]$`system:footprint`$coordinates), ncol = 2)
  # tmp_list <- lapply(seq_len(nrow(tmp)), function(idx) tmp[idx,])
  # sf::st_multipolygon(x = tmp_list)

  metadata_ids <- tibble::tibble(
    id = sapply(seq_along(metadata_list$features), function(idx) metadata_list$features[[idx]]$id)
  )


  dplyr::bind_cols(metadata_ids, metadata_features_df)

  #
  # jsonlite::prettify(jsonlite::toJSON(metadata_list$features[[100]]$properties$CLOUDY_PIXEL_PERCENTAGE))
  #
  # metadata_list$features
  # metadata_df <- dplyr::bind_rows(metadata_list$features$properties)
}
