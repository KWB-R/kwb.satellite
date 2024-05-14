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



#' Flatten Results: simplify satellite data and metadata structure by unnesting
#' from list to tibble
#'
#' @param sat_data_list satellite data list result as retrieved by for example
#' \code{\link{gee_get_data_for_years_parallel}}
#'
#' @return tibble with column names satellite_data.xxx and satellite_metadata.xxx
#' in addition also the complex column satellite_data.geometry is simplified and
#' stored in a satellite_data.geometry_coords
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
#' @importFrom sf st_polygon st_sfc st_set_crs
flatten_results <- function(sat_data_list) {

  sat_meta_unnest <- sat_data_list %>%
    dplyr::bind_rows() %>%
    tidyr::unnest(c("satellite_data", "satellite_metadata"),
                  names_sep = ".")


  coords <- lapply(seq_len(nrow(sat_meta_unnest)), function(idx) {
    convert_to_list(sat_meta_unnest$satellite_metadata.geometry[[idx]]$`system:footprint`$coordinates) %>%
      sf::st_polygon() %>%
      sf::st_sfc() %>%
      sf::st_set_crs(value = 4326)
  })

  sat_meta_unnest$satellite_metadata.geometry_coords <- coords

  sat_meta_unnest
}
