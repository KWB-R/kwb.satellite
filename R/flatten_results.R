#' Flatten Results: simplify satellite data and metadata structure by unnesting
#' from list to tibble
#'
#' @param sat_data_list satellite data list result as retrieved by for example
#' \code{\link{gee_get_data_for_years_parallel}}
#' @param cols_unnest vector with names of columns to unnest (default: "satellite_data_metadata")
#' @return tibble with column names satellite_data.xxx and satellite_metadata.xxx
#'
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
#' @importFrom sf st_polygon st_sfc st_set_crs
flatten_results <- function(
    sat_data_list,
    cols_unnest = "satellite_data_metadata"
)
{
  cols_satellite <- c("satellite_data", "satellite_metadata")

  sat_data_df_nested <- sat_data_list %>%
    dplyr::bind_rows()

  seq_len(nrow(sat_data_df_nested)) %>%
    lapply(function(i) {
      tidyr::unnest(
        sat_data_df_nested[i, ],
        tidyselect::all_of(cols_unnest),
        names_sep = if (all(cols_satellite %in% cols_unnest)) "." # else NULL
      )
    }) %>%
    dplyr::bind_rows()
}
