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
flatten_results <- function(sat_data_list,
                            cols_unnest = "satellite_data_metadata") {

  if(sum(c("satellite_data", "satellite_metadata") %in% cols_unnest) == 2) {
    names_sep <- "."
  } else {
    names_sep <- NULL
  }

  sat_data_df_nested <- sat_data_list %>%
    dplyr::bind_rows()


  lapply(seq_len(nrow(sat_data_df_nested)),
         function(i) {
           sat_data_df_nested[i, ] %>%
             tidyr::unnest(tidyselect::all_of(cols_unnest),
                           names_sep = names_sep)
         }) %>%
    dplyr::bind_rows()

}
