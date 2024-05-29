#' OpenEO Helper Function: get job metadata
#'
#' @param job_id job id
#' @param debug print debug messages? (default: TRUE)
#'
#' @return tibble with job metadata (e.g. costs)
#' @export
#' @importFrom openeo describe_job
#' @importFrom kwb.utils catAndRun
#' @importFrom tidyselect ends_with matches
#' @importFrom lubridate as_datetime
#' @importFrom dplyr across mutate
#' @importFrom tibble as_tibble
openeo_get_job_metadata <- function(job_id, debug = TRUE) {

kwb.utils::catAndRun(sprintf("Getting job metadata for job id '%s'",
                             job_id),
                     expr = {
job_info <- openeo::describe_job(job_id)
cols <- which(names(job_info) != "process")

meta <- tibble::as_tibble(t(unlist(job_info[cols]))) %>%
  dplyr::mutate(dplyr::across(tidyselect::matches("progress"), as.double)) %>%
  dplyr::mutate(dplyr::across(tidyselect::matches("costs"), as.integer)) %>%
  dplyr::mutate(dplyr::across(tidyselect::ends_with("value"), as.numeric)) %>%
  dplyr::mutate(created = lubridate::as_datetime(created, tz = "UTC"),
                updated = lubridate::as_datetime(updated, tz = "UTC"))

},
dbg = debug)
}


