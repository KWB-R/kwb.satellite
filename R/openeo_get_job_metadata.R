#' OpenEO Helper Function: get job metadata
#'
#' @param job_id job id
#' @param debug print debug messages? (default: TRUE)
#'
#' @return tibble with job metadata (e.g. costs)
#' @export
#' @importFrom openeo describe_job
#' @importFrom kwb.utils catAndRun
openeo_get_job_metadata <- function(job_id, debug = TRUE) {

kwb.utils::catAndRun(sprintf("Getting job metadata for job id '%s'",
                             job_id),
                     expr = {
job_info <- openeo::describe_job(job_id)
cols <- which(names(job_info) != "process")

tibble::as_tibble(t(unlist(job_info[cols])))
},
dbg = debug)
}


