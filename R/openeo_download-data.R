#' Openeo: download results
#'
#' @param job_id job id
#' @param tdir target directory (default: tempdir())
#'
#' @return list with job metadata and data. names are the title of the job
#' @export
#' @importFrom openeo describe_job download_results
#' @importFrom stringr str_replace
#' @importFrom kwb.utils removeExtension
#' @importFrom fs path_join
#' @importFrom readr read_csv
#' @importFrom stats setNames
#' @importFrom dplyr arrange
#' @examples
#' \dontrun{
#' jobs_finished <- tibble::as_tibble(openeo::list_jobs()) %>%
#' dplyr::filter(status == "finished") %>%
#' dplyr::arrange(dplyr::desc(updated))
#'
#' sat_data <- lapply(1:6, function(i) {
#' kwb.satellite::openeo_download_results(job_id = jobs_finished$id[i])
#' }
#' )
#' }

openeo_download_results <- function(job_id, tdir = tempdir()) {

job_details <- openeo::describe_job(job_id)


job_data_path <- openeo::download_results(job_id, folder = tdir)[[1]]

file_rename <- stringr::str_replace(basename(job_data_path),
                                             kwb.utils::removeExtension(basename(job_data_path[[1]])),
                                    job_details$title)


job_data_path_new <- fs::path_join(c(dirname(job_data_path), file_rename))

fs::file_move(path = job_data_path,
              new_path = job_data_path_new)

job_data <- readr::read_csv(job_data_path_new) %>%
  dplyr::arrange(date)


named_list <- list(job_details = job_details,
                   job_data = job_data,
                   job_data_path = job_data_path_new)

stats::setNames(list(named_list), nm = job_details$title)
}


