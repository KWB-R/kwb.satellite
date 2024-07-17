#' Helper function OpenEO: check job status
#'
#' @param job_id job id
#' @return status string of job
#' @export
#' @importFrom openeo describe_job
#'
check_job_status <- function(job_id) {
  job_info <- openeo::describe_job(job = job_id)
  return(job_info$status)
}

#' Helper function OpenEO: get number of active jobs
#'
#' @return number of active jobs
#' @export
#'
#' @importFrom openeo list_jobs
#' @importFrom tibble as_tibble
#' @importFrom dplyr pull
get_number_of_active_jobs <- function() {
  sum(openeo::list_jobs() %>%
        tibble::as_tibble() %>%
        dplyr::pull(status) %in% c("running", "queued"))
}


#' OpenEO: start maximum number of jobs
#'
#' @param job_ids character vector of job ids
#' @param max_jobs maximum number of concurrent jobs (default: 2)
#' @param check_interval check intervall in seconds if jobs are already finished
#' @param debug print debug messages (default: TRUE)
#'
#' @return starts all jobs respecting the max_jobs limit
#' @export
#' @importFrom kwb.utils catAndRun
openeo_start_max_jobs <- function(job_ids, max_jobs = 2, check_interval = 30, debug = TRUE) {
  idx <- 0

  while(idx < length(job_ids)) {
    active_jobs <- get_number_of_active_jobs()

    # Starte so viele neue Jobs wie möglich
    while(active_jobs < max_jobs && idx < length(job_ids)) {
      idx <- idx + 1
      job_status <- check_job_status(job_id = job_ids[idx])

      if(job_status %in% c("created", "error")) {
      kwb.utils::catAndRun(messageText = sprintf("Start job '%s' (%d/%d), active jobs: %d",
                                                 job_ids[idx],
                                                 idx,
                                                 length(job_ids),
                                                 active_jobs),
                           expr = {
                             openeo::start_job(job = job_ids[idx])
                             Sys.sleep(1)
                             active_jobs <- active_jobs + 1

                             Sys.sleep(1) # Kleine Pause, um Server nicht zu überlasten
                           },
      dbg = debug)
      } else if (job_status == "finished") {
        message(sprintf("Skipping job '%s'. It is already '%s'",
                        job_ids[idx],
                        job_status))
      } else {
        message(sprintf("Skipping job '%s'. It is already '%s'",
                        job_ids[idx],
                        job_status))
      }
    }

    if(active_jobs == max_jobs) {
      message(sprintf("We have to wait. There are already %d jobs 'running/queued' (%s)",
                      active_jobs,
                      Sys.time()))
    }

    # Wartezeit zwischen den Überprüfungen
    Sys.sleep(check_interval)

    # Überwachen und Anzahl der aktiven Jobs aktualisieren
    active_jobs <- get_number_of_active_jobs()
  }
}
