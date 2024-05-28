#' Helper function: import .rds files from folder
#'
#' @param rds_dir path to directoy containing .rds files
#' @param flatten should imported results be flattened (default: TRUE)
#' @param cols_unnest vector with names of columns to unnest (default:
#' "satellite_data_metadata"), only used if flatten == TRUE
#' @param drop_cols should columns not used for unnesting be dropped (refers to
#' the columns: "satellite_data_metadata", "satellite_data", "satellite_metadata"),
#' default: TRUE
#' @param run_parallel should multiple cores (if available) be used for import?
#' (default: TRUE)
#' @param n_cores number of cores to use if \code{run_parallel = TRUE}.
#' Default: one less than the detected number of cores.
#' @param debug print debug messages? (default: TRUE)
#' @return list of imported .rds file or flattened (if flatten == TRUE) with
#' tibble with column names satellite_data.xxx and satellite_metadata.xxx
#' @export
#' @importFrom kwb.utils removeExtension
#' @importFrom stats setNames
import_rds <- function(rds_dir,
                       flatten = TRUE,
                       cols_unnest = "satellite_data_metadata",
                       drop_cols = TRUE,
                       run_parallel = TRUE,
                       n_cores = parallel::detectCores() - 1L,
                       debug = TRUE) {

rds_paths <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

empty_files <- fs::file_size(rds_paths) == 0

if(sum(empty_files) > 0) {
message(sprintf("Ignoring %d zero byte files:\n %s",
                sum(empty_files),
        paste0(rds_paths[empty_files], collapse = ",\n")))

  rds_paths <- rds_paths[!empty_files]
}

n_paths <- length(rds_paths)
stopifnot(n_paths > 0)

rds_names <- kwb.utils::removeExtension(basename(rds_paths))

msg_txt <- sprintf("Importing%s %d .rds files from %s%s",
                   if(flatten) {
                     sprintf(" (and flattening using column(s) '%s')",
                             paste0(cols_unnest, collapse = ", "))
                   } else {
                     ""
                   },
                   n_paths,
                   rds_dir,
                   if(run_parallel) {
                     sprintf(" parallel (using %d cores)", n_cores)
                     } else { "" })

# Prepare parallel processing if desired
if (run_parallel) {
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl))
}

dat <- kwb.utils::catAndRun(messageText = msg_txt,
                     expr = {
                       stats::setNames(

                         if (run_parallel) {
                           parallel::parLapply(cl, rds_paths, function(rds_path) {
                             dat <- try(readRDS(rds_path))
                             if (flatten && !any(class(dat) == "try-error")) {
                               dat <- flatten_results(dat, cols_unnest, drop_cols)
                             }
                             dat
                           })
                         } else {
                         lapply(rds_paths, function(rds_path) {
                         dat <- try(readRDS(rds_path))
                         if (flatten && !any(class(dat) == "try-error")) {
                           dat <- flatten_results(dat, cols_unnest, drop_cols)
                         }
                         dat
                         })
                           }

                       ,
                       nm = rds_names)},
                     dbg = debug)

is_error <- sapply(dat, function(i) all(class(i) == "try-error"))

if(sum(is_error) > 0) {
  message(sprintf("Removing the following %d .rds files with errors:\n %s",
                  sum(is_error),
                  paste0(rds_paths[is_error], collapse = ",\n")))
  dat <- dat[!is_error]
}

dat

}

