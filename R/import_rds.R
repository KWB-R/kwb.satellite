#' Helper function: import .rds files from folder
#'
#' @param rds_dir path to directoy containing .rds files
#'
#' @return list with imported
#' @export
#' @importFrom kwb.utils removeExtension
#' @importFrom stats setNames
import_rds <- function(rds_dir) {

rds_paths <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)

rds_names <- kwb.utils::removeExtension(basename(rds_paths))

stats::setNames(lapply(rds_paths, readRDS),
                nm = rds_names)
}


