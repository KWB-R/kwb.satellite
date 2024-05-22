#' Helper function: import .rds files from folder
#'
#' @param rds_dir path to directoy containing .rds files
#' @return list with imported
#' @export
#' @importFrom kwb.utils removeExtension
#' @importFrom stats setNames
import_rds <- function(rds_dir)
{
  files <- list.files(rds_dir, pattern = "\\.rds$", full.names = TRUE)
  objects <- lapply(files, readRDS)
  stats::setNames(objects, kwb.utils::removeExtension(basename(files)))
}
