#' Create AD4GD Environment for Google Earth Engine Satellite Data
#'
#' @param force force installation even if already installed
#' @param debug print debug messages (default: FALSE)
#' @return python environment required for R package "rgee"
#' @export
#' @importFrom reticulate condaenv_exists
#' @importFrom kwb.python conda_py_install
create_ad4gd_env <- function(force = FALSE, debug = FALSE) {

  if(!reticulate::condaenv_exists("ad4gd") | force) {
  kwb.python::conda_py_install(env_name = "ad4gd",
                               pkgs = list(conda = c("python=3.12.2",
                                                     "numpy"),
                                           py = "earthengine-api==0.1.370"))
  } else {
    if(debug) {
      message(paste0("Conda environment 'ad4gd' already exists. Use ",
                             "'force' = TRUE, to reinstall if required"))
    }
  }
}

