#' Google Earth Engine: get data for years in parallel
#'
#' @param years years vector of years for which satellite data should be downloaded
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param image_collection image collection (default: "COPERNICUS/S2_SR_HARMONIZED")
#' @param bands bands
#' @param centroid use centroid or polygon? (default: FALSE)
#' @param ee_fun spatial aggregation function (default: rgee::ee$Reducer$mean())
#' @param scale scale parameter (default: 10), for details, see
#' \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud (google drive
#' or google cloud storage)
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for surface
#' water bodies)
#' @param debug print debug messages? (default: TRUE)
#'
#' @param col_lakename col_lakename (default: "GEWNAME")
#' @param set_lakenames_as_list_indices should lake names of "col_lakename" be used
#' for naming result list? (default: TRUE)
#' @param debug show debug messages (default: TRUE)
#' @param debug_dir directory where to save (default: tempdir())
#' @param ee_print show debug messages for "ee" (default: FALSE)
#' @param ncores number of cores for parallel processinfg (default:
#' parallel::detectCores() - 1)
#'
#' @return list with data and metadata, each of them tibbles
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ
#' clusterExport
#' @importFrom reticulate use_condaenv
#' @importFromr rgee ee_Initialize
#' @importFrom fs path_join
#' @importFrom stats setNames
#' @importFrom kwb.utils catAndRun
gee_get_data_for_years_parallel <- function(
    years = 2018,
    lakes,
    image_collection = "COPERNICUS/S2_SR_HARMONIZED",
    bands = as.list(c("QA60", paste0("B", 1:6))),
    centroid = FALSE,
    ee_fun = rgee::ee$Reducer$mean(),
    scale = 10,
    via = "getInfo",
    col_lakename = "GEWNAME",
    set_lakenames_as_list_indices = TRUE,
    debug = TRUE,
    debug_dir = tempdir(),
    ee_print = FALSE,
    ncores = parallel::detectCores() - 1) {

  stopifnot(ncores > 1)
  stopifnot(ncores <= parallel::detectCores())

  # Prepare parallel processing
  cl <- parallel::makeCluster(ncores,
                              outfile = fs::path_join(c(debug_dir,
                                                        "debug_parallel.txt")
                                                      )
                              )
  on.exit(parallel::stopCluster(cl))


  parallel::clusterEvalQ(cl, expr = {
    library(rgee)
    reticulate::use_condaenv("ad4gd")
    rgee::ee_Initialize()
  })


  # Exportieren der lakes Variablem an die Clusterarbeiter
  parallel::clusterExport(cl = cl,
                          varlist = c("lakes"))

  # Ausführen der parallelen Verarbeitung
  sat_data <- kwb.utils::catAndRun(
    sprintf(
      "Downloading satellite data for %d lakes in parallel on %d cores",
      nrow(lakes_berlin),
      ncores
    ),
    expr = {
      # Aufrufen der parLapply-Funktion in einer (parallelen) Schleife
      parallel::parLapply(cl,
                          1:7,
                          fun = function(idx) {
                            if(debug) {
                              lakename <- lakes[[col_lakename]][idx]
                              tfile <- fs::path_join(c(debug_dir,
                                              sprintf("debug_parallel_%02d_%s.txt",
                                                      idx,
                                                      lakename)))
                              sink(tfile)
                            }

                            res <- gee_get_data_for_years(
                              years = 2018,
                              lakes = lakes[idx,],
                              image_collection = "COPERNICUS/S2_SR_HARMONIZED",
                              bands = as.list(c("QA60", paste0("B", 1:6))),
                              centroid = centroid,
                              ee_fun = rgee::ee$Reducer$mean(),
                              scale = scale,
                              via = via,
                              col_lakename = col_lakename,
                              debug = debug,
                              ee_print = ee_print)

                            if(debug) sink()
                            return(res)
                          }
                          )
    },
    dbg = debug
  )


  if(set_lakenames_as_list_indices) {
    sat_data <- stats::setNames(sat_data, lakes[[col_lakename]])
  }

  sat_data

}
