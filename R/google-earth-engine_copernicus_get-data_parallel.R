#' Google Earth Engine: get data for years in parallel
#'
#' @param years years vector of years for which satellite data should be downloaded
#' @param lakes lakes sf data frame witch shapes of lakes
#' @param image_collection image collection (default: "COPERNICUS/S2_SR_HARMONIZED")
#' @param bands bands (defualt: NULL), for selection provide in the following
#' format: as.list(c("QA60", paste0("B", 1:6)))
#' @param point_on_surface use sf::st_point_on_surface or polygon? (default: FALSE)
#' @param spatial_fun spatial aggregation function (default: "mean")
#' @param scale scale parameter (default: 10), for details, see
#' \url{https://developers.google.com/earth-engine/guides/scale}
#' @param via via (default: "getInfo"), other options use google cloud (google drive
#' or google cloud storage)
#' @param col_lakename col_lakename ("GEWNAME", used by Berlin authority for surface
#' water bodies) use "SEE_NAME" for Brandenburg lakes (default: "SEE_NAME")
#' @param col_lakeid ("GEWRNEU", used by Berlin authority for surface
#' water bodies) use "SEE_KZ" for Brandenburg lakes (default: "SEE_KZ")
#' @param set_lakenames_as_list_indices should lake names of "col_lakename" be used
#' for naming result list? (default: TRUE)
#' @param debug show debug messages (default: TRUE)
#' @param debug_dir directory where to save (default: tempdir())
#' @param ee_print show debug messages for "ee" (default: FALSE)
#' @param export_rds save sat data into rds object for each lake (default: TRUE)
#' @param export_dir directory where to save data for each lake (default: tempdir())
#' @param ncores number of cores for parallel processinfg (default:
#' parallel::detectCores() - 1)
#' @param n_year_splits  number of year splits per request. Required in case request
#' uses too much images > 400-500 per year (default: NULL, determined automatically within
#' function. In case it should be overwritten by the user provide a meaningful integer number)
#' @param return_list should results be provided as R list? (default: FALSE). If FALSE,
#' the rds_path to the exported data is provided in case export_rds is set to TRUE
#' @return list with data and metadata, each of them tibbles (if return_list = TRUE),
#' If FALSE, the rds_path to the exported data is provided in case export_rds is
#' set to TRUE. In case an error occurs NULL is returned
#' @export
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterEvalQ
#' clusterExport
#' @importFrom reticulate use_condaenv
#' @importFrom rgee ee_Initialize
#' @importFrom fs path_join
#' @importFrom stats setNames
#' @importFrom kwb.utils catAndRun
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @import foreach
gee_get_data_for_years_parallel <- function(
    years = 2018,
    lakes,
    image_collection = "COPERNICUS/S2_SR_HARMONIZED",
    bands = NULL,
    point_on_surface = FALSE,
    spatial_fun = "mean",
    scale = 10,
    via = "getInfo",
    col_lakename = "SEE_NAME",
    col_lakeid = "SEE_KZ",
    set_lakenames_as_list_indices = TRUE,
    debug = TRUE,
    debug_dir = tempdir(),
    ee_print = FALSE,
    export_rds = TRUE,
    export_dir = tempdir(),
    ncores = parallel::detectCores() - 1,
    n_year_splits = NULL,
    return_list = FALSE) {


  geos <- tolower(sf::st_geometry_type(lakes))

  shape_type <- if(all(geos == "point")) {
    "point"
  } else if (all(geos == "polygon") & point_on_surface == FALSE) {
    "polygon"
  } else if (all(geos == "polygon") & point_on_surface == TRUE) {
    "point-on-surface"
  } else {
    "unclear"
  }

  # create_ad4gd_env(debug = debug)
  # reticulate::use_condaenv("ad4gd")

  stopifnot(spatial_fun %in% names(rgee::ee$Reducer))

  stopifnot(ncores > 1)
  stopifnot(ncores <= parallel::detectCores())

  if (ncores > nrow(lakes)) {
    ncores <- nrow(lakes)
  }

  # Prepare parallel processing
  cl <- parallel::makeCluster(ncores,
                              outfile = fs::path_join(c(debug_dir,
                                                        "debug_parallel.txt")))
  on.exit(parallel::stopCluster(cl))

  my_fun <- function(idx) {
    if(debug) {
      lakename <- lakes[[col_lakename]][idx]
      tfile <- fs::path_join(c(debug_dir,
                               sprintf("debug_parallel_%03d_%s.txt",
                                       idx,
                                       lakename)))
      sink(tfile, append = FALSE)
    }

    res <- try(gee_get_data_for_years(
      years = years,
      lakes = lakes[idx,],
      image_collection = image_collection,
      bands = bands,
      point_on_surface = point_on_surface,
      spatial_fun = spatial_fun,
      scale = scale,
      via = via,
      col_lakename = col_lakename,
      debug =  debug,
      ee_print = ee_print,
      n_year_splits = n_year_splits)
      )

    if(any(class(res) == "try-error")) {
      not_failed <- FALSE
    } else {
      not_failed <- TRUE
    }


    if(debug) sink()

    return_obj <- NULL


    lake_idx <- if(is.null(lakes[[col_lakeid]][idx])) {
      sprintf(paste0("%0", nchar(nrow(lakes_bb_selected)), "d_"), idx)
    } else {
      ""
    }

    lake_id <- if(!is.null(lakes[[col_lakeid]][idx])) {
      paste0(lakes[[col_lakeid]][idx], "_")
    } else {
      ""
    }

    if(export_rds && not_failed) {
      rds_name <- sprintf("%s%s_%s%s_%s_scale-%dm_%4d-%4d.rds",
                          lake_idx,
                          lakes[[col_lakename]][idx],
                          lake_id,
                          shape_type,
                          spatial_fun,
                          scale,
                          min(years),
                          max(years))

      rds_path <- fs::path_join(c(export_dir, rds_name))

      kwb.utils::catAndRun(sprintf("Exporting dataset to '%s'", rds_path),
                           expr = { saveRDS(res, file = rds_path) }
      )

      return_obj <- rds_path
    }

      if(return_list | !(export_rds && not_failed)) {
        return_obj <- res
      }

    return(return_obj)
  }

  # Initialize necessary packages and environments on each cluster
  parallel::clusterEvalQ(cl, expr = {
    library(rgee)
    reticulate::use_condaenv("ad4gd")
    rgee::ee_Initialize()
  })

  ## Export lakes to all clusters
  #parallel::clusterExport(cl, varlist = c("lakes"))

  # Prepare parallel processing
  doParallel::registerDoParallel(cl)
  library(foreach)

  # Run the parallel processing
  sat_data <- kwb.utils::catAndRun(
    sprintf(
      "Downloading satellite data for %d lakes in parallel on %d cores",
      nrow(lakes),
      ncores
    ),
    expr = {
    sat_data <- foreach::foreach(idx = seq_len(nrow(lakes)),
                                 .combine = "c") %dopar% {
                                   my_fun(idx)
                                   }

    # Stop parallel processing
    doParallel::stopImplicitCluster()

    if(set_lakenames_as_list_indices) {
      sat_data <- setNames(sat_data, lakes[[col_lakename]])
    }

    sat_data
    },
    dbg = debug
  )

}
