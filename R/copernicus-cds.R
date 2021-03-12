
#' Copernicus Climate Data Store: Single Query
#'
#' @description Run request (may take a while as nothing happens because many
#'   requests R are queued (in addition: maximum request data amount limited to
#'   140000 data points). To check queue status:
#'   "https://cds.climate.copernicus.eu/cdsapp#!/yourrequests?tab=form"
#' @param variable variable to query "2m_temperature"
#' @param dataset_short_name "reanalysis-era5-single-levels"
#' @param product_type  default: "reanalysis"
#' @param years character vector of years (default:  as.character(seq(2010,
#'   2020)))
#' @param area area coordinates in latitude/longitude (default: c(40, 116, 39,
#'   117))
#' @param file_format "grib" or "netcdf"
#' @param export_dir default: "."
#'
#' @return path to file with exported data
#' @export
#' @importFrom ecmwfr wf_request
#'
copernicus_cds <- function(
  variable = "2m_temperature",
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type = "reanalysis",
  years = as.character(seq(2010, 2020)),
  area = c(40, 116, 39, 117),
  file_format = "grib",
  export_dir = "."
)
{
  request <- list(
    product_type = product_type,
    format = file_format,
    variable = variable,
    year = years,
    month = sprintf("%02d", 1:12),
    day = sprintf("%02d", 1:31),
    time = sprintf("%02d:00", 0:23),
    area = area,
    dataset_short_name = dataset_short_name,
    target =  sprintf(
      "%s_%s_%s-%s.%s",
      dataset_short_name,
      variable,
      min(as.numeric(years)),
      max(as.numeric(years)),
      file_format
    )
  )

  ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    path = export_dir,
    verbose = FALSE
  )
}

#' Copernicus Climate Data Store: Multi Query
#' @description Runs \code{copernicus_cds} in parallel for all variables defined
#'   in parameter "variables" on all machine cores minus one
#' @param variables default: c("2m_temperature", "evaporation",
#'   "potential_evaporation", "precipitation_type", "runoff",
#'   "sub_surface_runoff", "surface_runoff", "total_precipitation")
#' @param dataset_short_name "reanalysis-era5-single-levels"
#' @param product_type  default: "reanalysis"
#' @param years character vector of years (default:  as.character(seq(2010,
#'   2020)))
#' @param area area coordinates in latitude/longitude (default: c(40, 116, 39,
#'   117))
#' @param file_format "grib" or "netcdf"
#' @param export_dir default: "."
#' @return list with paths to downloaded files
#' @export
#' @importFrom kwb.utils catAndRun
#' @importFrom parallel detectCores parLapply makeCluster stopCluster
#'
copernicus_cds_parallel <- function(
  variables = c(
    "2m_temperature",
    "evaporation",
    "potential_evaporation",
    "precipitation_type",
    "runoff",
    "sub_surface_runoff",
    "surface_runoff",
    "total_precipitation"
  ),
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type = "reanalysis",
  years = as.character(seq(2010, 2020)),
  area = c(40, 116, 39, 117),
  file_format = "grib",
  export_dir = "."
)
{
  ncores <- parallel::detectCores() - 1

  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))

  msg <- sprintf(
    "Importing %d variable(s) (%s) from Copernicus CDS parallel (ncores = %d)",
    length(variables),
    kwb.utils::stringList(variables),
    ncores
  )

  kwb.utils::catAndRun(
    messageText = msg,
    expr = parallel::parLapply(cl, variables, function(variable) {
      try(copernicus_cds(
        variable,
        dataset_short_name = dataset_short_name ,
        product_type = product_type,
        years = years,
        area = area,
        file_format = file_format,
        export_dir = export_dir
      ))
    })
  )
}
