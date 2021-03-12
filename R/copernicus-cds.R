
#' Copernicus Climate Data Store: Single Query
#'
#' @description Run request (may take a while as nothing happens because many requests R
#' are queued (in addition: maximum request data amount limited to 140000
#' data points).
#' To check queue status: "https://cds.climate.copernicus.eu/cdsapp#!/yourrequests?tab=form"
#' @param variable variable to query "2m_temperature"
#' @param dataset_short_name "reanalysis-era5-single-levels"
#' @param product_type  default: "reanalysis"
#' @param years character vector of years (default:  as.character(seq(2010, 2020)))
#' @param area area coordinates in latitude/longitude (default: c(40, 116, 39, 117))
#' @param file_format "grib" or "netcdf"
#' @param export_dir default: "."
#'
#' @return path to file with exported data
#' @export
#' @importFrom ecmwfr wf_request
#'
copernicus_cds <- function(variable = "2m_temperature",
                           dataset_short_name = "reanalysis-era5-single-levels",
                           product_type = "reanalysis",
                           years = as.character(seq(2010, 2020)),
                           area = c(40, 116, 39, 117),
                           file_format = "grib",
                           export_dir = ".") {



  request <- list(
    product_type = product_type,
    format = file_format,
    variable = variable,
    year = years,
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
            "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
            "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00",
             "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00",
             "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00",
             "21:00", "22:00", "23:00"),
    area = area,
    dataset_short_name = dataset_short_name,
    target =  sprintf("%s_%s_%s-%s.%s",
                      dataset_short_name,
                      variable,
                      min(as.numeric(years)),
                      max(as.numeric(years)),
                      file_format))


  ecmwfr::wf_request(request = request,
                     transfer = TRUE,
                     path = export_dir,
                     verbose = FALSE)

}


#' Copernicus Climate Data Store: Multi Query
#'
#' @param variables default: c("2m_temperature", "evaporation", "potential_evaporation",
#' "precipitation_type", "runoff", "sub_surface_runoff", "surface_runoff",
#' "total_precipitation")
#'
#' @return list with paths to downloaded files
#' @export
#' @importFrom kwb.utils catAndRun
#' @importFrom parallel detectCores parLapply makeCluster stopCluster
#'
copernicus_cds_parallel <- function(variables = c("2m_temperature",
                                                  "evaporation",
                                                  "potential_evaporation",
                                                  "precipitation_type",
                                                  "runoff",
                                                  "sub_surface_runoff",
                                                  "surface_runoff",
                                                  "total_precipitation")
) {

  ncores <- parallel::detectCores() - 1

  cl <- parallel::makeCluster(ncores)

  msg <- sprintf("Importing %d variables from Copernicus CDS parallel (ncores = %d)",
                 length(variables),
                 ncores)


  kwb.utils::catAndRun(
    messageText = msg,
    expr = parallel::parLapply(
      cl, variables, function(variable) {
        try(copernicus_cds(variable))
      })
  )

  parallel::stopCluster(cl)
}

