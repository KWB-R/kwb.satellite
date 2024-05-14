library(magrittr)
year <- 2018

archive::archive_extract("https://data.geobasis-bb.de/geofachdaten/Wasser/Hydrologie/seen25.zip",
                         dir = "lakes_bb")
lakes_bb <- sf::read_sf("lakes_bb/Seen25_20211105/seen25.shp")

start_date <- sprintf("%d-01-01", year)
end_date <- sprintf("%d-12-31", year)

lakes <- sf::st_transform(lakes_bb, crs = 4326) %>%
  dplyr::filter(SEE_NAME != "-")

lakes <- lakes[lakes$SEE_NAME == "Großer Baalsee",] %>%
  sf::st_transform(4326)


lakes_boundary <- lakes %>%
  sf::st_bbox() %>%
  sf::st_as_sfc()

openeo_con <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")

openeo::login(openeo_con)

p <- openeo::processes()

formats <- openeo::list_file_formats()

colls <- openeo::list_collections()

bands <- as.list(c("CLD", sprintf("B%02d", 1:6)))

data <- p$load_collection(id = colls$SENTINEL2_L2A$id,
                          spatial_extent = lakes,
                          temporal_extent = list("2017-01-01",
                                                 Sys.Date()))


temporal_reduce = p$reduce_dimension(data = data,
                                     dimension = "t",
                                     reducer = function(x,y){
  mean(x)
})

apply_linear_transform = p$apply(data=temporal_reduce,process = function(value,...) {
  p$linear_scale_range(x = value,
                       inputMin = -1,
                       inputMax = 1,
                       outputMin = 0,
                       outputMax = 255)
})

result <- p$save_result(data = data,
                        format = formats$output$netCDF)

job_definition <- openeo::create_job(result, title = "Baalsee_raw_all-bands_2017-2024_netCDF")

as(object = job_definition, "Process")


jobs <- openeo::list_jobs()
jobs
openeo::start_job(job = job_definition$id, log = TRUE)
openeo::describe_job(job_definition$id)
netcdf_path <- openeo::download_results(job = job_definition$id,
                                        folder = "./openeo_netcdf")

dat <- ncdf4::nc_open(netcdf_path[[1]])

dat$dim$t$vals + as.Date("1990-01-01")


View(dat)

b01_mat <- ncdf4::ncvar_get(dat, varid = "B01")

b01_mat[,,1]
