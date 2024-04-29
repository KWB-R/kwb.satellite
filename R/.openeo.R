library(magrittr)
year <- 2018

lakes <- sf::read_sf("Seen25_20211105/seen25.shp")

start_date <- sprintf("%d-01-01", year)
end_date <- sprintf("%d-12-31", year)

lakes <- sf::st_transform(lakes, crs = 4326) %>%
  dplyr::filter(SEE_NAME != "-")

lakes <- lakes[lakes$SEE_NAME == "Großer Baalsee",]


lakes_boundary <- lakes %>%
  sf::st_bbox() %>%
  sf::st_as_sfc()



openeo_con <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")

openeo::login(openeo_con)

p <- openeo::processes()

formats <- openeo::list_file_formats()

colls <- openeo::list_collections()

bands <- as.list(c("QA60", sprintf("B%02d", 1:6)))

data <- p$load_collection(id = colls$SENTINEL2_L2A$id,
                          spatial_extent = lakes_boundary,
                          temporal_extent = list("2018-04-01",
                                                 "2018-05-01"),
                          bands = bands)


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

result <- p$save_result(data = temporal_reduce,
                        format = formats$output$CSV)

job_definition <- openeo::create_job(result, title = "Baalsee_temp-reduce")

as(object = job_definition, "Process")


jobs <- openeo::list_jobs()
openeo::start_job(job = job_definition$id, log = TRUE)
openeo::describe_job(job_definition$id)
