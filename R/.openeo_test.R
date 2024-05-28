openeo_con <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
openeo::login(openeo_con)

test <- lapply(6:10, function(i) {
  kwb.satellite::openeo_get_data(lakes = lakes_bb_selected[i,])
  })

job_ids <- sapply(1:5, function(i) test[[i]]$job$id)

kwb.satellite::openeo_start_max_jobs(job_ids = job_ids)
