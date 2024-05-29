openeo_con <- openeo::connect(host = "https://openeo.dataspace.copernicus.eu")
openeo::login(openeo_con)

collection_id <- "SENTINEL2_L2A"

lakes_bb_selected_polygon <- lapply(seq_len(nrow(lakes_bb_selected)), function(i) {
  kwb.satellite::openeo_get_data(lakes = lakes_bb_selected[i,],
                                 collection_id = collection_id)
  })

job_ids <- sapply(seq_len(nrow(lakes_bb_selected)),
                  function(i) lakes_bb_selected_polygon [[i]]$job$id)

kwb.satellite::openeo_start_max_jobs(job_ids = job_ids)

jobs_finished <- tibble::as_tibble(openeo::list_jobs()) %>%
  dplyr::filter(status == "finished") %>%
  dplyr::arrange(dplyr::desc(updated))

nrow(jobs_finished)

jobs_meta <- lapply(jobs_finished$id, kwb.satellite::openeo_get_job_metadata) %>%
  dplyr::bind_rows()

tdir <- fs::path_abs("./vignettes/openeo/lakes_bb_selected_polygon/")
fs::dir_create(tdir)

jobs_results <- lapply(jobs_finished$id, function(id) {
                       kwb.satellite::openeo_download_results(job_id = id,
                                                              tdir = tdir)
  })


metadata_bands <- kwb.satellite::openeo_get_bands_meta(collection_id)


readr::write_csv(metadata_bands,
                 file = paste0(fs::path_abs("./vignettes/openeo/"),
                               sprintf("%s_metadata_bands.csv",
                                       collection_id)))
