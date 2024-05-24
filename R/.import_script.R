data_dirs <- fs::dir_ls("vignettes/gee/current")

data <- stats::setNames(lapply(data_dirs, function(dir) {
  kwb.satellite::import_rds(rds_dir = dir, flatten = TRUE) #%>%
    #dplyr::bind_rows()
    }),
                nm = basename(data_dirs))

length(tmp_list)

tmp <- kwb.satellite::flatten_results(tmp_list)


kwb.satellite::flatten_results(tmp_list$`Bückwitzer See_point-on-surface_mean_scale-10m_2017-2023`)
