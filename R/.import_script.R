remotes::install_github("kwb-r/kwb.satellite@dev")

sat_dir <-  "//medusa/projekte$/SUW_Department/Projects/AD4GD/Exchange/01_data/01_input/satellite_data/google-earth-engine"

sat_dirs <- fs::dir_ls(sat_dir)

sat_dat <- stats::setNames(lapply(data_dirs, function(dir) {
  kwb.satellite::import_rds(rds_dir = dir, flatten = TRUE) #%>%
    #dplyr::bind_rows()
    }),
  nm = basename(data_dirs))

