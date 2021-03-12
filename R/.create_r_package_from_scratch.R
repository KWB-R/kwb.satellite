### How to build an R package from scratch
remotes::install_github("kwb-r/kwb.pkgbuild")

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Michael Rustler",
               orcid = "0000-0003-0647-7726",
               url = "https://mrustl.de")

pkg <- list(name = "kwb.satellite",
            title = " R Package for Working with Satellite Data from Various Providers (Copernicus, GoogleEarthEngine)",
            desc  = paste("R Package with functions for working with satellite data of",
                          "Copernicus Climate Data Store (https://cds.climate.copernicus.eu)",
                          "or GoogleEarthEngine (https://earthengine.google.com/)."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("copernicus-cds")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c('dplyr', 'httr', 'kwb.utils', 'magrittr', 'rlang', 'rvest',
                        'stringr', 'tidyr', 'xml2')

  sapply(pkg_dependencies, usethis::use_package)

  desc::desc_add_remotes("kwb-r/kwb.datetime",normalize = TRUE)
  desc::desc_add_remotes("kwb-r/kwb.utils",normalize = TRUE)
  usethis::use_pipe()
}

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages("wasserportal")
