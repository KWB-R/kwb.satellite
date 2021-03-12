[![R-CMD-check](https://github.com/KWB-R/kwb.satellite/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.satellite/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.satellite/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.satellite/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.satellite/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.satellite)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.satellite)]()

# kwb.satellite

R Package with functions for working with satellite
data of Copernicus Climate Data Store
(https://cds.climate.copernicus.eu) or GoogleEarthEngine
(https://earthengine.google.com/).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.satellite' from GitHub
remotes::install_github("KWB-R/kwb.satellite")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.satellite](https://kwb-r.github.io/kwb.satellite)

Development: [https://kwb-r.github.io/kwb.satellite/dev](https://kwb-r.github.io/kwb.satellite/dev)
