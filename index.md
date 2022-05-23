[![R-CMD-check](https://github.com/KWB-R/kwb.satellite/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.satellite/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.satellite/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.satellite/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.satellite/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.satellite)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.satellite)]()
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.6566986.svg)](https://doi.org/10.5281/zenodo.6566986)

R Package with functions for working with satellite
data of Copernicus Climate Data Store
(https://cds.climate.copernicus.eu) or GoogleEarthEngine
(https://earthengine.google.com/).

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.satellite in R
install.packages('kwb.satellite')

# Browse the kwb.satellite manual pages
help(package = 'kwb.satellite')
```

## Workflows

- [Copernicus Climate Data](../articles/copernicus-cds.html)

- [Google Earth Engine](../articles/google-earth-engine.html)

