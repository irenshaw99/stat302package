
# stat302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/irenshaw99/stat302package/workflows/R-CMD-check/badge.svg)](https://github.com/irenshaw99/stat302package/actions)
[![codecov](https://codecov.io/gh/irenshaw99/stat302package/branch/master/graph/badge.svg?token=Y37J24FBA2)](https://codecov.io/gh/irenshaw99/stat302package)
<!-- badges: end -->

The goal of stat302package is to ...

## Installation

You can install the stat302package from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("irenshaw99/stat302package")
library(stat302package)

# Alternatively, install using these additional parameters to view the vignettes
devtools::install_github("irenshaw99/stat302package", build_vignette = TRUE, build_opts = c())
```

## Use

The vignette gives a tutorial on how to use the 4 main functions of this package. The vignette can be viewed using this code:

``` r
library(stat302package)
# Use this to view the vignette in the stat302package HTML help
help(package = "stat302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302package")
```



