
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rutils <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/frbcesab/rutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frbcesab/rutils/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/frbcesab/rutils/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/frbcesab/rutils/actions/workflows/pkgdown.yaml)
[![Test
coverage](https://github.com/frbcesab/rutils/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/frbcesab/rutils/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/frbcesab/rutils/branch/main/graph/badge.svg)](https://codecov.io/gh/frbcesab/rutils)
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

The R package `rutils` is a collection of R functions commonly used in
FRB-CESAB projects.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("frbcesab/rutils")
```

Then you can attach the package `rutils`:

``` r
library("rutils")
```

## Overview

- [`get_phylopic_image()`](https://frbcesab.github.io/rutils/reference/get_phylopic_image.html):
  downloads a silhouette from the [PhyloPic](https://www.phylopic.org/)
  database using the [PhyloPic API
  2.x.x](http://api-docs.phylopic.org/v2/)

- [`get_world_basemap()`](https://frbcesab.github.io/rutils/reference/get_world_basemap.html):
  downloads a spatial layer of world countries boundaries as defined by
  the IPBES

- [`leading_zero()`](https://frbcesab.github.io/rutils/reference/leading_zero.html):
  adds leading zeros to a vector

- [`multi_merge()`](https://frbcesab.github.io/rutils/reference/multi_merge.html):
  combines different matrices by row names and column names by
  performing a 2-dimension full join

- [`to_binomial_name()`](https://frbcesab.github.io/rutils/reference/to_binomial_name.html):
  corrects species binomial name case

- [`address_to_coords()`](https://frbcesab.github.io/rutils/reference/address_to_coords.html):
  gets geographic coordinates of any location in the World

- [`coords_to_address()`](https://frbcesab.github.io/rutils/reference/coords_to_address.html):
  retrieve any location in the World from geographic coordinates

- [`rename_col()`](https://frbcesab.github.io/rutils/reference/rename_col.html):
  Rename any columns of a dataframe (from 1 to as much columns as you
  want)

- [`signi()`](https://frbcesab.github.io/rutils/reference/signi.html):
  Produce produces significance symbols for the values of p

- [`plot_2d_img()`](https://frbcesab.github.io/rutils/reference/plot_2d_img.html):
  Plot two variables using images png as points

- [`hiking_time()`](https://frbcesab.github.io/rutils/reference/hiking_time.html):
  Hiking time calculation

- [`get_city_admin_info()`](https://frbcesab.github.io/rutils/reference/get_city_admin_info.html):
  Retrieve administrative information of a French city (postal code,
  INSEE code, region, etc.)

## Contribution

Visit the
[wiki](https://github.com/FRBCesab/rutils/wiki/How-to-contribute) to
learn how to contribute to this project.

## Citation

Please cite this package as:

> Casajus N. (2025) `rutils`: A collection of R functions commonly used
> in FRB-CESAB projects. R package version 0.0.0.9000.
> <https://frbcesab.github.io/rutils>.

## Code of Conduct

Please note that the `rutils` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
