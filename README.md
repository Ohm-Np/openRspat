![](https://komarev.com/ghpvc/?username=Ohm-Np)
# openRspat

<!-- badges: start -->
<!-- badges: end -->

The goal of openRspat is to create API access to the free and open source geodatasets and provide functions to process those datasets.

`Note: With every new functions being added to the repo, this readme file gets updated.`

## Installation

You can install the released version of openRspat from [GitHub](https://github.com/) with:

``` r
remotes::install_github("Ohm-Np/openRspat")
```

## Example

This is a basic example which shows you how to solve a common problem:

`1. area_proj`

The function generates projection string in Lambert Azimuthal Equal Area Projection of your desired sf object.
``` r
library(openRspat)
## basic example code

fname <- system.file("shape/nc.shp", package="sf")
sf <- sf::st_read(fname)
x <- sf[1, ]
area_proj(x)
```

============================================================================

`2. get_copernicus_land_cover`

With this function, you can download copernicus global land cover rasters for your desired region of interest. 100m spatial resolution rasters are available to download for the years 2015 to 2019.
```r
library(openRspat)
## basic example code

f <- "../../" # path to the folder where you want to download the rasters
get_copernicus_land_cover(2015, "W120N40", f)
```

============================================================================

`3. compute_land_cover`

With this function, you can compute 23 copernicus land cover classes values in sqkm.
```r
library(openRspat)
## basic example code


```

