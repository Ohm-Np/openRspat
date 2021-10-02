
# openRspat

<!-- badges: start -->
<!-- badges: end -->

The goal of openRspat is to create API access to the free and open source geodatasets and provides functions to process those datasets.

## Installation

You can install the released version of openRspat from [GitHub](https://github.com/) with:

``` r
install_github("Ohm-Np/openRspat")
```

## Example

This is a basic example which shows you how to solve a common problem:

`1. area_proj`

``` r
library(openRspat)
## basic example code

fname <- system.file("shape/nc.shp", package="sf")
sf <- sf::st_read(fname)
x <- sf[1, ]
area_proj(x)
```

`2. get_copernicus_land_cover`

```{r}
library(openRspat)
## basic example code

f <- "../../" # path to the folder where you want to download the rasters
get_copernicus_land_cover(2015, "W120N40", f)
```

