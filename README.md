
# openRspat

<!-- badges: start -->
<!-- badges: end -->

The goal of openRspat is to ...

## Installation

You can install the released version of openRspat from [GitHub](https://github.com/) with:

``` r
install.github("Ohm-Np/openRspat")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(openRspat)
## basic example code

fname <- system.file("shape/nc.shp", package="sf")
sf <- sf::st_read(fname)
x <- sf[1, ]
area_proj(x)
```

