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
Other available functions in the package are listed here:
|Functions|Decsription|
|---------|-----------|
|2. get_copernicus_land_cover|Download 100m spatial resolution copernicus global land cover rasters for years 2015 to 2019.|
|3. compute_land_cover|Compute area of copernicus land cover classes in sqkm.|
|4. get_gadm_shp|Download administrative polygons from GADM as a shapefile.|
|5. get_gadm_gpkg|Download administrative polygons from GADM as a geopackage.|
