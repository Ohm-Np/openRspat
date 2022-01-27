![](https://komarev.com/ghpvc/?username=Ohm-Np)
# openRspat

<!-- badges: start -->
<!-- badges: end -->

The obejctive of the package **openRspat** is to create API access to the free and open source geodatasets and provide functionalities to process those datasets. The aim is to provide users with an option to pass an argument which might be a polygon or WDPAID and return them the results they wanted in a form of data frame.

`Note: With every new functions being added to the repo, I will keep updating this readme file. Keep in mind, you can still use the package for the functions mentioned below. Also, keep checking the readme file for the possible future updates.`

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
|`get_copernicus_land_cover`|Download 100m spatial resolution copernicus global land cover rasters for years 2015 to 2019.|
|`compute_land_cover`|Compute area of copernicus land cover classes in sqkm.|
|`get_gadm_shp`|Download administrative polygons from GADM as a shapefile.|
|`get_gadm_gpkg`|Download administrative polygons from GADM as a geopackage.|
|`zonal_operation`|Compute zonal statistics from raster on region of interest.|
|`area_computation`|Compute areal statistics from polygon on region of interest.|
