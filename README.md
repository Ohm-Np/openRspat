![](https://komarev.com/ghpvc/?username=Ohm-Np)
# openRspat

<!-- badges: start -->
<!-- badges: end -->

The objective of the package **openRspat** is to create API access to the free and open source geodatasets and provide functionalities to process those datasets. The aim is to provide users with an option to pass an argument which might be a polygon or WDPAID and return them the results they wanted in a form of data frame.

`Note: With every new functions being added to the repo, I will keep updating this readme file. You can still use the package for the functions mentioned below. Also, keep checking the readme file for the possible future updates.`

# Quickstart Guide
This section will take you through a tour on how to use this package on your R environment.  

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
aoi <- sf[1, ]
area_proj(aoi)
```
Other available functions in the package are listed here:
|Functions|Decsription|
|---------|-----------|
|`get_copernicus_land_cover`|Download 100m spatial resolution copernicus global land cover rasters for years 2015 to 2019 for the desired 20*20 grid.|
|`compute_land_cover`|Compute area of copernicus land cover classes in sqkm.|
|`get_gadm_shp`|Download administrative polygons from GADM as a shapefile.|
|`get_gadm_gpkg`|Download administrative polygons from GADM as a geopackage.|
|`zonal_operation`|Compute zonal statistics from raster on region of interest.|
|`area_computation`|Compute areal statistics from polygon on region of interest.|
|`get_accessibility`|Download Accessibility to cities' global rasters from the collection of 11 different raster layers for the year 2015.|
|`compute_accessibility`|Compute zonal statistics for travel time from the area of interest to the nearby cities.|
|`get_climate_rasters`|Download 30s climate raster datasets - average temperature and precipitation (1970-2000).|
|`get_global_mangrove_watch`|Download global mangrove watch polygon data as geopackage for different available timeframes.|
|`get_ecoregions`|Download global terrestrial ecoregions of the world polygon data as geopackage.|
|`get_pop_count`|Download 1km global population count rasters from WorldClim for years 2000 to 2020.|
|`get_clay_content`|Download global clay content rasters at three different available depths (0cm, 10cm, 30cm).|
|`get_drought_indicator`|Download weekly global NASA GRACE-based drought indicator rasters available from year 2003 to present.|
|`get_net_forest_carbon_flux`|Download Net Forest Carbon Flux rasters from Global Forest Watch (GFW) for the desired 10*10 grid.|
