#' Get Copernicus Global Land Cover
#'
#' Download Copernicus Global Land Cover rasters with 23 discrete classifications for the desired year from 2015 to 2019 and for the desired grid
#'
#' @param y year (2015 to 2019)
#'
#' @param g Latlon grid
#'
#' @param f path
#'
#' @importFrom utils download.file
#'
#' @return copernicus land cover rasters
#'
#' @export
#' @examples
#' f <- ".." # path to the folder where you want to download the rasters
#' get_copernicus_land_cover(2015, "W120N40", f)
#' @export


get_copernicus_land_cover <- function(y, g, f) {

  if (y == 2015) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-base_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f, "/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")

  } else if (y == 2019) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-nrt_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f, "/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")

  } else {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-conso_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f,"/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
  }
}





#' Compute land cover statistics
#'
#' Compute copernicus global land cover statistics - 23 discrete classifications
#'
#' @param r land cover raster
#'
#' @param p polygon object (sf)
#'
#' @param y year
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#'
#' @return land cover classes
#'
#' @examples
#'
#' # load raster
#' r <- terra::rast(system.file("extdata", "test_lc_rast.tif", package = "openRspat"))
#' # load polygon
#' fname <- system.file("shape/nc.shp", package="sf")
#' sf <- sf::st_read(fname)
#' # transform sf object to match projection with raster object
#' sf <- sf::st_transform(sf, "+proj=longlat +datum=WGS84 +no_defs")
#' # call function
#' compute_land_cover(r, sf[1,], 2015)
#'
#' @export

compute_land_cover <- function(r, p, y) {

  # crop the raster
  lc_rast_crop <- terra::crop(r,
                              p)
  # vect for terra compatibility
  p_v <-
    terra::vect(p)
  # mask the raster
  lc_rast_mask <- terra::mask(lc_rast_crop,
                              p_v)
  # store raster values as dataframe
  df <-
    terra::as.data.frame(lc_rast_mask)
  # new dataframe with value column - to change the column name to value
  lc_value <- NULL
  df.new <-
    data.frame(lc_value=NA)
  # rename column to match with new df where raster values are stored
  colnames(df) <-
    colnames(df.new)
  # area of masked raster in km
  area_sqkm <- terra::expanse(lc_rast_mask,
                              unit="km")
  # area per row of dataframe
  area_sqkm_per_cell <-
    area_sqkm/nrow(df)
  # set UID
  p <-
    tibble::rowid_to_column(p, "UID")
  # create empty df to receive results
  df.final <-
    data.frame(p$UID)


  # discrete classification and respective area computation - map code represents respective class name
  ### empty classes
  empty <- df%>%
    filter(lc_value %in% 0)%>%
    nrow()
  df.final$copernicus_lc_empty_area_sqkm <- area_sqkm_per_cell*empty
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_empty_area_sqkm"] <-
    paste0("copernicus_lc_empty_area_sqkm_",y)

  ### 111 Closed forest, evergreen needle leaf
  cfenl <- df%>%
    filter(lc_value %in% 111)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell*cfenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_",y)

  ### 113 closed forest, deciduous needle leaf
  cfdnl <- df%>%
    filter(lc_value %in% 113)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell*cfdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_",y)

  ### 112 closed forest, evergreen, broad leaf
  cfebl <- df%>%
    filter(lc_value %in% 112)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell*cfebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_",y)

  ### 114 closed forest, deciduous broad leaf
  cfdbl <- df%>%
    filter(lc_value %in% 114)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell*cfdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_",y)

  ### 115 closed forest, mixed
  cfm <- df%>%
    filter(lc_value %in% 115)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_mixed_area_sqkm <- area_sqkm_per_cell*cfm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_mixed_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_mixed_area_sqkm_",y)

  ### 116 closed forest, unknown
  cfu <- df%>%
    filter(lc_value %in% 116)%>%
    nrow()
  df.final$copernicus_lc_closed_forest_unknown_area_sqkm <- area_sqkm_per_cell*cfu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_unknown_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_unknown_area_sqkm_",y)

  ### 121 Open forest, evergreen needle leaf
  ofenl <- df%>%
    filter(lc_value %in% 121)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell*ofenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_",y)

  ### 123 Open forest, deciduous needle leaf
  ofdnl <- df%>%
    filter(lc_value %in% 123)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell*ofdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_",y)

  ### 122 Open forest, evergreen broad leaf
  ofebl <- df%>%
    filter(lc_value %in% 122)%>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell*ofebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_",y)

  ### 124 Open forest, deciduous broad leaf
  ofdbl <- df%>%
    filter(lc_value %in% 124)%>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell*ofdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_",y)

  ### 125 Open forest, mixed
  ofm <- df%>%
    filter(lc_value %in% 125)%>%
    nrow()
  df.final$copernicus_lc_open_forest_mixed_area_sqkm <- area_sqkm_per_cell*ofm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_mixed_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_mixed_area_sqkm_",y)

  ### 126 Open forest, unknown
  ofu <- df%>%
    filter(lc_value %in% 126)%>%
    nrow()
  df.final$copernicus_lc_open_forest_unknown_area_sqkm <- area_sqkm_per_cell*ofu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_unknown_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_unknown_area_sqkm_",y)

  ### 20 Shrubs
  shrubs <- df%>%
    filter(lc_value %in% 20)%>%
    nrow()
  df.final$copernicus_lc_shrubs_area_sqkm <- area_sqkm_per_cell*shrubs
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_shrubs_area_sqkm"] <-
    paste0("copernicus_lc_shrubs_area_sqkm_",y)

  ### 30 herbaceous vegetation
  herb_veg <- df%>%
    filter(lc_value %in% 30)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_vegetation_area_sqkm <- area_sqkm_per_cell*herb_veg
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_vegetation_area_sqkm"] <-
    paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_",y)

  ### 90 herbaceous wetland
  herb_wet <- df%>%
    filter(lc_value %in% 90)%>%
    nrow()
  df.final$copernicus_lc_herbaceous_wetland_area_sqkm <- area_sqkm_per_cell*herb_wet
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_wetland_area_sqkm"] <-
    paste0("copernicus_lc_herbaceous_wetland_area_sqkm_",y)

  ### 100 Moss and lichen
  moss <- df%>%
    filter(lc_value %in% 100)%>%
    nrow()
  df.final$copernicus_lc_moss_area_sqkm <- area_sqkm_per_cell*moss
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_moss_area_sqkm"] <-
    paste0("copernicus_lc_moss_area_sqkm_",y)

  ### 60 Bare / sparse vegetation
  bsv <- df%>%
    filter(lc_value %in% 60)%>%
    nrow()
  df.final$copernicus_lc_bare_sparse_vegetation_area_sqkm <- area_sqkm_per_cell*bsv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_bare_sparse_vegetation_area_sqkm"] <-
    paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_",y)

  ### 40 Cultivated and managed vegetation/agriculture (cropland)
  cmv <- df%>%
    filter(lc_value %in% 40)%>%
    nrow()
  df.final$copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm <- area_sqkm_per_cell*cmv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm"] <-
    paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_",y)

  ### 50 Urban / Built up
  urban <- df%>%
    filter(lc_value %in% 50)%>%
    nrow()
  df.final$copernicus_lc_urban_built_up_area_sqkm <- area_sqkm_per_cell*urban
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_urban_built_up_area_sqkm"] <-
    paste0("copernicus_lc_urban_built_up_area_sqkm_",y)

  ### 70 Snow and Ice
  snow <- df%>%
    filter(lc_value %in% 70)%>%
    nrow()
  df.final$copernicus_lc_snow_and_ice_area_sqkm <- area_sqkm_per_cell*snow
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_snow_and_ice_area_sqkm"] <-
    paste0("copernicus_lc_snow_and_ice_area_sqkm_",y)

  ### 80 Permanent water bodies
  pwb <- df%>%
    filter(lc_value %in% 80)%>%
    nrow()
  df.final$copernicus_lc_permanent_water_bodies_area_sqkm <- area_sqkm_per_cell*pwb
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_permanent_water_bodies_area_sqkm"] <-
    paste0("copernicus_lc_permanent_water_bodies_area_sqkm_",y)

  ### 200 Open Sea
  sea <- df%>%
    filter(lc_value %in% 200)%>%
    nrow()
  df.final$copernicus_lc_open_sea_area_sqkm <- area_sqkm_per_cell*sea
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_sea_area_sqkm"] <-
    paste0("copernicus_lc_open_sea_area_sqkm_",y)

  # pivot resulting dataframe to long format
  df.final_long <- pivot_longer(df.final,
                                cols=c(paste0("copernicus_lc_empty_area_sqkm_",y), paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_",y), paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_",y), paste0("copernicus_lc_closed_forest_mixed_area_sqkm_",y),
                                       paste0("copernicus_lc_closed_forest_unknown_area_sqkm_",y), paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_",y), paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_",y), paste0("copernicus_lc_open_forest_mixed_area_sqkm_",y),
                                       paste0("copernicus_lc_open_forest_unknown_area_sqkm_",y), paste0("copernicus_lc_shrubs_area_sqkm_",y),
                                       paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_",y), paste0("copernicus_lc_herbaceous_wetland_area_sqkm_",y),
                                       paste0("copernicus_lc_moss_area_sqkm_",y), paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_",y),
                                       paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_",y), paste0("copernicus_lc_urban_built_up_area_sqkm_",y),
                                       paste0("copernicus_lc_snow_and_ice_area_sqkm_",y), paste0("copernicus_lc_permanent_water_bodies_area_sqkm_",y),
                                       paste0("copernicus_lc_open_sea_area_sqkm_",y)))
  # return results
  return(df.final_long)
}
