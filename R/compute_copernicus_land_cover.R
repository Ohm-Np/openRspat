#' Get Copernicus Global Land Cover
#'
#' Download Copernicus Global Land Cover rasters with 23 discrete classifications for the desired year from 2015 to 2019 and for the desired grid
#'
#' @param year year (2015 to 2019)
#'
#' @param grid 20*20 Latlon grid (e.g. "W120N40")
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return copernicus land cover rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' f <- ".." # path to the folder where you want to download the rasters
#' # Download global land cover rasters for the year 2019
#' get_copernicus_land_cover(year=2019, grid="W120N40", file_path=f)
#' }
#'
#' @export


get_copernicus_land_cover <- function(year = NULL,
                                      grid = NULL,
                                      file_path = NULL) {
  if (year == 2015) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-base_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(file_path, "/", year, "_", grid, ".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
  } else if (year == 2019) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-nrt_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(file_path, "/", year, "_", grid, ".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
  } else {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/", year, "/", grid, "/", grid, "_PROBAV_LC100_global_v3.0.1_", year, "-conso_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(file_path, "/", year, "_", grid, ".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
  }
}





#' Compute land cover statistics
#'
#' Compute copernicus global land cover statistics - 23 discrete classifications
#'
#' @param rast land cover raster
#'
#' @param aoi area of interest - polygon object (sf)
#'
#' @param year year
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#'
#' @return land cover classes
#'
#' @examples
#'
#' \donttest{
#' # load raster
#' r <- terra::rast(system.file("extdata", "test_lc_rast.tif", package = "openRspat"))
#' # load polygon
#' fname <- system.file("shape/nc.shp", package="sf")
#' sf <- sf::st_read(fname)
#' # transform sf object to match projection with raster object
#' sf <- sf::st_transform(sf, "+proj=longlat +datum=WGS84 +no_defs")
#' # call function
#' compute_land_cover(rast=r, aoi=sf[1,], year=2015)
#' }
#'
#' @export

compute_land_cover <- function(rast = NULL,
                               aoi = NULL,
                               year = NULL) {

  # crop the raster
  lc_rast_crop <- terra::crop(
    rast,
    aoi
  )
  # vect for terra compatibility
  p_v <-
    terra::vect(aoi)
  # mask the raster
  lc_rast_mask <- terra::mask(
    lc_rast_crop,
    p_v
  )
  # store raster values as dataframe
  df <-
    terra::as.data.frame(lc_rast_mask)
  # new dataframe with value column - to change the column name to value
  lc_value <- NULL
  df.new <-
    data.frame(lc_value = NA)
  # rename column to match with new df where raster values are stored
  colnames(df) <-
    colnames(df.new)
  # area of masked raster in km
  area_sqkm <- terra::expanse(lc_rast_mask,
    unit = "km"
  )
  # area per row of dataframe
  area_sqkm_per_cell <-
    area_sqkm / nrow(df)
  # set UID
  aoi <-
    tibble::rowid_to_column(aoi, "UID")
  # create empty df to receive results
  df.final <-
    data.frame(aoi$UID)


  # discrete classification and respective area computation - map code represents respective class name
  ### empty classes
  empty <- df %>%
    filter(lc_value %in% 0) %>%
    nrow()
  df.final$copernicus_lc_empty_area_sqkm <- area_sqkm_per_cell * empty
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_empty_area_sqkm"] <-
    paste0("copernicus_lc_empty_area_sqkm_", year)

  ### 111 Closed forest, evergreen needle leaf
  cfenl <- df %>%
    filter(lc_value %in% 111) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell * cfenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_", year)

  ### 113 closed forest, deciduous needle leaf
  cfdnl <- df %>%
    filter(lc_value %in% 113) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell * cfdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_", year)

  ### 112 closed forest, evergreen, broad leaf
  cfebl <- df %>%
    filter(lc_value %in% 112) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell * cfebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_", year)

  ### 114 closed forest, deciduous broad leaf
  cfdbl <- df %>%
    filter(lc_value %in% 114) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell * cfdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_", year)

  ### 115 closed forest, mixed
  cfm <- df %>%
    filter(lc_value %in% 115) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_mixed_area_sqkm <- area_sqkm_per_cell * cfm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_mixed_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_mixed_area_sqkm_", year)

  ### 116 closed forest, unknown
  cfu <- df %>%
    filter(lc_value %in% 116) %>%
    nrow()
  df.final$copernicus_lc_closed_forest_unknown_area_sqkm <- area_sqkm_per_cell * cfu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_closed_forest_unknown_area_sqkm"] <-
    paste0("copernicus_lc_closed_forest_unknown_area_sqkm_", year)

  ### 121 Open forest, evergreen needle leaf
  ofenl <- df %>%
    filter(lc_value %in% 121) %>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm <- area_sqkm_per_cell * ofenl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_", year)

  ### 123 Open forest, deciduous needle leaf
  ofdnl <- df %>%
    filter(lc_value %in% 123) %>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm <- area_sqkm_per_cell * ofdnl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_", year)

  ### 122 Open forest, evergreen broad leaf
  ofebl <- df %>%
    filter(lc_value %in% 122) %>%
    nrow()
  df.final$copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm <- area_sqkm_per_cell * ofebl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_", year)

  ### 124 Open forest, deciduous broad leaf
  ofdbl <- df %>%
    filter(lc_value %in% 124) %>%
    nrow()
  df.final$copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm <- area_sqkm_per_cell * ofdbl
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_", year)

  ### 125 Open forest, mixed
  ofm <- df %>%
    filter(lc_value %in% 125) %>%
    nrow()
  df.final$copernicus_lc_open_forest_mixed_area_sqkm <- area_sqkm_per_cell * ofm
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_mixed_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_mixed_area_sqkm_", year)

  ### 126 Open forest, unknown
  ofu <- df %>%
    filter(lc_value %in% 126) %>%
    nrow()
  df.final$copernicus_lc_open_forest_unknown_area_sqkm <- area_sqkm_per_cell * ofu
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_forest_unknown_area_sqkm"] <-
    paste0("copernicus_lc_open_forest_unknown_area_sqkm_", year)

  ### 20 Shrubs
  shrubs <- df %>%
    filter(lc_value %in% 20) %>%
    nrow()
  df.final$copernicus_lc_shrubs_area_sqkm <- area_sqkm_per_cell * shrubs
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_shrubs_area_sqkm"] <-
    paste0("copernicus_lc_shrubs_area_sqkm_", year)

  ### 30 herbaceous vegetation
  herb_veg <- df %>%
    filter(lc_value %in% 30) %>%
    nrow()
  df.final$copernicus_lc_herbaceous_vegetation_area_sqkm <- area_sqkm_per_cell * herb_veg
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_vegetation_area_sqkm"] <-
    paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_", year)

  ### 90 herbaceous wetland
  herb_wet <- df %>%
    filter(lc_value %in% 90) %>%
    nrow()
  df.final$copernicus_lc_herbaceous_wetland_area_sqkm <- area_sqkm_per_cell * herb_wet
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_herbaceous_wetland_area_sqkm"] <-
    paste0("copernicus_lc_herbaceous_wetland_area_sqkm_", year)

  ### 100 Moss and lichen
  moss <- df %>%
    filter(lc_value %in% 100) %>%
    nrow()
  df.final$copernicus_lc_moss_area_sqkm <- area_sqkm_per_cell * moss
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_moss_area_sqkm"] <-
    paste0("copernicus_lc_moss_area_sqkm_", year)

  ### 60 Bare / sparse vegetation
  bsv <- df %>%
    filter(lc_value %in% 60) %>%
    nrow()
  df.final$copernicus_lc_bare_sparse_vegetation_area_sqkm <- area_sqkm_per_cell * bsv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_bare_sparse_vegetation_area_sqkm"] <-
    paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_", year)

  ### 40 Cultivated and managed vegetation/agriculture (cropland)
  cmv <- df %>%
    filter(lc_value %in% 40) %>%
    nrow()
  df.final$copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm <- area_sqkm_per_cell * cmv
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm"] <-
    paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_", year)

  ### 50 Urban / Built up
  urban <- df %>%
    filter(lc_value %in% 50) %>%
    nrow()
  df.final$copernicus_lc_urban_built_up_area_sqkm <- area_sqkm_per_cell * urban
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_urban_built_up_area_sqkm"] <-
    paste0("copernicus_lc_urban_built_up_area_sqkm_", year)

  ### 70 Snow and Ice
  snow <- df %>%
    filter(lc_value %in% 70) %>%
    nrow()
  df.final$copernicus_lc_snow_and_ice_area_sqkm <- area_sqkm_per_cell * snow
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_snow_and_ice_area_sqkm"] <-
    paste0("copernicus_lc_snow_and_ice_area_sqkm_", year)

  ### 80 Permanent water bodies
  pwb <- df %>%
    filter(lc_value %in% 80) %>%
    nrow()
  df.final$copernicus_lc_permanent_water_bodies_area_sqkm <- area_sqkm_per_cell * pwb
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_permanent_water_bodies_area_sqkm"] <-
    paste0("copernicus_lc_permanent_water_bodies_area_sqkm_", year)

  ### 200 Open Sea
  sea <- df %>%
    filter(lc_value %in% 200) %>%
    nrow()
  df.final$copernicus_lc_open_sea_area_sqkm <- area_sqkm_per_cell * sea
  # rename the column to store value per year
  names(df.final)[names(df.final) == "copernicus_lc_open_sea_area_sqkm"] <-
    paste0("copernicus_lc_open_sea_area_sqkm_", year)

  # pivot resulting dataframe to long format
  df.final_long <- pivot_longer(df.final,
    cols = c(
      paste0("copernicus_lc_empty_area_sqkm_", year), paste0("copernicus_lc_closed_forest_evergreen_needle_leaf_area_sqkm_", year),
      paste0("copernicus_lc_closed_forest_deciduous_needle_leaf_area_sqkm_", year), paste0("copernicus_lc_closed_forest_evergreen_broad_leaf_area_sqkm_", year),
      paste0("copernicus_lc_closed_forest_deciduous_broad_leaf_area_sqkm_", year), paste0("copernicus_lc_closed_forest_mixed_area_sqkm_", year),
      paste0("copernicus_lc_closed_forest_unknown_area_sqkm_", year), paste0("copernicus_lc_open_forest_evergreen_needle_leaf_area_sqkm_", year),
      paste0("copernicus_lc_open_forest_deciduous_needle_leaf_area_sqkm_", year), paste0("copernicus_lc_open_forest_evergreen_broad_leaf_area_sqkm_", year),
      paste0("copernicus_lc_open_forest_deciduous_broad_leaf_area_sqkm_", year), paste0("copernicus_lc_open_forest_mixed_area_sqkm_", year),
      paste0("copernicus_lc_open_forest_unknown_area_sqkm_", year), paste0("copernicus_lc_shrubs_area_sqkm_", year),
      paste0("copernicus_lc_herbaceous_vegetation_area_sqkm_", year), paste0("copernicus_lc_herbaceous_wetland_area_sqkm_", year),
      paste0("copernicus_lc_moss_area_sqkm_", year), paste0("copernicus_lc_bare_sparse_vegetation_area_sqkm_", year),
      paste0("copernicus_lc_cultivated_managed_vegetation_agriculture_area_sqkm_", year), paste0("copernicus_lc_urban_built_up_area_sqkm_", year),
      paste0("copernicus_lc_snow_and_ice_area_sqkm_", year), paste0("copernicus_lc_permanent_water_bodies_area_sqkm_", year),
      paste0("copernicus_lc_open_sea_area_sqkm_", year)
    )
  )
  # return results
  return(df.final_long)
}
