#' Compute area statistics for region of interest on desired polygon datasets
#'
#' @param aoi area of interest - polygon object (sf)
#'
#' @param target_data An sf object of which area is to be computed
#'
#' @param unit A character vector defining whether the result area should be in meter ("m") or kilometer ("km") - meter being default value
#'
#' @import sf
#'
#' @return A dataframe with areal statistics
#'
#' @examples
#'
#' \donttest{
#' # load wdpa polygon
#' pa <- sf::st_read(system.file("extdata", "test_wdpa.gpkg", package = "openRspat"))
#' # load test data
#' teow <- sf::st_read(system.file("extdata", "test_eoregions.gpkg", package = "openRspat"))
#' # define unit
#' unit <- "m"
#' # call function
#' area_computation(aoi=pa, target_data=teow, unit=unit)
#' }
#'
#' @export

area_computation <- function(aoi = NULL,
                             target_data = NULL,
                             unit = "m") {

  # transform crs of pol to area_proj()
  aoi <- st_transform(
    aoi,
    area_proj(aoi)
  )
  # transform crs of vec to match with polygon
  target_data <- st_transform(
    target_data,
    st_crs(aoi)
  )
  # perform intersection
  intersected <-
    st_intersection(
      target_data,
      aoi
    )
  if (unit == "km") {
    # compute area
    intersected$area_sqkm <-
      as.character(st_area(intersected) / 1000000)
  } else {
    # compute area
    intersected$area_sqm <-
      as.character(st_area(intersected))
  }
  # drop geometry
  area_stats <- st_drop_geometry(intersected)
  # return results
  return(area_stats)
}



#' Area Projection
#'
#' Get LAEA projection strings from the provided sf object
#'
#' @param aoi area of interest - polygon object (sf)
#'
#' @return LAEA projection string
#'
#' @export
#' @examples
#'
#' \donttest{
#' fname <- system.file("shape/nc.shp", package="sf")
#' sf <- sf::st_read(fname)
#' aoi <- sf[1, ]
#' area_proj(aoi)
#' }
#'
#' @export

area_proj <- function(aoi = NULL) {
  bb <- sf::st_bbox(aoi)
  cntr_long <- (bb[3] - bb[1]) * 0.5 + bb[1]
  cntr_lat <- (bb[4] - bb[2]) * 0.5 + bb[2]
  bbm <- bb * 111000
  rng_x <- round(bbm[3] - bbm[1])
  rng_y <- round(bbm[4] - bbm[2])
  paste0(
    "+proj=laea +lat_0=",
    cntr_lat,
    " +lon_0=",
    cntr_long,
    " +x_0=",
    rng_x,
    " +y_0=",
    rng_y,
    " +a=6371007.181 +b=6371007.181 +units=m +no_defs"
  )
}

