#' Compute area statistics for region of interest on desired polygon datasets
#'
#' @param roi An sf object with polygons representing region of interest
#'
#' @param data An sf object of which area is to be computed
#'
#' @param unit A character vector defining whether the result area should be in meter ("m") or kilometer ("km")
#'
#' @import sf
#'
#' @return A dataframe with areal statistics
#'
#' @examples
#'
#' # load wdpa polygon
#' pa <- sf::st_read(system.file("extdata", "test_wdpa.gpkg", package = "openRspat"))
#' # load test data
#' teow <- sf::st_read(system.file("extdata", "test_ecoregions.gpkg", package = "openRspat"))
#' # define unit
#' unit <- "m"
#' # call function
#' area_computation(pa, teow, unit)
#'
#' @export

area_computation <- function(roi, data, unit) {

  # transform crs of pol to area_proj()
  roi <- st_transform(roi,
                      area_proj(roi))
  # transform crs of vec to match with polygon
  data <- st_transform(data,
                      st_crs(roi))
  # perform intersection
  intersected <-
    st_intersection(data,
                    roi)
  if (unit=="km") {
    # compute area
    intersected$area_sqkm <-
      as.character(st_area(intersected)/1000000)
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
#' @param x sf object
#'
#' @return LAEA projection string
#'
#' @export
#' @examples
#' fname <- system.file("shape/nc.shp", package="sf")
#' sf <- sf::st_read(fname)
#' x <- sf[1, ]
#' area_proj(x)
#' @export

area_proj <- function(x) {

  bb <- sf::st_bbox(x)
  cntr_long <- (bb[3] - bb[1]) * 0.5 + bb[1]
  cntr_lat <- (bb[4] - bb[2]) * 0.5 + bb[2]
  bbm <- bb * 111000
  rng_x <- round(bbm[3] - bbm[1])
  rng_y <- round(bbm[4] - bbm[2])
  paste0("+proj=laea +lat_0=",
         cntr_lat,
         " +lon_0=",
         cntr_long,
         " +x_0=",
         rng_x,
         " +y_0=",
         rng_y,
         " +a=6371007.181 +b=6371007.181 +units=m +no_defs")
}

