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
