#' Compute zonal statistics from provided raster files
#'
#' @param pol An sf object with polygons representing areas of interest
#'
#' @param idcol A character vector identifying a column name which uniquely
#' identifies the polygons in the \code{pol} object.
#'
#' @param rast A raster object as spatRaster on which to perform zonal operation
#'
#' @param opn A type of zonal operation (e.g. sum, mean, min, max)
#'
#' @import sf
#' @import terra
#'
#' @return A dataframe with zonal statistics
#'
#' @examples
#'
#' # load raster
#' rast <- terra::rast(system.file("extdata", "test_worldpop2020.tif", package = "openRspat"))
#' # load polygon
#' pol <- sf::st_read(system.file("extdata", "test_wdpa.gpkg", package = "openRspat"))
#' # provide unique id column
#' idcol <- "WDPAID"
#' # zonal operation type
#' opn <- "min"
#' # call function
#' zonal_operation(pol, idcol, rast, opn)
#'
#' @export

zonal_operation <- function(pol, idcol, rast, opn) {

  # transform crs of pol to area_proj()
  pol <- st_transform(pol,
                      "+proj=longlat +datum=WGS84 +no_defs")
  # convert sf to spatvector
  pol_v <-
    vect(pol)
  # crop the raster
  crop <- crop(rast,
               pol_v)
  # mask the raster
  mask <- mask(crop,
               pol_v)
  # rasterize the polygon
  r <- rasterize(pol_v,
                 mask,
                 idcol)
  # compute zonal statistics
  z <- zonal(mask,
             r,
             fun=opn,
             na.rm=T)
  # return zonal statistics as data frame
  return(z)
}
