#' Compute zonal statistics from provided raster files
#'
#' @param aoi An sf object with polygons representing areas of interest
#'
#' @param idcol A character vector identifying a column name which uniquely
#' identifies the polygons in the \code{aoi} object.
#'
#' @param rast A raster object as spatRaster on which to perform zonal operation
#'
#' @param opn A type of zonal operation (sum/mean/min/max) - mean being default value
#'
#' @import sf
#' @import terra
#'
#' @return A dataframe with zonal statistics
#'
#' @examples
#'
#' \donttest{
#' # load raster
#' rast <- terra::rast(system.file("extdata", "test_worldpop2020.tif", package = "openRspat"))
#' # load polygon
#' aoi <- sf::st_read(system.file("extdata", "test_wdpa.gpkg", package = "openRspat"))
#' # provide unique id column
#' idcol <- "WDPAID"
#' # zonal operation type
#' opn <- "min"
#' # call function
#' zonal_operation(aoi, idcol, rast, opn)
#' }
#'
#' @export

zonal_operation <- function(aoi=NULL,
                            idcol=NULL,
                            rast=NULL,
                            opn="mean") {

  # transform crs of aoi to area_proj()
  aoi <- st_transform(aoi,
                      "+proj=longlat +datum=WGS84 +no_defs")
  # convert sf to spatvector
  aoi_v <-
    vect(aoi)
  # crop the raster
  crop <- crop(rast,
               aoi_v)
  # mask the raster
  mask <- mask(crop,
               aoi_v)
  # rasterize the polygon
  r <- rasterize(aoi_v,
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
