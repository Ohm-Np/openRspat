#' Get Accessibility to cities raster datasets
#'
#' Download Accessibility to cities global rasters from the collection of 11 different layers for the year from 2015
#'
#' The list of range of population layer and corresponding index to use:
#' "5k_10k", 14189840
#' "10k_20k", 14189837
#' "20k_50k", 14189831
#' "50k_100k", 14189825
#' "100k_200k", 14189819
#' "200k_500k", 14189816
#' "500k_1mio", 14189810
#' "1mio_5mio", 14189807
#' "50k_50mio", 14189849
#' "5k_110mio", 14189852
#' "20k_110mio", 14189843
#'
#' @param range range of urban population
#'
#' @param index index in which the file is stored in the database cloud
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return accessibility rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' get_accessibility(range="5k_10k", index=14189840, file_path=file_path)
#' }
#'
#' @export


get_accessibility <- function(range = NULL,
                              index = NULL,
                              file_path = NULL) {
  tryCatch(
    {
      # create URL
      url <- paste0("https://ndownloader.figshare.com/files/", index)
      # destination file to download the raster file
      destfile <- paste0(file_path, "/", range, ".tif")
      # download command
      download.file(url, destfile)
    },
    error = function(e) {
      message("Re-check the arguments!")
    }
  )
}


#' Compute zonal statistics for travel time from the area of interest to the nearby cities
#'
#' Returns the result in long-table format
#'
#' @param aoi area of interest - polygon (sf)
#'
#' @param acc_rast accessibility raster (terra)
#'
#' @param idcol A character vector identifying a column name which uniquely
#' identifies the polygons in the \code{aoi} object.
#'
#' @param opn A type of zonal operation (sum/mean/min/max) - mean being default value
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#'
#' @return zonal statistics
#'
#' @examples
#'
#' \donttest{
#' # load raster
#' r <- terra::rast(system.file("extdata", "test_acc_rast.tif", package = "openRspat"))
#' # load polygon
#' fname <- system.file("shape/nc.shp", package="sf")
#' sf <- sf::st_read(fname)
#' # provide unique id column
#' idcol <- sf[1, ]$CNTY_ID
#' # zonal operation type
#' opn <- "min"
#' # call function
#' compute_accessibility(aoi=sf[1,], acc_rast=r, idcol=idcol, opn=opn)
#' }
#'
#' @export
#'


compute_accessibility <- function(aoi = NULL,
                                  acc_rast = NULL,
                                  idcol = NULL,
                                  opn = "mean") {
  tryCatch(
    {
      zstats <-
        openRspat::zonal_operation(
          aoi = aoi,
          rast = acc_rast,
          idcol = idcol,
          opn = opn
        )
      travel_time_to_nearby_cities <- NULL
      df <- data.frame(
        ID = idcol,
        travel_time_to_nearby_cities = zstats[, 2]
      )
      zstats_longer <- tidyr::pivot_longer(df,
        cols = travel_time_to_nearby_cities
      )
      return(zstats_longer)
    },
    error = function(e) {
      message("Re-check arguments!")
    }
  )
}
