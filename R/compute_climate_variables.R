#' Get 30s climate raster datasets - average temperature and precipitation (1970-2000)
#'
#' @param var use "tavg" for average temperature rasters and "prec" for precipitation rasters - pass as character value
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return climatic variable rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' # Download average temperature rasters
#' get_climate_rasters(var = "tavg", file_path=file_path)
#' }
#'
#' @export


get_climate_rasters <- function(var = NULL,
                                file_path = NULL) {
  tryCatch(
    {
      # create URL
      url <- paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_", var, ".zip")
      # destination file to download the raster file
      destfile <- paste0(file_path, "/", var, ".zip")
      # download command
      download.file(url, destfile)
      # unzip folder
      unzip(
        zipfile = paste0(file_path, "/", var, ".zip"),
        exdir = paste0(file_path, "/")
      )
      # delete zip
      unlink(paste0(file_path, "/*.zip"))
      print("rasters downloaded and unzipped successfully!")
    },
    error = function(e) {
      message("Re-check the arguments!")
    }
  )
}

