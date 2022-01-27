#' Get Accessibility to cities raster datasets
#'
#' Download Accessibility to cities global rasters from the collection of 11 different layers for the year from 2015
#'
#' @param range range of urban population
#'
#' @param index index in which the file is stored in the database cloud
#'
#' @param path path location
#'
#' @importFrom utils download.file
#'
#' @return copernicus land cover rasters
#'
#' @export
#' @examples
#' f <- "../../Om/test/" # path to the folder where you want to download the rasters
#' get_accessibility("5k_10k", 14189840, f)
#' @export


get_accessibility <- function(range, index, path) {

  tryCatch(
    {
      # create URL
      url <- paste0("https://ndownloader.figshare.com/files/",index)
      # destination file to download the raster file
      destfile <- paste0(path, "/", range,".tif")
      # download command
      download.file(url, destfile)
    },
    error = function(e) {
      message('Error in this line!')
    }
  )
}
