#' Get GADM country level datasets (geopackage)
#'
#' Download country level administrative datasets in geopackage
#'
#' @importFrom utils unzip
#'
#' @param iso ISO3 country code
#'
#' @param file_path filepath
#'
#' @return gadm geopackage
#'
#' @examples
#'
#' \donttest{
#' # give path to your destination folder
#' file_path <- ".."
#' # call the function
#' get_gadm_gpkg(iso="NPL", file_path=f)
#' }
#'
#' @export



get_gadm_gpkg <- function(iso = NULL,
                          file_path = NULL) {
  tryCatch(
    {

      # create url
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_", iso, "_gpkg.zip")
      # create string for temporary file
      destfile <- paste0(file_path, "/", iso, ".zip")
      # download the file and save it to tempfolder
      download.file(url, destfile)
      # unzip folder
      unzip(
        zipfile = paste0(file_path, "/", iso, ".zip"),
        exdir = paste0(file_path, "/")
      )
      # delete zip
      unlink(paste0(file_path, "/*.zip"))
      unlink(paste0(file_path, "/license.txt"))
      print("geopackage download completed successfully!")
    },
    error = function(e) {
      message("Invalid ISO3 code OR invalid file path!")
    }
  )
}



#' Get GADM country level datasets (shapefile)
#'
#' Download country level administrative datasets in ESRI shapefile format
#'
#' @param iso ISO3 country code
#'
#' @param file_path filepath
#'
#' @return gadm shapefiles
#'
#' @examples
#'
#' \donttest{
#' # give path to your destination folder
#' file_path <- ".."
#' # call the function
#' get_gadm_shp(iso="NPL", file_path=file_path)
#' }
#'
#' @export

get_gadm_shp <- function(iso = NULL,
                         file_path = NULL) {
  tryCatch(
    {

      # create url
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_", iso, "_shp.zip")
      # create string for temporary file
      destfile <- paste0(file_path, "/", iso, ".zip")
      # download the file and save it to tempfolder
      download.file(url, destfile)
      # unzip folder
      unzip(
        zipfile = paste0(file_path, "/", iso, ".zip"),
        exdir = paste0(file_path, "/")
      )
      # delete zip
      unlink(paste0(file_path, "/*.zip"))
      unlink(paste0(file_path, "/license.txt"))
      print("shapefile download completed successfully!")
    },
    error = function(e) {
      message("Invalid ISO3 code OR invalid file path!")
    }
  )
}
