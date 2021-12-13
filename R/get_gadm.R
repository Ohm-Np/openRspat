#' Get GADM country level datasets (geopackage)
#'
#' Download country level administrative datasets in geopackage
#'
#' @importFrom utils unzip
#'
#' @param x ISO3 country code
#'
#' @param f filepath
#'
#' @return gadm geopackage
#'
#' @examples
#' # give path to your destination folder
#' f <- "../../"
#' # call the function
#' get_gadm_gpkg("NPL", f)
#'
#' @export



get_gadm_gpkg <- function(x, f) {

  tryCatch(
    {

      # create url
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/gpkg/gadm36_",x,"_gpkg.zip")
      # create string for temporary file
      destfile <- paste0(f, "/", x,".zip")
      # download the file and save it to tempfolder
      download.file(url, destfile)
      # unzip folder
      unzip(zipfile = paste0(f, "/",x,".zip"),
            exdir = paste0(f, "/"))
      # delete zip
      unlink(paste0(f, "/*.zip"))
      unlink(paste0(f, "/license.txt"))
      print("geopackage download completed successfully!")
    },
    error = function(e) {
      message('Invalid ISO3 code!')
    }
  )
}



#' Get GADM country level datasets (shapefile)
#'
#' Download country level administrative datasets in ESRI shapefile format
#'
#' @param x ISO3 country code
#'
#' @param f filepath
#'
#' @return gadm shapefiles
#'
#' @examples
#' # give path to your destination folder
#' f <- "../../"
#' # call the function
#' get_gadm_shp("NPL", f)
#'
#' @export

get_gadm_shp <- function(x, f) {

  tryCatch(
    {

      # create url
      url <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_",x,"_shp.zip")
      # create string for temporary file
      destfile <- paste0(f,"/",x,".zip")
      # download the file and save it to tempfolder
      download.file(url, destfile)
      # unzip folder
      unzip(zipfile = paste0(f, "/",x,".zip"),
            exdir = paste0(f, "/"))
      # delete zip
      unlink(paste0(f,"/*.zip"))
      unlink(paste0(f,"/license.txt"))
      print("shapefile download completed successfully!")
    },
    error = function(e) {
      message('Invalid ISO3 code!')
    }
  )
}
