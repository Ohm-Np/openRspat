#' Download global mangrove watch polygon data
#'
#' The function first downloads the zip file, unzip it, load the shapefile...
#' ...convert the shapefile to geopackage and write the geopackage to file.
#'
#' @param yyyy year (1996, 2007, 2008, 2009, 2010, 2015, 2016)
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return gmw gpkg
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the polygon
#' # Download teow polygon
#' get_global_mangrove_watch(yyyy = 2007, file_path=file_path)
#' }
#'
#' @export


get_global_mangrove_watch <- function(yyyy = NULL,
                                      file_path = NULL) {

  if (yyyy  == 2007) {

    tryCatch(
      {
        # create url
        url <- paste0("https://wcmc.io/GMW_",yyyy)
        # create string for temporary file
        destfile <- paste0(file_path, "/global-mangrove-watch-",yyyy,".zip")
        # download the file and save it to temp folder
        download.file(url, destfile)
        # unzip
        unzip(
          zipfile = paste0(file_path, "/global-mangrove-watch-",yyyy,".zip"),
          exdir = paste0(file_path, "/")
        )
        # load shapefile
        gmw <-
          st_read(paste0(file_path, "/GMW_2007_v2.0.shp"))
        # write as geopackage
        st_write(gmw,
                 paste0(file_path, "/global-mangrove-watch-2007.gpkg"))
        # delete zip file
        unlink(paste0(file_path, "/global-mangrove-watch-2007.zip"))
      },
      error = function(e) {
        message('Re-check the arguments!')
      }
    )
  } else {

    tryCatch(
      {
        # create url
        url <- paste0("https://wcmc.io/GMW_",yyyy)
        # create string for temporary file
        destfile <- paste0(file_path, "/global-mangrove-watch-",yyyy,".zip")
        # download the file and save it to temp folder
        download.file(url, destfile)
        # unzip
        unzip(
          zipfile = paste0(file_path, "/global-mangrove-watch-",yyyy,".zip"),
          exdir = paste0(file_path, "/")
        )
        # load shapefile
        gmw <-
          st_read(paste0(file_path, "/GMW_001_GlobalMangroveWatch_",yyyy,"/01_Data/GMW_",yyyy,"_v2.shp"))
        # write as geopackage
        st_write(gmw,
                 paste0(file_path, "global-mangrove-watch-",yyyy,".gpkg"))
        # delete unnecessary files
        unlink(paste0(file_path, "/global-mangrove-watch-",yyyy,".zip"))
      },
      error = function(e) {
        message('Re-check the arguments!')
      }
    )
  }
}

