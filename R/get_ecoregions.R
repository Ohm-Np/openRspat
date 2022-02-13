#' Download terrestrial ecoregions of the world polygon
#'
#' It is a one-time downloadable data. The function first downloads the zip file, unzip it...
#' ...load the shapefile, convert the shapefile to geopackage and write the geopackage to file.
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return teow gpkg
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the polygon
#' # Download teow polygon
#' get_ecoregions(file_path=file_path)
#' }
#'
#' @export


get_ecoregions <- function(file_path = NULL) {
  tryCatch(
    {
      # create URL
      url <- paste0("https://opendata.arcgis.com/datasets/af92f20b1b43479581c819941e0f75ea_0.zip?outSR=%7B%22falseM%22%3A-100000%2C%22xyTolerance%22%3A8.983152841195215e-9%2C%22mUnits%22%3A10000%2C%22zUnits%22%3A10000%2C%22latestWkid%22%3A4326%2C%22zTolerance%22%3A0.001%2C%22wkid%22%3A4326%2C%22xyUnits%22%3A11258999068426.24%2C%22mTolerance%22%3A0.001%2C%22falseX%22%3A-400%2C%22falseY%22%3A-400%2C%22falseZ%22%3A-100000%7D")
      # destination file to download the zip file
      destfile <- paste0(file_path, "/teow_global.zip")
      # download command
      download.file(url, destfile)
      # unzip folder
      unzip(
        zipfile = paste0(file_path, "/teow_global.zip"),
        exdir = paste0(file_path, "/")
      )
      # delete zip
      unlink(paste0(file_path, "/*.zip"))
      print("rasters downloaded and unzipped successfully!")
      # load shapefile
      teow <-
        read_sf(paste0(file_path, "/Terrestrial_Ecoregions_World.shp"))
      # write to disk as geopackage
      st_write(teow,
               paste0(file_path, "/Terrestrial_Ecoregions_World.gpkg"))
    },
    error = function(e) {
      message("Re-check the arguments!")
    }
  )
}
