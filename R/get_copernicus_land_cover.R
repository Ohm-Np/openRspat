#' Get Copernicus Global Land Cover
#'
#' Download Copernicus Global Land Cover rasters with 23 discrete classifications for the desired year from 2015 to 2019 and for the desired grid
#'
#' @param y year (2015 to 2019)
#'
#' @param g Latlon grid
#'
#' @param f path
#'
#' @importFrom utils download.file
#'
#' @return copernicus land cover rasters
#'
#' @export
#' @examples
#' f <- "../../" # path to the folder where you want to download the rasters
#' get_copernicus_land_cover(2015, "W120N40", f)
#' @export


get_copernicus_land_cover <- function(y, g, f) {

  if (y == 2015) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-base_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f, "/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")

  } else if (y == 2019) {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-nrt_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f, "/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")

  } else {

    # create url
    url <- paste0("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/",y,"/",g,"/",g,"_PROBAV_LC100_global_v3.0.1_",y,"-conso_Discrete-Classification-map_EPSG-4326.tif")
    # create string for temporary file
    destfile <- paste0(f,"/",y,"_",g,".tif")
    # download the file and save it to tempfolder
    download.file(url, destfile)
    print("download completed!")
  }
}
