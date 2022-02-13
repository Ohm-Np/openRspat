#' Get 1km global population count rasters
#'
#' @param yyyy year (from 2000 to 2020)
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return population count rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' # Download population count rasters
#' get_pop_count(yyyy = 2020, file_path=file_path)
#' }
#'
#' @export

get_pop_count <- function(yyyy = NULL,
                          file_path = NULL) {

  tryCatch(
    {

      # create the url
      url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020/",yyyy,"/0_Mosaicked/ppp_",yyyy,"_1km_Aggregated.tif")
      # create destination file path
      destfile <- paste0(file_path,
                         "/global_mosaic-",yyyy,".tif")
      # download the file
      download.file(url, destfile)
    },

    error = function(e) {
      message('Re-check the arguments!')
    }
  )
}

