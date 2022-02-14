#' Download weekly global NASA GRACE-based drought indicator rasters
#'
#' @param yyyymmdd timeframe - pass date value as e.g. 20020203 (for february 3rd, 2002)
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return drought indicator rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' # Download drought indicator rasters for 31st January, 2022
#' get_drought_indicator(yyyymmdd = 20220131, file_path=file_path)
#' }
#'
#' @export

get_drought_indicator <- function(yyyymmdd = NULL,
                                  file_path = NULL) {

  tryCatch(
    {

      # create the url
      url <- paste0("https://nasagrace.unl.edu/globaldata/",yyyymmdd,"/gws_perc_025deg_GL_",yyyymmdd,".tif")
      # create destination file path
      destfile <- paste0(file_path,
                         "/gws_perc_025deg_GL_",yyyymmdd,".tif")
      # download the file
      download.file(url, destfile)
    },

    error = function(e) {
      message('Re-check the arguments!')
    }
  )
}

