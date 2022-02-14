#' Download global clay content rasters at three different depths
#'
#' @param depth depth in cm (0, 10, 30)
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return clay content rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' # Download clay content rasters
#' get_clay_content(depth = 30, file_path=file_path)
#' }
#'
#' @export

get_clay_content <- function(depth = NULL,
                             file_path = NULL) {

  tryCatch(
    {

      # create url
      url <- paste0("https://zenodo.org/record/2525663/files/sol_clay.wfraction_usda.3a1a1a_m_250m_b",depth,"..",depth,"cm_1950..2017_v0.2.tif?download=1")
      # create destination file path
      destfile <- paste0(file_path,
                         "/clay_content_",depth,"_cm.tif")
      # download the file to the file path
      download.file(url, destfile)
    },
    error = function(e) {
      message('Re-check the arguments!')
    }
  )
}

