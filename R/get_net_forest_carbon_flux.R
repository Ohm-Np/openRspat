#' Get Net Forest Carbon Flux rasters (carbon balance)
#'
#' Download Net Forest Carbon Flux rasters from Global Forest Watch (GFW) for the desired grid
#' Please register at GFW first from your PC in order to access the API to raster files
#'
#' @param grid 10*10 Latlon grid (e.g. "00N_040W")
#'
#' @param file_path path location
#'
#' @importFrom utils download.file
#'
#' @return carbon balance rasters
#'
#' @export
#' @examples
#'
#' \donttest{
#' file_path <- ".." # path to the folder where you want to download the rasters
#' # Download carbon balance rasters for the grid 00N,040W
#' get_net_forest_carbon_flux(grid="00N_040W", file_path=file_path)
#' }
#'
#' @export


get_net_forest_carbon_flux <- function(grid = NULL,
                                       file_path = NULL) {

  tryCatch(
    {

      # create url
      url <- paste0("https://data-api.globalforestwatch.org/dataset/gfw_forest_carbon_net_flux/v20210331/download/geotiff?grid=10/40000&tile_id=",grid,"&pixel_meaning=Mg_CO2e_ha")
      # create destination file path
      destfile <- paste0(file_path,
                         "/",grid,".tif")
      # download the ratser file
      download.file(url,
                    destfile)
    },
    error = function(e) {
      message('Re-check the arguments!')
    }
  )
}

