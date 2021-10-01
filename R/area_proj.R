#' Area Projection
#'
#' Get LAEA projection strings from the provided sf object
#'
#' @param x sf object
#'
#' @return LAEA projection string
#'
#' @examples
#' x <- spData::us_states[1,]
#' area_proj(x)
#'
#' @export

area_proj <- function(x) {
  bb <- sf::st_bbox(x)
  cntr_long <- (bb[3] - bb[1]) * 0.5 + bb[1]
  cntr_lat <- (bb[4] - bb[2]) * 0.5 + bb[2]
  bbm <- bb * 111000
  rng_x <- round(bbm[3] - bbm[1])
  rng_y <- round(bbm[4] - bbm[2])
  paste0("+proj=laea +lat_0=",
         cntr_lat,
         " +lon_0=",
         cntr_long,
         " +x_0=",
         rng_x,
         " +y_0=",
         rng_y,
         " +a=6371007.181 +b=6371007.181 +units=m +no_defs")
}
