#' geolinecompare
#'
#' @description This function takes a LINESTRING simple feature in WGS84 and creates its copy in an equirectangular projection as if the ellipsoidal coordinates are treated as metrics and not as spherical. It returns a list as s3 class geoline_compare
#'
#' @param sfline_wgs84 a LINESTRING simple feature in WGS84
#'
#' @return object of class geolinecompare
#' @export
#'
#' @examples random_matrix <- matrix(c(runif(30, -90, 90), runif(30, -180, 180)), nrow=10, ncol=2)
#'random_line <- sf::st_linestring(random_matrix)
#'random_line <- sf::st_sfc(random_line, crs = 4326)
#'geolinecompare <- geolinecompare(random_line)
#'
geolinecompare <- function(sfline_wgs84) {
  if(class(sfline_wgs84)[1] != "sfc_LINESTRING") {
    stop("Please only insert a LINESTRING simple feature into the function")
  }
  if(length(sfline_wgs84)>1) {
    stop("Please insert only one LINESTRING")
  }
  if(sf::st_crs(sfline_wgs84)$input != "EPSG:4326") {
    stop("Please transform your simple feature into WGS84 using st_transform(...)")
  }
  sf_eqc <- sf::st_transform(sfline_wgs84, "+proj=eqc")
  # Segmentize both simple features
  sf_wgs84_seg <- sf::st_segmentize(sfline_wgs84, sf::st_length(sfline_wgs84)/100)
  sf_eqc_seg <- sf::st_segmentize(sf_eqc, sf::st_length(sf_eqc)/100)
  # Structure them together in a list and assign new s3 class "geoline compare"
  geoline_compare <- list(wgs84_seg = sf_wgs84_seg, eqc_seg = sf_eqc_seg)
  class(geoline_compare) <- "geolinecompare"
  return(geoline_compare)
}
