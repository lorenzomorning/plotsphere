#' geolinecompare_diff
#'
#' @description This function calculates the length difference between a sf LINESTRING on WGS84 and the equirectangular projection
#' @param x object of class geolinecompare
#'
#' @return length in meter
#' @export
#'
#' @examples random_matrix <- matrix(c(runif(30, -90, 90), runif(30, -180, 180)), nrow=10, ncol=2)
#'random_line <- sf::st_linestring(random_matrix)
#'random_line <- sf::st_sfc(random_line, crs = 4326)
#'random_line_comp <- geolinecompare(random_line)
#'geolinecompare_diff(random_line_comp)
#'
geolinecompare_diff <- function(x) {
  if(class(x)!="geolinecompare") {
    stop("X input must be object of s3 class 'geolinecompare'")
  }
  length_diff <- sf::st_length(x$eqc_seg) - sf::st_length(x$wgs84_seg)
  return(length_diff)
}
