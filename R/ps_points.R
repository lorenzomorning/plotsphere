#' Plot simple feature points on a sphere
#'
#' Plot simple feature points on a sphere. Use ps_globe() first to plot the points on the globe
#'
#' @param sf_points simple feature point or multi-points
#' @param color set color of the point(s)
#' @param alpha set transparency of the point(s)
#' 0 = transparent
#' 1 = non-transparent
#' @param ... more parameters of rgl::material3d()
#'
#' @export
#'
#' @examples random_matrix <- matrix(c(runif(30, -90, 90), runif(30, -180, 180)), nrow=10, ncol=2)
#'random_points <- sf::st_multipoint(random_matrix)
#'random_points <- sf::st_sfc(random_points, crs = 4326)
#'ps_globe()
#'ps_points(random_points, color='red', alpha=0.8)
ps_points <- function(sf_points, color="black", alpha=1, ...) {
  if(all(sf::st_geometry_type(sf_points)=='POINT' | sf::st_geometry_type(sf_points)=='MULTIPOINT') == FALSE) {
    stop("Input sf geometries are not all POINTs or MULTIPOINTs")
  }
  sf_points <- sf::st_geometry(sf_points)
  sf_points <- sf::st_transform(sf_points, "+proj=geocent")
  sf_points <- sf::st_coordinates(sf_points)
  rgl::points3d(x=sf_points[,'X'], y=sf_points[,'Y'], z=sf_points[,'Z'], color=color, alpha=alpha, ...)
}
