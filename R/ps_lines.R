#' Plot simple feature lines on a sphere
#'
#' Plot simple feature lines on a sphere. Use ps_globe() first to plot the lines on the globe
#'
#' @param sf_lines simple feature linestrings or mutli-linestrings
#' @param color set color of the point(s)
#' @param alpha set transparency of the point(s)
#' 0 = transparent
#' 1 = non-transparent
#' @param ... more parameters of rgl::material3d()
#'
#' @export
#'
#' @examples random_matrix <- matrix(c(runif(30, -90, 90), runif(30, -180, 180)), nrow=10, ncol=2)
#'random_line <- sf::st_linestring(random_matrix)
#'random_line <- sf::st_sfc(random_line, crs = 4326)
#'ps_globe()
#'ps_lines(random_line, color = 'green')
ps_lines <- function(sf_lines, color="black", alpha = 1, ...) {
  if (all(sf::st_is_valid(sf_lines)) == FALSE) {
    sf_lines <- sf::st_make_valid(sf_lines)
  }
  sf_lines <- sf::st_geometry(sf_lines)
  sf_lines <- sf::st_make_valid(sf_lines)
  sf_lines <- sf::st_transform(sf_lines, "+proj=geocent")
  for (single_line in sf_lines) {
    single_line <- sf::st_coordinates(single_line)
    rgl::arc3d(from = single_line[1:nrow(single_line)-1,1:3], to = single_line[2:nrow(single_line),1:3], center = c(0,0,0), color=color, alpha=alpha, ...)
  }
}
