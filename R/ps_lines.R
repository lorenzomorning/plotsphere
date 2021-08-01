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
  if(all(sf::st_geometry_type(sf_lines)=='LINESTRING' | all(sf::st_geometry_type(sf_lines)=='MULTILINESTRING')) == FALSE) {
    stop("Input sf geometries are not all LINESTRINGs or MULTILINESTRINGs")
  }
  if (all(sf::st_is_valid(sf_lines)) == FALSE) {
    sf_lines <- sf::st_make_valid(sf_lines)
  }
  sf_lines <- sf::st_geometry(sf_lines)
  sf_lines <- sf::st_transform(sf_lines, "+proj=geocent")
  # if line geometries are multilinestrings cast them to single linestrings
  if (all(sf::st_geometry_type(sf_lines)=='MULTILINESTRING')) {
    sf_lines <- sf::st_cast(sf_lines, to='LINESTRING')
  }
  # if geometry is a single linestring no for loop is needed
  if (length(sf_lines)==1) {
    coord <- sf::st_coordinates(sf_lines[[1]])
    from <- coord[1:nrow(coord)-1,1:3]
    to <- coord[2:nrow(coord),1:3]
  }
  # iterate through the single linestring geometry and create a big from and to table
  else if (length(sf_lines)>1) {
    for (i in 1:length(sf_lines)) {
      coord <- sf::st_coordinates(sf_lines[[i]])
      # create starting from and to matrix from the first geometry
      if(i == 1) {
        from <- coord[1:nrow(coord)-1,1:3]
        to <- coord[2:nrow(coord),1:3]
      }
      if (i > 1) {
        # add coordinates to from (except the last one)
        from <- rbind(from, coord[1:nrow(coord)-1,1:3])
        to <- rbind(to, coord[2:nrow(coord),1:3])
      }
    }
  }
  rgl::arc3d(from = from, to = to, center = c(0,0,0), color=color, alpha=alpha, ...)
}
