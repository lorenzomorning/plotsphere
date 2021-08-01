#' Plot simple feature polygons on a sphere
#'
#' Plot simple feature polygons on a sphere. Use ps_globe() first to plot the polygons on the globe
#'
#' @param sf_polygons simple feature polygons or mutli-polygons
#' @param color set color of the point(s)
#' @param alpha set transparency of the point(s)
#' 0 = transparent
#' 1 = non-transparent
#' @param ... more parameters of rgl::material3d()
#'
#' @export
#'
#' @examples poly <- sf::st_polygon(list(rbind(c(0,23), c(3,21), c(15,32), c(26,12),
#' c(25,-33), c(42,-40), c(32,-54), c(10,-23), c(0,-23), c(0,23))))
#'poly <- sf::st_sfc(poly, crs = 4326)
#'ps_globe(alpha=0.4)
#'ps_polygons(poly, color = 'darkgreen')
ps_polygons <- function(sf_polygons, color="black", alpha = 1, ...) {
  if(all(sf::st_geometry_type(sf_polygons)=='POLYGON' | all(sf::st_geometry_type(sf_polygons)=='MULTIPOLYGON')) == FALSE) {
    stop("Input sf geometries are not all POLYGONs or MULTIPOLYGONs")
  }
  # if polygon geometries are multipolygons cast them to single polygons
  if (all(sf::st_geometry_type(sf_polygons)=='MULTIPOLYGON')) {
    sf_polygons <- sf::st_cast(sf_polygons, to='POLYGON')
  }
  # triangulation
  triangulated <- sfdct::ct_triangulate(sf_polygons, a = 0.3)
  # split back geometry collection to polygons
  triangulated <- sf::st_collection_extract(triangulated, "POLYGON")
  # transform to geocentric
  triangulated <- sf::st_transform(triangulated, "+proj=geocent")
  triangulated_coord <- sf::st_coordinates(triangulated)
  # get rid of every fourth row as this is the point to close the polygon in sf (but not needed to plot a triangle in rgl)
  triangulated_coord <- triangulated_coord[(1:nrow(triangulated_coord) %% 4) != 0, ]
  rgl::triangles3d(x=triangulated_coord[,'X'], y=triangulated_coord[,'Y'], z=triangulated_coord[,'Z'], color=color, alpha=alpha, diffuse = "black", specular = "black", shininess = 75, ...)
}
