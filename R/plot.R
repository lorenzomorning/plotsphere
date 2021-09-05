#' @export
#'
plot.geolinecompare <- function(x,...) {
  ps_globe(alpha=0.8)
  ps_lines(x$wgs84_seg, color='darkblue')
  ps_lines(x$eqc_seg, color='darkred')
  length_wgs <- as.numeric(sf::st_length(x$wgs84_seg))
  wgs84_coord <- sf::st_transform(sf::st_centroid(x$wgs84_seg), "+proj=geocent")
  wgs84_coord <- sf::st_coordinates(wgs84_coord)
  rgl::text3d(x=wgs84_coord[1,1]+length_wgs/10, y=wgs84_coord[1,2]+length_wgs/10, z=wgs84_coord[1,3]+length_wgs/10, 'WGS84_seg', color='darkblue')
  length_eqc <- as.numeric(sf::st_length(x$eqc_seg))
  eqc_coord <- sf::st_transform(sf::st_centroid(x$eqc_seg), "+proj=geocent")
  eqc_coord <- sf::st_coordinates(eqc_coord)
  rgl::text3d(x=eqc_coord[1,1]+length_eqc/10, y=eqc_coord[1,2]+length_eqc/10, z=eqc_coord[1,3]+length_eqc/10, 'eqc_seg', color='darkred')
}

#' @export
#'
plot.geoairline <- function(x,...) {
  ps_globe(alpha=0.8)
  ps_points(x$cities, color = 'darkblue')
  ps_lines(x$airline, color='darkblue')
  cities <- as.data.frame(x$cities)
  cities_geoc <- sf::st_transform(x$cities, "+proj=geocent")
  cities_geoc_coord <- sf::st_coordinates(cities_geoc)
  geo_length <- min(as.numeric(sf::st_length(x$airline)))
  rgl::text3d(x=cities_geoc_coord[,1]+geo_length/10, y=cities_geoc_coord[,2]+geo_length/10, z=cities_geoc_coord[,3]+geo_length/10, cities[,1])
}
