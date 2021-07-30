#' Create the globe
#'
#' Plots the basic globe in a rgl window device
#'
#' @param show_poles logical if the poles should be added for orientation
#' @param alpha set transparency for the globe
#' 0 = transparent
#' 1 = non-transparent
#' @param ... more parameters of rgl::material3d()
#'
#' @export
#'
#' @examples ps_globe(show_poles=TRUE)
ps_globe <- function(show_poles=TRUE, alpha=1, ...) {
  drawsphere <- function(xc=0, yc=0, zc=0, r=6330000, lats=18L, longs=36L) {
    #xc,yc,zc give centre of sphere, r is radius, lats/longs for resolution
    vertices = vector(mode = "numeric", length = 12L * lats * longs)
    vi = 1L
    for(i in 1:lats) {
      lat0 = pi * (-0.5 + (i - 1) / lats)
      z0   = sin(lat0)*r
      zr0  = cos(lat0)*r
      lat1 = pi * (-0.5 + i / lats)
      z1   = sin(lat1)*r
      zr1  = cos(lat1)*r
      for(j in 1:longs) {
        lng1 = 2 * pi *  (j - 1) / longs
        lng2 = 2 * pi *  (j) / longs
        x1 = cos(lng1)
        y1 = sin(lng1)
        x2 = cos(lng2)
        y2 = sin(lng2)
        vertices[vi] = x1 * zr0 + xc;    vi = vi + 1L
        vertices[vi] = y1 * zr0 + yc;    vi = vi + 1L
        vertices[vi] = z0 + zc;          vi = vi + 1L
        vertices[vi] = x1 * zr1 + xc;    vi = vi + 1L
        vertices[vi] = y1 * zr1 + yc;    vi = vi + 1L
        vertices[vi] = z1 + zc;          vi = vi + 1L
        vertices[vi] = x2 * zr1 + xc;    vi = vi + 1L
        vertices[vi] = y2 * zr1 + yc;    vi = vi + 1L
        vertices[vi] = z1 + zc;          vi = vi + 1L
        vertices[vi] = x2 * zr0 + xc;    vi = vi + 1L
        vertices[vi] = y2 * zr0 + yc;    vi = vi + 1L
        vertices[vi] = z0 + zc;          vi = vi + 1L
      }
    }
    indices = 1:(length(vertices)/3)
    rgl::shade3d(rgl::qmesh3d(vertices, indices, homogeneous=F), color = "#86a0dd", specular = "#1b618c", shininess = 75, alpha = alpha, ...)
  }
  if (show_poles==TRUE) {
    # create north and south pole points for orientation
    pole_n <- sf::st_point(c(0, 90))
    pole_n <- sf::st_sfc(pole_n, crs = 4326)
    pole_n <- sf::st_transform(pole_n, "+proj=geocent")
    pole_n <- sf::st_coordinates(pole_n)
    pole_s <- sf::st_point(c(0, -90))
    pole_s <- sf::st_sfc(pole_s, crs = 4326)
    pole_s <- sf::st_transform(pole_s, "+proj=geocent")
    pole_s <- sf::st_coordinates(pole_s)
  }
  # open rgl 3D plot
  drawsphere()
  if (show_poles==TRUE) {
  rgl::points3d(x=pole_n[1,'X'], y=pole_n[1,'Y'], z=pole_n[1,'Z'])
  rgl::text3d(c(pole_n[1,'X'], pole_n[1,'Y'], pole_n[1,'Z']+1000000),text = "North")
  rgl::points3d(x=pole_s[1,'X'], y=pole_s[1,'Y'], z=pole_s[1,'Z'])
  rgl::text3d(c(pole_s[1,'X'], pole_s[1,'Y'], pole_s[1,'Z']-1000000),text = "South")
  }
}
