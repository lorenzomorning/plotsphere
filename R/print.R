#' @export
#'
print.geolinecompare <- function(x,...) {
  cat("GEOLINECOMPARE object (list of two sf linestrings)")
cat("
Segmentized line in WGS84 ($wgs84_seg):
")
print(x$wgs84_seg)
cat("
Segmentized line in equirectangular projection ($eqc_seg):
")
print(x$eqc_seg)
}

#' @export
#'
print.geoairline <- function(x,...) {
  cat("GEOAIRLINE object (list of two elements)

Cities as POINT simple feature collection ($cities):")
print(x$cities)
cat("
Airline as LINESTRING geometry set ($airline):
")
print(x$airline)
}
