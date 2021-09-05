#' @export
#'
summary.geolinecompare <- function(object,...) {
  lengths <- c(sf::st_length(object$wgs84_seg), sf::st_length(object$eqc_seg))
  df_print <- data.frame("Linestring" = names(object),
             "Geo lengths" = lengths)
  print(df_print)
}

#' @export
#'
summary.geoairline <- function(object,...) {
  geo_length <- sf::st_length(object$airline)
  connections <- vector()
  for (i in 1:length(geo_length)) {
    connections <- append(connections, paste(as.data.frame(object$cities)[i,1], as.data.frame(object$cities)[i+1,1]))
  }
print(as.data.frame(object$cities))
  cat(paste("
  Length of connection line", connections, "on a sphere in meters:", round(geo_length,0)))
}
