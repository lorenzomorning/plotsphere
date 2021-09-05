#' geoairline_addcity
#'
#' @description This function adds another city to an object of class geoairline
#'
#' @param geoairline object of class geoairline where to add the new city to
#' @param city city name in english of the new city
#' @param country country name in english of the new city
#'
#' @return geoairline
#' @export
#'
#' @examples b2t <- geoairline("Berlin", "Germany", "Tokyo", "Japan")
#' b2t2r <- geoairline_addcity(b2t, "Rio de Janeiro", "Brazil")
geoairline_addcity <- function(geoairline, city, country) {
  if(class(geoairline)!="geoairline") {
    stop("Please use an object of class geoairline as input")
  }
  # Set global variable to NULL to avoid package notes
  world.cities <- name <- country.etc <- NULL
  # Load package data
  utils::data(world.cities, envir = environment())
  # Create dataframe from one subset
  station <- subset(world.cities, name == city & country.etc == country)
  # Check if this can be found or is doubled
  if(nrow(station) == 0) {
    stop(paste(city, "could not be found"))
  }
  else if(nrow(station) > 1) {
    cat("We have double cities found here
")
    print(station)
    {
      row = readline(prompt = "Enter the index number of the entry you want to keep: ")
    }
    station <- station[row,]
    rownames(station) <- NULL
  }
  # Convert station to a point and add it to point simple feature of geoairline class
  station_sf <- sf::st_as_sf(station, coords = c('long', 'lat'), crs=4326)
  cities_sf <- rbind(geoairline$cities, station_sf)
  rownames(cities_sf) <- NULL
  # Create new line between the last points and add it to the linestring
  new_airline_sf <- sf::st_sfc(sf::st_linestring(sf::st_coordinates(cities_sf)[c(nrow(cities_sf)-1, nrow(cities_sf)),]), crs = 4326)
  airline_sf <- rbind(sf::st_as_sf(geoairline$airline), sf::st_as_sf(new_airline_sf))
  airline_sf <- sf::st_geometry(airline_sf)
  # list the geoairline together again
  geoairline_new <- list("cities" = cities_sf, "airline" = airline_sf)
  class(geoairline_new) <- "geoairline"
  return(geoairline_new)
}
