#' geoairline
#'
#' @description This function creates the shortest great circle line connection over a sphere between two cities
#'
#' @param city1 english name of the first city
#' @param country1 english name of the country of the first city
#' @param city2 english name of the second city
#' @param country2 english name of the country of the second city
#'
#' @return object of class geoairline - list of two elements: cities_sf and LINESTRING geometry between the cities
#' @export
#'
#' @examples berlin2tokyo <- geoairline("Berlin", "Germany", "Tokyo", "Japan")
geoairline <- function(city1, country1, city2, country2) {
  # Set global variable to NULL to avoid package notes
  world.cities <- name <- country.etc <- NULL
  # Load package data
  utils::data(world.cities, envir = environment())
  # Create two sf POINTS from the world city data set
  cities <- subset(world.cities, name == city1 & country.etc == country1)
  cities <- rbind(cities, subset(world.cities, name == city2 & country.etc == country2))
  rownames(cities) <- NULL
  # Check if there no cities found or double names
  if(nrow(cities) == 0 & !identical(c(city1, city2),cities$name)) {
    stop(paste(city1, "and", city2, "could not be found"))
  }
  else if(nrow(cities) == 1 & !identical(c(city1, city2),cities$name)) {
    if(cities$name == city1) {
      stop(paste(city2, "could not be found"))
    }
    else {
      stop(paste(city1, "could not be found"))
    }
  }
  else if(nrow(cities) == 2 & !identical(c(city1, city2),cities$name)) {
    if(all(city1==cities$name)) {
      stop(paste(city2, "could not be found"))
    } else {
      stop(paste(city1, "could not be found"))
    }
  }
  else if(nrow(cities) > 2) {
    cat("We have double cities found here
")
print(cities)
    {
      row1 = readline(prompt = "Enter the index number of the first entry you want to keep: ")
      row2 = readline(prompt = "Enter the index number of the second entry you want to keep: ")
    }
    cities <- cities[c(row1, row2),]
    rownames(cities) <- NULL
  }

  cities_sf <- sf::st_as_sf(cities, coords = c('long', 'lat'), crs=4326)
  # create a linestring between the cities
  airline_sf <- sf::st_sfc(sf::st_linestring(sf::st_coordinates(cities_sf)), crs = 4326)
  # list the geoairline together
  geoairline <- list("cities" = cities_sf, "airline" = airline_sf)
  class(geoairline) <- "geoairline"
  return(geoairline)
}
