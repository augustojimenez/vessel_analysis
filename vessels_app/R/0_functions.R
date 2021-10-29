to_radians <- function(degrees){
# Converts degrees to radians
      radians <- degrees * pi / 180
      
      return(radians)
}

haversine <- function(lat_1, lon_1, lat_2, lon_2, degrees = TRUE){
# Calculates distance using the 'haversine' formula
# References:
### https://stackoverflow.com/questions/365826/calculate-distance-between-2-gps-coordinates
### http://www.movable-type.co.uk/scripts/latlong.html
      R <- 6371 # Earth radius in Km
      
      if (degrees) {
            lat_1 <- to_radians(lat_1)
            lon_1 <- to_radians(lon_1)
            
            lat_2 <- to_radians(lat_2)
            lon_2 <- to_radians(lon_2)
      }
      
      d_lat <- lat_1 - lat_2
      d_lon <- lon_1 - lon_2
      
      a <- sin(d_lat / 2)^2 + cos(lat_1) * cos(lat_2) * sin(d_lon / 2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a))
      d <- R * c * 1000
      
      return(d)
}

to_km <- function(nm){
# Converts nautical miles to Km
      km <- nm * 1.852
      
      return(km)
}

calculate_distance <- function(speed, time){
# Calculates distance in meters, from speed and time (in hours)
      distance <- speed * as.numeric(time)
      distance <- to_km(distance) * 1000
      
      return(distance)
}

