x <- team_matches_10[[3]]
glimpse(x)

z <- nrow(x)
index <- 1
distances <- 0
lat_1 <- 0
lat_2 <- 0
lon_1 <- 0
lon_2 <- 0

x$road_trip_distance <- 0
glimpse(x)

for (i in 2:z){
  
  
  if (x[i, "target_homeAway"] == "Away") {
  
    lat_1 <- x[i, "opponentLat"] 
    lon_1 <- x[i, "opponentLon"]
    lat_2 <- x[index, "opponentLat"] 
    lon_2 <- x[index, "opponentLon"]
   
    disatances <- distHaversine(c(lon_1, lat_1), c(lon_2, lat_2))
    x[i, "road_trip_distance"] <- distances
    index <- i
  
  } else if (x[i, "target_homeAway"] == "Home" & x[i - 1, "target_homeAway"] == "Away") {
  
    lat_1 <- x[i, "opponentLat"] 
    lon_1 <- x[i, "opponentLon"]
    lat_2 <- x[index, "opponentLat"] 
    lon_2 <- x[index, "opponentLon"]
  
    disatances <- round(distHaversine(c(lon_1, lat_1), c(lon_2, lat_2))/1000,3)
    x[i, "road_trip_distance"] <- distances
    index <- i
    
  }
  
}


glimpse(x) 
