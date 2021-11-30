readVeg <- function() {
  return(read.csv(file = "vegetation.csv", row.names = 1, header = TRUE, check.names = FALSE))
}

getVegetation <- function(long, lat) {
  vegFile <- readVeg()
  long <- (round_any(long, .1, f = floor) + .05)
  lat <- round_any(lat, .1, f = floor) + .05
  if (long > -64.95 || long < -127 || lat > 50 || lat < 23.4) {
    return(NULL)
  }
  vegetation <- vegFile[as.character(lat), as.character(long)]
  if (vegetation < 1) {
    return(vegetation)
  } else {
    return(NULL)
  }
}