library(owmr)
library(rjson)

source("read-csv.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 
API_key = "ac66c8209bdf887068a2a79e4fdbca33"

willFireStart <- function(long, lat) {
  start <- TRUE
  tryCatch({
    url <- paste0("http://history.openweathermap.org/data/2.5/history/city?lat=",lat
                  , "&lon=", long
                  , "&type=hour"
                  , "&cnt=4"
                  , "&appid=", API_key)
    historical_weather <- fromJSON(file = url)
    print(historical_weather)
    print("Wind Speed:")
    print(historical_weather$list[[4]]$wind$speed)
  }, error = function(e) {
    start <- FALSE
  }, warning = function(w) {
    start <- FALSE
  })
  
  
  
  print(" ")
  print("Long: ")
  print(long)
  print("Lat: ")
  print(lat)
  print("Vegetation:")
  print(getVegetation(long, lat))
  return(start)
}

willFireSpread <- function(long, lat) {
  url <- paste0("http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
                ,"&lon=", long
                ,"&appid=", API_key
                ,"&cnt=", 4)
  weather <- fromJSON(file = url)
  vegetation <- getVegetation(long, lat)
  
  if (vegetation.isnull || vegetation < .2) {
    return(FALSE)
  }
}

fireGrow <- function(long, lat) {

}