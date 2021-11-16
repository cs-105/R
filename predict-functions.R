library(owmr)
library(rjson)

source("read-csv.R")

<<<<<<< HEAD
Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33")
API_key = "ac66c8209bdf887068a2a79e4fdbca33"
#PREDICT IF FIRE WILL START
willFireStart <- function(long, lat) {
  start <- TRUE
  fireThreshold <- 0
  
  tryCatch({
    url <-
      paste0(
        "http://history.openweathermap.org/data/2.5/history/city?lat=", lat
        , "&lon=", long
        , "&type=hour"
        , "&cnt=4"
        , "&appid=", API_key
        , "&units=imperial"
      )
    historical_weather <- fromJSON(file = url)
    vegitation <- (getVegetation(long, lat))
    
    print(historical_weather)
    ##FIRE THRESHOLD STATEMENTS
    #WIND
    
    if(historical_weather$list[[4]]$wind$speed > 1.0 || historical_weather$list[[1]]$wind$speed < 3.0) { 
      fireThreshold <- fireThreshold+0.05 }
    else if(historical_weather$list[[4]]$wind$speed > 3.1 || historical_weather$list[[1]]$wind$speed < 5.0) { 
      fireThreshold <- fireThreshold+0.1 }
    else if(historical_weather$list[[4]]$wind$speed > 5.1 || historical_weather$list[[1]]$wind$speed < 7.0) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(historical_weather$list[[4]]$wind$speed > 7.1 || historical_weather$list[[1]]$wind$speed < 9.0) { 
      fireThreshold <- fireThreshold+0.18 } 
    else if(historical_weather$list[[4]]$wind$speed > 9.1 || historical_weather$list[[1]]$wind$speed < 11.0) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(historical_weather$list[[4]]$wind$speed > 11.1 || historical_weather$list[[1]]$wind$speed < 13.0) { 
      fireThreshold <- fireThreshold+0.25 } 
    #HUMIDITY 
    if(historical_weather$list[[4]]$main$humidity > 30 || historical_weather$list[[1]]$main$humidity < 40) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(historical_weather$list[[4]]$main$humidity > 41 || historical_weather$list[[1]]$main$humidity < 70) { 
      fireThreshold <- fireThreshold+0.10 }
    else if(historical_weather$list[[4]]$main$humidity > 71 || historical_weather$list[[1]]$main$humidity < 100) { 
      fireThreshold <- fireThreshold+0.05 } 
    else if(historical_weather$list[[4]]$main$humidity > 101 || historical_weather$list[[1]]$main$humidity < 200) { 
      fireThreshold <- fireThreshold+0.03 } 
    
    #TEMP
    if(historical_weather$list[[1]]$main$temp > 200 || historical_weather$list[[1]]$main$temp < 300) { 
      fireThreshold <- fireThreshold+.38 }
    else if(historical_weather$list[[1]]$main$temp > 100 || historical_weather$list[[1]]$main$temp < 200) { 
      fireThreshold <- fireThreshold+0.35 }      
    else if(historical_weather$list[[1]]$main$temp > 90 || historical_weather$list[[1]]$main$temp < 99) { 
      fireThreshold <- fireThreshold+0.3 }
    else if(historical_weather$list[[4]]$main$temp > 80 || historical_weather$list[[1]]$main$temp < 89) { 
      fireThreshold <- fireThreshold+0.22 }
    else if(historical_weather$list[[4]]$main$temp > 70 || historical_weather$list[[1]]$main$temp < 79) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(historical_weather$list[[4]]$main$temp > 60 || historical_weather$list[[1]]$main$temp < 69) { 
      fireThreshold <- fireThreshold+.15 }
    else if(historical_weather$list[[4]]$main$temp > 40 || historical_weather$list[[1]]$main$temp < 59) { 
      fireThreshold <- fireThreshold+.1  }
    else if(historical_weather$list[[4]]$main$temp > 20 || historical_weather$list[[1]]$main$temp < 39) { 
      fireThreshold <- fireThreshold+.05  }
    else if(historical_weather$list[[4]]$main$temp > 10 || historical_weather$list[[1]]$main$temp < 19) { 
      fireThreshold <- fireThreshold+.02 }
    else if(historical_weather$list[[4]]$main$temp > 0 || historical_weather$list[[1]]$main$temp < 9) { 
      fireThreshold <- fireThreshold+.0 }
    
    #VEGITATION
    if(vegitation < 0.9999 || vegitation > 0.70) { 
      fireThreshold <- fireThreshold+0.4 } 
    else if(vegitation < 0.69 || vegitation > 0.50) {
      fireThreshold <- fireThreshold+0.35 } 
    else if(vegitation < 0.49 || vegitation > 0.25) {
      fireThreshold <- fireThreshold+0.3 } 
    else if(vegitation < 0.25 || vegitation > 0.10) {
      fireThreshold <- fireThreshold+0.2 } 
    else if(vegitation < 0.25 || vegitation > 0.10) {
      fireThreshold <- fireThreshold+0.12 } 
    else if(vegitation < 0.9 || vegitation > 0.1) {
      fireThreshold <- fireThreshold+0.1 } 
    else if(vegitation < 0.09 || vegitation > 0.00001) {
      fireThreshold <- fireThreshold+0.0001 } 
    else { 
      fireThreshold <- fireThreshold+0.0000 } 
    
    print("Wind Speed:")
    print(historical_weather$list[[4]]$wind$speed)
    print("Wind Direction:")
    print(historical_weather$list[[4]]$wind$deg)
    print("Predicted Fire Threshold: ")
    print(fireThreshold)
    
    
    ##PRINT STATEMENTS TO CONSOLE
    print("Wind Speed:")
    print(historical_weather$list[[1]]$wind$speed)
    print("Humidity:")
    print(historical_weather$list[[1]]$main$humidity)
    print("Temp:")
    print(historical_weather$list[[1]]$main$temp)
    print("Vegetation at coordinates: ")
    print("Long, ")
    print("Lat: ")
    print(long)
    print(lat)
    #SEE IF BASED ON WEATHER ITLL START
    
    print(vegitation)
    print("Wind Direction:")
    print(historical_weather$list[[1]]$wind$deg)
    
    if(fireThreshold >= 1) { 
      return(TRUE)
    }
    else { 
      return(FALSE)
    }
    
    
=======
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
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
  }, error = function(e) {
    start <- FALSE
  }, warning = function(w) {
    start <- FALSE
  })
<<<<<<< HEAD
  return(start)
}


willFireSpread <- function(long, lat) {
  url <-
    paste0(
      "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
      , "&lon=", long
      , "&appid=", API_key
      , "&cnt=", 4
    )
  weather <- fromJSON(file = url)
  vegetation <- getVegetation(long, lat)
}

fireGrow <- function(longLatList, count) {
  if (length(longLatList) == -1 || count == 0) {
    return(list())
  }
  
  local_long_lat <- list()
  
  for (i in seq(1, length(longLatList))) {
    if (i %% 2 == 1) {
      long <- longLatList[[i]]
      lat <- longLatList[[(i + 1)]]
      
      url <-
        paste0(
          "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
          , "&lon=", long
          , "&appid=", API_key
          , "&cnt=", 4
          , "&units=imperial"
        )
      weather <- fromJSON(file = url)
      # print(weather)
      wind <- weather$list[[count]]$wind$deg
      
      localList <- list()
      
      # If the wind is blowing North
      if (wind >= 135 && wind < 225) {
        if (willFireSpread((long - 0.00013), (lat + 0.00013))) {
          local_long_lat <- append(localList, list((long - 0.00013), (lat + 0.00013)))
        }
        if (willFireSpread(long, (lat + 0.00017))) {
          local_long_lat <- append(localList, list(long, (lat + 0.00017)))
        }
        if (willFireSpread((long + 0.00013), (lat + 0.00013))) {
          local_long_lat <- append(localList, list((long + 0.00013), (lat + 0.00013)))
        }
      }
      
      # If the wind is blowing East
      if (wind >= 225 && wind < 315) {
        if (willFireSpread((long + 0.00013), (lat + 0.00013))) {
          local_long_lat <- append(localList, list((long + 0.00013), (lat + 0.00013)))
        }
        if (willFireSpread((long + 0.000215), lat)) {
          local_long_lat <- append(localList, list((long + 0.000215), lat))
        }
        if (willFireSpread((long + 0.00013), (lat - 0.00013))) {
          local_long_lat <- append(localList, list((long + 0.00013), (lat - 0.00013)))
        }
      }
      
      # If the wind is blowing South
      if (wind >= 315 || wind < 45) {
        if (willFireSpread((long + 0.00013), (lat - 0.00013))) {
          local_long_lat <- append(localList, list((long + 0.00013), (lat - 0.00013)))
        }
        if (willFireSpread(long, (lat - 0.00017))) {
          local_long_lat <- append(localList, list(long, (lat - 0.00017)))
        }
        if (willFireSpread((long - 0.00013), (lat - 0.00013))) {
          local_long_lat <- append(localList, list((long - 0.00013), (lat - 0.00013)))
        }
      }
      
      # If the wind is blowing West
      if (wind >= 45 && wind < 135) {
        if (willFireSpread((long - 0.00013), (lat - 0.00013))) {
          local_long_lat <- append(localList, list((long - 0.00013), (lat - 0.00013)))
        }
        if (willFireSpread((long - 0.000215), lat)) {
          local_long_lat <- append(localList, list((long - 0.000215), lat))
        }
        if (willFireSpread((long - 0.00013), (lat + 0.00013))) {
          local_long_lat <- append(localList, list((long - 0.00013), (lat + 0.00013)))
        }
      }
    }
  }
  
  return(local_long_lat <- append(local_long_lat, fireGrow(local_long_lat, (count - 1))))
}
=======
  
  
  
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
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
