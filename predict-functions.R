library(owmr)
library(rjson)

source("read-csv.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33")
API_key = "ac66c8209bdf887068a2a79e4fdbca33"

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
    
    print(historical_weather)
    
    if(historical_weather$list[[4]]$wind$speed > 1.0 || historical_weather$list[[4]]$wind$speed < 3.0) { 
      fireThreshold <- fireThreshold+0.1 
      return(fireThreshold)
    }
    else if(historical_weather$list[[4]]$wind$speed > 3.1 || historical_weather$list[[4]]$wind$speed < 5.0) { 
      fireThreshold <- fireThreshold+0.2
      return(fireThreshold)
    }
    else if(historical_weather$list[[4]]$wind$speed > 5.1 || historical_weather$list[[4]]$wind$speed < 7.0) { 
      fireThreshold <- fireThreshold+0.25 
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$wind$speed > 7.1 || historical_weather$list[[4]]$wind$speed < 9.0) { 
      fireThreshold <- fireThreshold+0.3 
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$wind$speed > 5.1 || historical_weather$list[[4]]$wind$speed < 11.0) { 
      fireThreshold <- fireThreshold+.35 
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$wind$speed > 11.5 || historical_weather$list[[4]]$wind$speed < 13.0) { 
      fireThreshold <- fireThreshold+.37 
      return(fireThreshold)
    } 
    #HUMIDITY 
    else if(historical_weather$list[[4]]$main$humidity > 30 || historical_weather$list[[4]]$wind$speed < 40) { 
      fireThreshold <- fireThreshold+.25
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$main$humidity > 50 || historical_weather$list[[4]]$wind$speed < 70) { 
      fireThreshold <- fireThreshold+.15
      return(fireThreshold)
    }
    else if(historical_weather$list[[4]]$main$humidity > 71 || historical_weather$list[[4]]$wind$speed < 100) { 
      fireThreshold <- fireThreshold+.1
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$main$humidity > 71 || historical_weather$list[[4]]$wind$speed < 100) { 
      fireThreshold <- fireThreshold+.1
      return(fireThreshold)
    } 
    #TEMP
    else if(historical_weather$list[[1]]$main$temp > 90 || historical_weather$list[[1]]$main$temp < 100) { 
      fireThreshold <- fireThreshold+.45
      return(fireThreshold)
    } #VERY HIGH - RED FLAG IN JUST TEMP ALONE
    else if(historical_weather$list[[4]]$main$temp > 80 || historical_weather$list[[4]]$main$temp < 89) { 
      fireThreshold <- fireThreshold+.4
      return(fireThreshold)
    }  
    else if(historical_weather$list[[4]]$main$temp > 70 || historical_weather$list[[4]]$main$temp < 79) { 
      fireThreshold <- fireThreshold+.3
      return(fireThreshold)
    } 
    else if(historical_weather$list[[4]]$main$temp > 60 || historical_weather$list[[4]]$main$temp < 69) { 
      fireThreshold <- fireThreshold+.2
      return(fireThreshold)
    }
    else if(historical_weather$list[[4]]$main$temp > 40 || historical_weather$list[[4]]$main$temp < 59) { 
      fireThreshold <- fireThreshold+.11
      return(fireThreshold) } 
    
    else { (historical_weather$list[[4]]$main$temp < 39) 
      fireThreshold <- fireThreshold+.05}
    
    if(fireThreshold >= 1) {
      return(TRUE)
    }
    else { 
      return(FALSE)
    }
    
    # if (is.null(vegetation) || vegetation <- .25) {
    #   return(FALSE)
    # }

    print("Wind Speed:")
    print(historical_weather$list[[4]]$wind$speed)
    print("Wind Direction:")
    print(historical_weather$list[[4]]$wind$deg)
    print("Predicted Fire Threshold: ")
    print(FireThreshold)
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
  # print("Vegetation:")
  # print(getVegetation(long, lat))
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
