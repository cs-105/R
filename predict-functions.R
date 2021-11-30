library(owmr)
library(rjson)

source("read-csv.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 
API_key = "ac66c8209bdf887068a2a79e4fdbca33"

willFireStart <- function(long, lat, localVegetation) {
  start <- TRUE
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
    print("🔵🔵🔵Begin historical Weather: 🔵🔵🔵")
    print(historical_weather)
    print("🔵🔵🔵End historical Weather: 🔵🔵🔵")
    vegFile <- localVegetation
    
    
    
    ##RAIN - IF ITS RAINING, RETURN FALSE
    ##RAIN all historical counts 
    rain1 <- historical_weather$list[[1]]$weather[[1]]$description 
    rain2 <- historical_weather$list[[2]]$weather[[1]]$description 
    rain3 <- historical_weather$list[[3]]$weather[[1]]$description 
    rain3 <- historical_weather$list[[4]]$weather[[1]]$description 
    rainVar <- "rain"
    if(rain1 || rain2 || rain3 || rain4 == rainVar) { 
      return(FALSE)
    }
    else{ 
      return(TRUE)
    }
    ##WIND
    if(historical_weather$list[[1]]$wind$speed > 1.0 || historical_weather$list[[1]]$wind$speed < 3.0) { 
      fireThreshold <- fireThreshold+0.05 }
    else if(historical_weather$list[[1]]$wind$speed > 3.1 || historical_weather$list[[1]]$wind$speed < 5.0) { 
      fireThreshold <- fireThreshold+0.1 }
    else if(historical_weather$list[[1]]$wind$speed > 5.1 || historical_weather$list[[1]]$wind$speed < 7.0) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(historical_weather$list[[1]]$wind$speed > 7.1 || historical_weather$list[[1]]$wind$speed < 9.0) { 
      fireThreshold <- fireThreshold+0.18 } 
    else if(historical_weather$list[[1]]$wind$speed > 9.1 || historical_weather$list[[1]]$wind$speed < 11.0) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(historical_weather$list[[1]]$wind$speed > 11.1 || historical_weather$list[[1]]$wind$speed < 13.0) { 
      fireThreshold <- fireThreshold+0.25 } 
    #HUMIDITY 
    if(historical_weather$list[[1]]$main$humidity > 30 || historical_weather$list[[1]]$main$humidity < 40) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(historical_weather$list[[1]]$main$humidity > 41 || historical_weather$list[[1]]$main$humidity < 70) { 
      fireThreshold <- fireThreshold+0.10 }
    else if(historical_weather$list[[1]]$main$humidity > 71 || historical_weather$list[[1]]$main$humidity < 100) { 
      fireThreshold <- fireThreshold+0.05 } 
    else if(historical_weather$list[[1]]$main$humidity > 101 || historical_weather$list[[1]]$main$humidity < 200) { 
      fireThreshold <- fireThreshold+0.03 } 
    #TEMP
    if(historical_weather$list[[1]]$main$temp > 200 || historical_weather$list[[1]]$main$temp < 300) { 
      fireThreshold <- fireThreshold+.38 }
    else if(historical_weather$list[[1]]$main$temp > 100 || historical_weather$list[[1]]$main$temp < 200) { 
      fireThreshold <- fireThreshold+0.35 }      
    else if(historical_weather$list[[1]]$main$temp > 90 || historical_weather$list[[1]]$main$temp < 99) { 
      fireThreshold <- fireThreshold+0.3 }
    else if(historical_weather$list[[1]]$main$temp > 80 || historical_weather$list[[1]]$main$temp < 89) { 
      fireThreshold <- fireThreshold+0.22 }
    else if(historical_weather$list[[1]]$main$temp > 70 || historical_weather$list[[1]]$main$temp < 79) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(historical_weather$list[[1]]$main$temp > 60 || historical_weather$list[[1]]$main$temp < 69) { 
      fireThreshold <- fireThreshold+.15 }
    else if(historical_weather$list[[1]]$main$temp > 40 || historical_weather$list[[1]]$main$temp < 59) { 
      fireThreshold <- fireThreshold+.1  }
    else if(historical_weather$list[[1]]$main$temp > 20 || historical_weather$list[[1]]$main$temp < 39) { 
      fireThreshold <- fireThreshold+.05  }
    else if(historical_weather$list[[1]]$main$temp > 10 || historical_weather$list[[1]]$main$temp < 19) { 
      fireThreshold <- fireThreshold+.02 }
    else if(historical_weather$list[[1]]$main$temp > 0 || historical_weather$list[[1]]$main$temp < 9) { 
      fireThreshold <- fireThreshold+.0 }
    #ATMOSPHERIC PRESSURE-10.2
    #Conversion: 10.2PSI(10'000 feet fire will not burn) - 689.4757293178 hPa
    if(historical_weather$list[[1]]$main$pressure < 689.4757293178) { 
      return(FALSE)
    }
    
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
    # print("Wind Speed:")
    # print(historical_weather$list[[4]]$wind$speed)
    # print("Wind Direction:")
    # print(historical_weather$list[[4]]$wind$deg)
  }, error = function(e) {
    start <- FALSE
  }, warning = function(w) {
    start <- FALSE
  })
  
  
  vegetation <- getVegetation(long, lat, localVegetation)
  if(is.null(vegetation) || vegetation < .25) {
    print("Vegetation too low:")
    print(vegetation)
    return(FALSE)
  }
  
  # print(rainVar)
  # print(" ")
  # print("Long: ")
  # print(long)
  # print("Lat: ")
  # print(lat)
  return(start)
}


willFireSpread <- function(long, lat, localVegitation) {
  url <- paste0("http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
                ,"&lon=", long
                ,"&appid=", API_key
                ,"&cnt=", 4)
  weather <- fromJSON(file = url)
  vegetation <- getVegetation(long, lat)
  vegFile <- localVegetation
  start <- TRUE
  
  tryCatch({
    url <-
      current_weather <- fromJSON(file = url)
    print("🔴🔴🔴Begin current weather: 🔴🔴🔴")
    print(current_weather)
    print("🔴🔴🔴End current weather: 🔴🔴🔴")
    if(current_weather$list[[1]]$wind$speed > 1.0 || current_weather$list[[1]]$wind$speed < 3.0) { 
      fireThreshold <- fireThreshold+0.05 }
    else if(current_weather$list[[1]]$wind$speed > 3.1 || current_weather$list[[1]]$wind$speed < 5.0) { 
      fireThreshold <- fireThreshold+0.1 }
    else if(current_weather$list[[1]]$wind$speed > 5.1 || current_weather$list[[1]]$wind$speed < 7.0) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(current_weather$list[[1]]$wind$speed > 7.1 || current_weather$list[[1]]$wind$speed < 9.0) { 
      fireThreshold <- fireThreshold+0.18 } 
    else if(current_weather$list[[1]]$wind$speed > 9.1 || current_weather$list[[1]]$wind$speed < 11.0) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(current_weather$list[[1]]$wind$speed > 11.1 || current_weather$list[[1]]$wind$speed < 13.0) { 
      fireThreshold <- fireThreshold+0.25 } 
    #HUMIDITY 
    if(current_weather$list[[1]]$main$humidity > 30 || current_weather$list[[1]]$main$humidity < 40) { 
      fireThreshold <- fireThreshold+0.15 } 
    else if(current_weather$list[[1]]$main$humidity > 41 || current_weather$list[[1]]$main$humidity < 70) { 
      fireThreshold <- fireThreshold+0.10 }
    else if(current_weather$list[[1]]$main$humidity > 71 || current_weather$list[[1]]$main$humidity < 100) { 
      fireThreshold <- fireThreshold+0.05 } 
    else if(current_weather$list[[1]]$main$humidity > 101 || current_weather$list[[1]]$main$humidity < 200) { 
      fireThreshold <- fireThreshold+0.03 } 
    #TEMP
    if(current_weather$list[[1]]$main$temp > 200 || current_weather$list[[1]]$main$temp < 300) { 
      fireThreshold <- fireThreshold+.38 }
    else if(current_weather$list[[1]]$main$temp > 100 || current_weather$list[[1]]$main$temp < 200) { 
      fireThreshold <- fireThreshold+0.35 }      
    else if(current_weather$list[[1]]$main$temp > 90 || current_weather$list[[1]]$main$temp < 99) { 
      fireThreshold <- fireThreshold+0.3 }
    else if(current_weather$list[[1]]$main$temp > 80 || current_weather$list[[1]]$main$temp < 89) { 
      fireThreshold <- fireThreshold+0.22 }
    else if(current_weather$list[[1]]$main$temp > 70 || current_weather$list[[1]]$main$temp < 79) { 
      fireThreshold <- fireThreshold+.2 } 
    else if(current_weather$list[[1]]$main$temp > 60 || current_weather$list[[1]]$main$temp < 69) { 
      fireThreshold <- fireThreshold+.15 }
    else if(current_weather$list[[1]]$main$temp > 40 || current_weather$list[[1]]$main$temp < 59) { 
      fireThreshold <- fireThreshold+.1  }
    else if(current_weather$list[[1]]$main$temp > 20 || current_weather$list[[1]]$main$temp < 39) { 
      fireThreshold <- fireThreshold+.05  }
    else if(current_weather$list[[1]]$main$temp > 10 || current_weather$list[[1]]$main$temp < 19) { 
      fireThreshold <- fireThreshold+.02 }
    else if(current_weather$list[[1]]$main$temp > 0 || current_weather$list[[1]]$main$temp < 9) { 
      fireThreshold <- fireThreshold+.0 }
    #ATMOSPHERIC PRESSURE-10.2
    #Conversion: 10.2PSI(10'000 feet fire will not burn) - 689.4757293178 hPa
    if(current_weather$list[[1]]$main$pressure < 689.4757293178) { 
      return(FALSE)
    }
    
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
    # print("Wind Speed:")
    # print(current_weather$list[[4]]$wind$speed)
    # print("Wind Direction:")
    # print(current_weather$list[[4]]$wind$deg)
  }, error = function(e) {
    start <- FALSE
  }, warning = function(w) {
    start <- FALSE
  })
  
  vegetation <- getVegetation(long, lat, localVegetation)
  if(is.null(vegetation) || vegetation < .25) {
    print("Vegetation too low:")
    print(vegetation)
    return(FALSE)
  }
  
}

fireGrow <- function(long, lat) {
  
}