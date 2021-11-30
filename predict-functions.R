library(owmr)
library(rjson)

source("read-csv.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 
API_key = "ac66c8209bdf887068a2a79e4fdbca33"

willFireStart <- function(long, lat) {
  print("Enter Predict")
  
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
  }, error = function(e) {
    print("Fire will not start (Weather error)")
    return(FALSE)
  }, warning = function(w) {
    print("Fire will not start (Weather error)")
    return(FALSE)
  })
  
  
    
  ##RAIN - IF ITS RAINING, RETURN FALSE
  ##RAIN all historical counts 
  rain1 <- historical_weather$list[[1]]$weather[[1]]$main 
  rain2 <- historical_weather$list[[2]]$weather[[1]]$main
  rain3 <- historical_weather$list[[3]]$weather[[1]]$main
  rain4 <- historical_weather$list[[4]]$weather[[1]]$main
  if(rain1 == "Rain" || rain2 == "Rain" || rain3 == "Rain" || rain4 == "Rain") { 
    print("Fire will not start (Rain)")
    return(FALSE)
  }
    
  ##WIND  
  if(historical_weather$list[[1]]$wind$speed <= 3.0) { 
    fireThreshold <- fireThreshold+0.2 }
  else if(historical_weather$list[[1]]$wind$speed <= 5.0) { 
    fireThreshold <- fireThreshold+0.25 }
  else if(historical_weather$list[[1]]$wind$speed <= 7.0) { 
    fireThreshold <- fireThreshold+0.3 } 
  else if(historical_weather$list[[1]]$wind$speed > 7.0) { 
    fireThreshold <- fireThreshold+0.4 } 
    
  #HUMIDITY 
  if (historical_weather$list[[1]]$main$humidity <= 20) { 
    fireThreshold <- fireThreshold+0.5 } 
  else if(historical_weather$list[[1]]$main$humidity <= 30) { 
    fireThreshold <- fireThreshold+0.4 } 
  else if(historical_weather$list[[1]]$main$humidity <= 50) { 
    fireThreshold <- fireThreshold+0.3 }
  else if(historical_weather$list[[1]]$main$humidity > 50) { 
    fireThreshold <- fireThreshold+0.1 } 
    
  #TEMP
  if(historical_weather$list[[1]]$main$temp >= 100) { 
    fireThreshold <- fireThreshold+0.6 }      
  else if(historical_weather$list[[1]]$main$temp >= 90) { 
    fireThreshold <- fireThreshold+0.5 }
  else if(historical_weather$list[[1]]$main$temp >= 80) { 
    fireThreshold <- fireThreshold+0.4 }
  else if(historical_weather$list[[1]]$main$temp >= 70) { 
    fireThreshold <- fireThreshold+.3 } 
  else if(historical_weather$list[[1]]$main$temp >= 60) { 
    fireThreshold <- fireThreshold+.2 }
  else if(historical_weather$list[[1]]$main$temp >= 40) { 
    fireThreshold <- fireThreshold+.15  }
  else if(historical_weather$list[[1]]$main$temp < 40) { 
    fireThreshold <- fireThreshold+.1  }

  #ATMOSPHERIC PRESSURE-10.2
  #Conversion: 10.2PSI(10'000 feet fire will not burn) - 689.4757293178 hPa
  if(historical_weather$list[[1]]$main$pressure < 689.4757293178) { 
    print("Fire will not start (Pressure)")
    return(FALSE)
  }
  
  vegetation <- getVegetation(long, lat)
  if(is.null(vegetation) || vegetation < .25) {
    print("Vegetation too low:")
    print(vegetation)
    print("Fire will not start")
    return(FALSE)
  }
  
  if(fireThreshold >= 1) {
    print("Fire will start")
    print(fireThreshold)
    return(TRUE)
  } else {
    print("Fire will not start (Threshold too low)")
    print(fireThreshold)
    return(FALSE)
  }
}