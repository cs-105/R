library(owmr)
library(rjson)

source("read-csv.R")

API_key = "ac66c8209bdf887068a2a79e4fdbca33"
global_long_lat <- list()
weather <- NULL

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
    print(historical_weather)
    print("Wind Speed:")
    print(historical_weather$list[[4]]$wind$speed)
    print("Wind Direction:")
    print(historical_weather$list[[4]]$wind$deg)
  }, error = function(e) {
    start <- FALSE
  }, warning = function(w) {
    start <- FALSE
  })
  
  if(is.null(localVegetation) || localVegetation < .25) {
    return(FALSE)
  }
  
  
  print(" ")
  print("Long: ")
  print(long)
  print("Lat: ")
  print(lat)
  # print("Vegetation:")
  # print(getVegetation(long, lat))
  return(start)
}

willFireSpread <- function(long, lat, localWeather, localVegetation) {
  
  weather <- localWeather

  if (is.null(localVegetation) || localVegetation < .25) {
    return(FALSE)
  }
  return(TRUE)
}

# fireGrow <- function(longLatList, count, windDeg) {
#   print(count)
#   if (length(longLatList) == 0 || count == -1) {
#     return(list(list()))
#   }
# 
#   wind <- windDeg
#   local_long_lat <- list()
# 
#   for (i in seq(1, length(longLatList))) {
#       long <- longLatList[[i]][[1]]
#       lat <- longLatList[[i]][[2]]
# 
#       if (count %% 5 == 1) {
#         if(i == 1) {
#           url <-
#             paste0(
#               "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
#               , "&lon=", long
#               , "&appid=", API_key
#               , "&cnt=", count
#               , "&units=imperial"
#             )
#           weather <- fromJSON(file = url)
#           # print(weather)
#           wind <- weather$list[[count]]$wind$deg
#         }
#       }
# 
# 
#       # If the wind is blowing North
#       if (wind >= 135 && wind < 225) {
#         if (willFireSpread(long, (lat + 0.00017)) && !(list(long, (lat + 0.00017)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list(long, (lat + 0.00017))))
#         }
#         if (willFireSpread((long - 0.00026), (lat + 0.00026)) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
#         }
#         if (willFireSpread((long + 0.00026), (lat + 0.00026)) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
#         }
#       }
# 
#       # If the wind is blowing East
#       if (wind >= 225 && wind < 315) {
#         if (willFireSpread((long + 0.000215), lat) && !(list((long + 0.000215), lat) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long + 0.000215), lat)))
#         }
#         if (willFireSpread((long + 0.00026), (lat + 0.00026)) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
#         }
#         if (willFireSpread((long + 0.00026), (lat - 0.00026)) && !(list((long + 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat - 0.00026))))
#         }
#       }
# 
#       # If the wind is blowing South
#       if (wind >= 315 || wind < 45) {
#         if (willFireSpread(long, (lat - 0.00017)) && !(list(long, (lat - 0.00017)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list(long, (lat - 0.00017))))
#         }
#         if (willFireSpread((long + 0.00026), (lat - 0.00026)) && !(list((long + 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat - 0.00026))))
#         }
#         if (willFireSpread((long - 0.00026), (lat - 0.00026)) && !(list((long - 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat - 0.00026))))
#         }
#       }
# 
#       # If the wind is blowing West
#       if (wind >= 45 && wind < 135) {
#         if (willFireSpread((long - 0.000215), lat) && !(list((long - 0.000215), lat) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long - 0.000215), lat)))
#         }
#         if (willFireSpread((long - 0.00026), (lat - 0.00026)) && !(list((long - 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat - 0.00026))))
#         }
#         if (willFireSpread((long - 0.00026), (lat + 0.00026)) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
#           local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
#         }
#       }
#     }
# 
#   global_long_lat <- append(global_long_lat, local_long_lat)
#   return(local_long_lat <- append(local_long_lat, fireGrow(local_long_lat, (count - 1), wind)))
# }

fireGrow <- function(longLatList, count, windDeg, localWindSpeed, localVegetation) {
  print(count)
  if (length(longLatList) == 0 || count == 0) {
    return(list(list()))
  }
  
  windDirection <- windDeg
  windSpeed <- localWindSpeed
  vegetation <- localVegetation
  local_long_lat <- list()
  
  for (i in seq(1, length(longLatList))) {
    long <- longLatList[[i]][[1]]
    lat <- longLatList[[i]][[2]]
    
    if (count %% 5 == 1) {
      if(i == 1) {
        url <-
          paste0(
            "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
            , "&lon=", long
            , "&appid=", API_key
            , "&cnt=", count
            , "&units=imperial"
          )
        weather <- fromJSON(file = url)
        # print(weather)
        windDirection <- weather$list[[count]]$wind$deg
        windSpeed <- weather$list[[count]]$wind$speed
        
        vegetation <- getVegetation(long, lat)
      }
    }
    
    if (windSpeed < -6) {
      # If the wind is blowing North
      if (windDirection >= 135 && windDirection < 225) {
        if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
      }
    
      # If the wind is blowing East
      if (windDirection >= 225 && windDirection < 315) {
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat - 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat - 0.00026))))
        }
      }
      
      # If the wind is blowing South
      if (windDirection >= 315 || windDirection < 45) {
        if (willFireSpread((long + 0.00026), (lat - 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat - 0.00026))))
        }
        if (willFireSpread((long - 0.00026), (lat - 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat - 0.00026))))
        }
      }
    
      # If the wind is blowing West
      if (windDirection >= 45 && windDirection < 135) {
        if (willFireSpread((long - 0.00026), (lat - 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat - 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat - 0.00026))))
        }
         if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
         local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
       }
      }
    }
    
    else {
      print(windDirection)
      # If the wind is blowing North
      if (windDirection >= 157.5 && windDirection < 202.5) {
        if (willFireSpread((long - 0.00005), (lat + 0.0008), weather, vegetation) && !(list((long - 0.00005), (lat + 0.0008)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00005), (lat + 0.0008))))
        }
        if (willFireSpread((long + 0.00005), (lat + 0.0008), weather, vegetation) && !(list((long + 0.00005), (lat + 0.0008)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00005), (lat + 0.0008))))
        }
      }
      
      # If the wind is blowing North East
      if (windDirection >= 202.5 && windDirection < 247.5) {
        if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
      }
      
      # If the wind is blowing East
      if (windDirection >= 247.5 && windDirection < 292.5) {
        if (willFireSpread((long + 0.00035), (lat + 0.00015), weather, vegetation) && !(list((long + 0.00035), (lat + 0.00015)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00035), (lat + 0.00015))))
        }
        if (willFireSpread((long + 0.00035), (lat - 0.00015), weather, vegetation) && !(list((long + 0.00035), (lat - 0.00015)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00035), (lat - 0.00015))))
        }
      }
      
      # If the wind is blowing South East
      if (windDirection >= 292.5 && windDirection < 337.5) {
        if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
      }
      
      # If the wind is blowing South
      if (windDirection >= 337.5 || windDirection < 22.5) {
        if (willFireSpread((long + 0.00015), (lat - 0.00035), weather, vegetation) && !(list((long + 0.00015), (lat - 0.00035)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00015), (lat - 0.00035))))
        }
        if (willFireSpread((long - 0.00015), (lat - 0.00035), weather, vegetation) && !(list((long - 0.00015), (lat - 0.00035)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00015), (lat - 0.00035))))
        }
      }
      
      # If the wind is blowing South West
      if (windDirection >= 22.5 && windDirection < 67.5) {
        if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
      }
      
      # If the wind is blowing West
      if (windDirection >= 67.5 && windDirection < 112.5) {
        if (willFireSpread((long - 0.00035), (lat - 0.00015), weather, vegetation) && !(list((long - 0.00035), (lat - 0.00015)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00035), (lat - 0.00015))))
        }
        if (willFireSpread((long - 0.00035), (lat + 0.00015), weather, vegetation) && !(list((long - 0.00035), (lat + 0.00015)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.000355), (lat + 0.00015))))
        }
      }
      
      # If the wind is blowing North West
      if (windDirection >= 112.5 && windDirection < 157.5) {
        if (willFireSpread((long - 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long - 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long - 0.00026), (lat + 0.00026))))
        }
        if (willFireSpread((long + 0.00026), (lat + 0.00026), weather, vegetation) && !(list((long + 0.00026), (lat + 0.00026)) %in% global_long_lat)) {
          local_long_lat <- append(local_long_lat, list(list((long + 0.00026), (lat + 0.00026))))
        }
      }
    }
  }
  
  global_long_lat <- append(global_long_lat, local_long_lat)
  return(local_long_lat <- append(local_long_lat, fireGrow(local_long_lat, (count - 1), windDirection, vegetation)))
}

