library(owmr)
library(rjson)

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 
API_key = "ac66c8209bdf887068a2a79e4fdbca33"

fireStart <- function(long, lat) {
  date = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
  url <- paste0("http://history.openweathermap.org/data/2.5/history/city?lat=",lat
                , "&lon=", long
                , "&type=hour"
                , "&cnt=4"
                , "&appid=", API_key)
  historical_weather <- fromJSON(file = url)
  print(historical_weather)
}

fireSpread <- function(long, lat) {
  
}