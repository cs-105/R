#install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)

res = GET('https://api.openweathermap.org/data/2.5/weather?q=Santa%20Barbara&appid=60d5dc43ff743a24068d3eb20a06aad9')

data = fromJSON(rawToChar(res$content))

print(data$weather$main)
