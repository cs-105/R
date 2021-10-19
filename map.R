library(leaflet)
library(shiny)
  
map <- leaflet() %>% addTiles() %>% fitBounds(map, -124.848974, 24.396308, -66.885444, 49.384358)
map 