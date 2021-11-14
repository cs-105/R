library(shiny)
library(leaflet)
library(shinyjs)
library(owmr)
library(plyr)

source("predict-functions.R")
source("read-csv.R")

API_key = "ac66c8209bdf887068a2a79e4fdbca33"

long <- 0.0
lat <- 0.0
long_lat_list <- list()

ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript"),
    useShinyjs()
  ),
  fluidRow(
      actionButton("startFire", "Start Fire"),
      actionButton("endFires", "End Fire(s)")
    ),
  fluidRow(
    leafletOutput("map")
  )
)

server <- function(input, output, session) {
  map <- leaflet() %>%
    addTiles() %>%
    fitBounds(-124.848974, 24.396308, -66.885444, 49.384358)
    
  
  output$map <- renderLeaflet(
    map
  )
  
  observeEvent(input$map_click, {
    id <- paste0("marker", input$map_click)
    click <- input$map_click
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    leafletProxy("map") %>%
      addMarkers(click$lng, click$lat, id,
                 "mymarkers",
                 popup = sprintf("%.2f / %.2f: %s", click$lng, click$lat,
                                 id))
    
    long <<- click$lng
    lat <<- click$lat
    print("")
    round_any(long, .1, f = floor) + .05
    round_any(lat, .1, f = floor) + .05
    print("")
    print("Coords are:")
    print("Long: ") 
    print(long)
    print("Lat: ") 
    print(lat)
    
    runjs(sprintf("setTimeout(() => open_popup('%s'), 10)", id))
  })
  
  observeEvent(input$startFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    vegetation <- getVegetation(long, lat)
    
    if (willFireStart(long, lat, vegetation)) {
      url <-
        paste0(
          "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
          , "&lon=", long
          , "&appid=", API_key
          , "&cnt=", 1
          , "&units=imperial"
        )
      weather <- fromJSON(file = url)
      windDirection <- weather$list[[1]]$wind$deg
      windSpeed <- weather$list[[1]]$wind$speed
      
      long_lat_list <- append(long_lat_list, list(list(long, lat)))
      long_lat_list <- append(long_lat_list, fireGrow(long_lat_list, 1, windDirection, windSpeed, vegetation))
      long_lat_list <- unique(long_lat_list)
      
      for (i in seq(2, length(long_lat_list))) {
        
          long <- long_lat_list[[i-1]][[1]]
          lat <- long_lat_list[[i-1]][[2]]
          
          leafletProxy("map") %>% 
            addCircles(lng = long, lat = lat, weight = 1, radius = 20, color = "#FF2C00", group = "fires")
      }
      
      leafletProxy("map") %>% 
        sliderInput(inputId = "time", label = "Select time since inception (in hours)", min = 0, max = 96, value = 0, step = 4)

      print(long_lat_list)
    } else {
      content <- paste(sep = "<br/>",
          "Fire will not start.",
          "Try a new location."
          )
      leafletProxy("map") %>% 
        addPopups(long, lat, content, options = popupOptions((closeButton = TRUE)))
    }
  })
  
  observeEvent(input$endFires, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    leafletProxy("map") %>% 
      clearGroup("fires")
  })
}

shinyApp(ui, server)
