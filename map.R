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
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("setHours", "Hours:",
                  min = 0, max = 96,
                  value = 0)
    ),
    mainPanel(
      tableOutput("values")
    )
  ),
  fluidRow(
    actionButton("updateFire", "Burn"),
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
    vegetation <- readVeg()
    
    if (willFireStart(long, lat, vegetation)) {
      
      leafletProxy("map") %>% 
        addCircles(lng = long, lat = lat, weight = 1, radius = 20, color = "#FF2C00", group = "fires")
      
    } else {
      content <- paste(sep = "<br/>",
                       "Fire will not start.",
                       "Try a new location."
      )
      leafletProxy("map") %>% 
        addPopups(long, lat, content, options = popupOptions((closeButton = TRUE)))
    }
  })
  
  observeEvent(input$updateFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    leafletProxy("map") %>% 
      clearGroup("fires")
    
    print(input$setHours)
    
    vegetation <- readVeg()
    
    url <-
      paste0(
        "http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lat
        , "&lon=", long
        , "&appid=", API_key
        , "&cnt=", input$setHours
        , "&units=imperial"
      )
    weather <- fromJSON(file = url)
    
    long_lat_list <- append(long_lat_list, list(list(long, lat)))
    long_lat_list <- append(long_lat_list, fireGrow(long_lat_list, input$setHours, weather, vegetation, 1))
    long_lat_list <- unique(long_lat_list)
    
    for (i in seq(2, length(long_lat_list))) {
      
      long <- long_lat_list[[i-1]][[1]]
      lat <- long_lat_list[[i-1]][[2]]
      
      leafletProxy("map") %>% 
        addCircles(lng = long, lat = lat, weight = 1, radius = 20, color = "#FF2C00", group = "fires")
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