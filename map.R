library(shiny)
library(leaflet)
library(shinyjs)
library(owmr)
library(plyr)

source("predict-functions.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 

long <- 0.0
lati <- 0.0

ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript"),
    useShinyjs()
  ),
  fluidRow(
<<<<<<< HEAD
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
=======
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
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
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
    lati <<- click$lat
    print("")
    round_any(long, .1, f = floor) + .05
    round_any(lati, .1, f = floor) + .05
    print("")
    print("Coords are:")
    print("Long: ") 
    print(long)
    print("Lat: ") 
    print(lati)
    
    runjs(sprintf("setTimeout(() => open_popup('%s'), 10)", id))
  })
  
  observeEvent(input$updateFire, {
    print(input$setHours)
    
    hours <<- input$setHours
    
    radius <<- 0.00015*hours
    
    # North wind 0
    # West wind 90
    # South wind 180
    # East wind 270
    wind_dir <<- pi/2
    
    for(i in 1:(hours*5)){
      
      deg <<- i*2*pi/(hours*5)
      
      x <<- cos(i*2*pi/(hours*5))*radius*1.25
      y <<- sin(i*2*pi/(hours*5))*radius
<<<<<<< HEAD
      
=======
        
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
      # wo = wind offset
      print('wo')
      wo_x <<- (cos(deg)+1)/(cos(wind_dir)+1)/1
      print(wo_x)
      if(deg < 90 | deg > 270){
        wo_x <<- x*wo_x/10
      }else{
        wo_x <<- -x*wo_x/10
      }
      wo_y <<- (sin(deg)*radius)/20
      
      new_x <<- x + wo_x
<<<<<<< HEAD
      
=======

>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
      leafletProxy("map") %>% 
        addCircles(lng = long+x, lat = lati+y, weight = 1, radius = 10, color = "#FF2C00", group = "fires")
    }
    
  })
  
  observeEvent(input$startFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    
    if (willFireStart(long, lati)) {
      leafletProxy("map") %>% 
        addCircles(lng = long, lat = lati, weight = 1, radius = 10, color = "#FF2C00", group = "fires")
<<<<<<< HEAD
      
=======
    
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
      leafletProxy("map") %>% 
        sliderInput(inputId = "time", label = "Select time since inception (in hours)", min = 0, max = 96, value = 0, step = 4)
      
      fireGrow(long, lati)
    } else {
      content <- paste(sep = "<br/>",
<<<<<<< HEAD
                       "Fire will not start.",
                       "Try a new location."
      )
=======
          "Fire will not start.",
          "Try a new location."
          )
>>>>>>> 54c77b4caa1314fc0e39aa7833bad6defe0ab677
      leafletProxy("map") %>% 
        addPopups(long, lati, content, options = popupOptions((closeButton = TRUE)))
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
