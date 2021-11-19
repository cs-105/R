library(shiny)
library(leaflet)
library(shinyjs)
library(owmr)
library(plyr)

source("predict-functions.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 

long <- 0.0
lati <- 0.0
burn_area <- c()
grid <- c()
directions <- list(list(1,1),list(1,-1),list(-1,1),list(-1,-1),list(1,0),list(0,1),list(-1,0),list(0,-1))
grid_density <- 100

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

    hours <<- input$setHours

    for(i in 1:grid_density){
      row_i <- c()
      for(j in 1:grid_density){
        lon <- long + i/50 - 1
        lat <- lati + j/50 - 1
        row_i[[j]] <- list(lon, lat, 0, 0, i, j)
      }
      grid[[i]] <<- row_i
    }
    
    print('done')
    
    grow_fire(grid, list(list(50,50)),0)
    
    draw_fire()
    
    # for(i in seq(from=1, to=length(grid), by=50)){
    #   for(j in seq(from=1, to=length(grid[[i]]), by=50)){
    # 
    #     lon <- grid[[i]][[j]][[1]]
    #     lat <- grid[[i]][[j]][[2]]
    # 
    #     leafletProxy("map") %>%
    #       addCircles(lng = lon, lat = lat, weight = 1, radius = 10, color = "#FF2C00", group = "fires")
    # 
    #   }
    # }
  })
  
  grow_fire <- function(grid, spread_squares, d){
    if(d>40){
      return()
    }
    new_spread_squares <- c()
    
    for(s in spread_squares){
      #ss_lon is spread square indes lon
      ss_lon = s[[1]]
      ss_lat = s[[2]]
      
      for(dir in directions){
        
        x <- dir[[1]]
        y <- dir[[2]]
        
        if(grid[[ss_lon+x]][[ss_lat+y]][[3]] == 0 & grid[[ss_lon+x]][[ss_lat+y]][[4]] < 3){
          if(will_spread(grid[[ss_lon]][[ss_lat]], grid[[ss_lon+x]][[ss_lat+y]])){
            new_spread_squares <- append(new_spread_squares, list(list(ss_lon+x, ss_lat+y)))
          }
          grid[[ss_lon+x]][[ss_lat+y]][[3]] <- 1
          grid[[ss_lon+x]][[ss_lat+y]][[4]] <- grid[[ss_lon+x]][[ss_lat+y]][[4]]+1
        }
      }
    }
    
    burn_area <<- append(burn_area, new_spread_squares)
    
    print(d)

    grow_fire(grid, new_spread_squares, d+1)
  }
  
  draw_fire <- function(){
    for(pos in burn_area){
      lon <- grid[[pos[[1]]]][[pos[[2]]]][[1]]
      lat <- grid[[pos[[1]]]][[pos[[2]]]][[2]]
      leafletProxy("map") %>%
        addCircles(lng = lon, lat = lat, weight = 1, radius = 500, color = "#FF2C00", group = "fires")
    }
  }
  
  will_spread <- function(cell1, cell2){
    lon <- cell2[[1]]
    lat <- cell2[[2]]
    vegetation <- getVegetation(lon, lat)
    if (is.null(vegetation) || vegetation < .2) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  observeEvent(input$startFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    
    if (willFireStart(long, lati)) {
      leafletProxy("map") %>% 
        addCircles(lng = long, lat = lati, weight = 1, radius = 100, color = "#000000", group = "fires")
    
      leafletProxy("map") %>% 
        sliderInput(inputId = "time", label = "Select time since inception (in hours)", min = 0, max = 96, value = 0, step = 4)
      
      fireGrow(long, lati)
    } else {
      content <- paste(sep = "<br/>",
          "Fire will not start.",
          "Try a new location."
          )
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
