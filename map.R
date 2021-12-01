library(shiny)
library(leaflet)
library(shinyjs)
library(owmr)
library(plyr)

source("predict-functions.R")

Sys.setenv(OWM_API_KEY = "ac66c8209bdf887068a2a79e4fdbca33") 

long <- 0.0
lati <- 0.0
hour <<- 0
weather <<- NULL
wind_speed <<- 0
wind_dir <<- 0
burn_area <- c()
grid <- c()
weather_grid <- c()
directions <- list(list(1,1),list(1,-1),list(-1,1),list(-1,-1),list(1,0),list(0,1),list(-1,0),list(0,-1))
grid_density <- 100

ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript"),
    useShinyjs()
  ),
  fluidRow(
    leafletOutput("map")
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sethour", "hour:",
                  min = 1, max = 48,
                  value = 1),
    ),
    mainPanel(
      tableOutput("values")
    )
  ),
  fluidRow(
    actionButton("updateFire", "Burn"),
    actionButton("endFires", "End Fire(s)")
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
  })
  
  observeEvent(input$updateFire, {
    print("Starting burn")
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    if(willFireStart(long, lati)){

      leafletProxy("map") %>%
        addCircles(lng = long, lat = lati, weight = 1, radius = 800, color = "#000000", group = "fires")

      print("Fire is starting")
      hour <<- input$sethour
      grid <<- c()
      burn_area <<- c()

      for(i in 1:grid_density){
        row_i <- c()
        for(j in 1:grid_density){
          lon <- long + i/50 - 1
          lat <- lati + j/50 - 1
          row_i[[j]] <- list(lon, lat, 0, 0, i, j)
        }
        grid[[i]] <<- row_i
      }
      grid[[50]][[50]][[3]] <- 1
    
      url <- paste0("http://pro.openweathermap.org/data/2.5/forecast/hourly?lat=", lati
                  ,"&lon=", long
                  ,"&appid=", API_key
                  ,"&cnt=", 48)
      weather <<- fromJSON(file = url)
      wind_speed <<- weather$list[[1]]$wind$speed
      wind_dir <<- weather$list[[1]]$wind$deg

      grow_fire(grid, list(list(50,50)),1, hour)
    
      draw_fire()
    }
    else {
      content <- paste(sep = "<br/>",
                       "Fire will not start.",
                       "Try a new location."
      )
        leafletProxy("map") %>% 
          addPopups(long, lati, content, options = popupOptions((closeButton = TRUE)))
    }
  })
  
  # Grid is the grid built that the fire can spread to
  # spread_squares are the squares on fire that are currently spreading the fire
  # Depth is the depth of the recursion.  How many squares deep will the fire spread to
  
  grow_fire <- function(grid, spread_squares, d, hour){
    if(d>ceiling(hour/2)){
      return()
    }
    new_spread_squares <- c()
    
    for(s in spread_squares){
      # ss_lon is spread square index lon
      # ss_laat is spread square index lat
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
    
    wind_speed <<- weather$list[[d+1]]$wind$speed
    wind_dir <<- weather$list[[d+1]]$wind$deg

    print(wind_speed)
    print(deg_to_NSEW(wind_dir))
    
    grow_fire(grid, new_spread_squares, d+1, hour)
  }
  
  will_spread <- function(cell1, cell2){
    lon <- cell2[[1]]
    lat <- cell2[[2]]
    vegetation <- getVegetation(lon, lat)

    
    # gets the cell change vector
    x <- cell2[[1]]-cell1[[1]]
    if(x > 0)
      x <- 1
    else if(x < 0)
      x <- -1
    else
      x <- 0
    
    y <- cell2[[2]]-cell1[[2]]
    if(y > 0)
      y <- 1
    else if(y < 0)
      y <- -1
    else
      y <- 0
    
    # Grabbing the direction of the cell we're trying to spread to
    cell_dir <- 0
    if(x==0&y==1)
      cell_dir <- 0
    else if(x==1&y==1)
      cell_dir <- 45
    else if(x==1&y==0)
      cell_dir <- 90
    else if(x==1&y==-1)
      cell_dir <- 135
    else if(x==0&y==-1)
      cell_dir <- 180
    else if(x==-1&y==-1)
      cell_dir <- 225
    else if(x==-1&y==0)
      cell_dir <- 270
    else if(x==-1&y==1)
      cell_dir <- 315
      
    # direction difference between wind and cell direction
    dd <- abs(wind_dir - cell_dir)
    if (dd > 180) {
      dd <- 360 - dd
    }
    if (dd > 120) {
      wind_prob <- (wind_speed / 2) + log(dd)
    } else if (dd < 60) {
      wind_prob <- log(dd) - (wind_speed / 2)
    } else {
      wind_prob <- log(dd)
    }
    rand <- sample(1:10, 1)
    prob = (6 * log(1 + vegetation)) + wind_prob
    
    if (is.null(vegetation) || vegetation > 1 || prob < rand ) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  draw_fire <- function(){
    for(pos in burn_area){
      lon <- grid[[pos[[1]]]][[pos[[2]]]][[1]]
      lat <- grid[[pos[[1]]]][[pos[[2]]]][[2]]
      leafletProxy("map") %>%
        addCircles(lng = lon, lat = lat, weight = 1, radius = 800, color = "#FF2C00", group = "fires")
    }
  }
  
  deg_to_NSEW <- function(deg){
    dir_list <- list('SW','W','NW','N','NE','E','SE')
    if(wind_dir > 22.5 & wind_dir < 337.5){
      return(dir_list[[ceiling((wind_dir - 22.5) / 45)]])
  }
    else
      return('S')
  }
  
  observeEvent(input$endFires, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    leafletProxy("map") %>% 
      clearGroup("fires")
  })
}

shinyApp(ui, server)
