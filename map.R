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
    
    # size of precidtion 100 x 100
    
    # number of squares x by x
    num_sqr <- 100
    for(i in 1:num_sqr){
      list_j <- c()
      for(j in 1:num_sqr){
        lon <- long + i/50 - 1
        lat <- lati + j/50 - 1
        list_j[[j]] <- list(lon, lat, 0, 0, i, j)
      }
      grid[[i]] <<- list_j
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
  
  grow_fire <- function(grid, f_s, d){
    if(d>40){
      return()
    }
    new_f_s <- c()
    
    for(s in f_s){
      
      s_i = s[[1]]
      s_j = s[[2]]
      
      if(grid[[s_i+1]][[s_j+1]][[3]] == 0 & grid[[s_i+1]][[s_j+1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i+1]][[s_j+1]])){
          new_f_s <- append(new_f_s, list(list(s_i+1, s_j+1)))
        }
        grid[[s_i+1]][[s_j+1]][[3]] <- 1
      }
      if(grid[[s_i+1]][[s_j-1]][[3]] == 0 & grid[[s_i+1]][[s_j-1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i+1]][[s_j-1]])){
          new_f_s <- append(new_f_s, list(list(s_i+1, s_j-1)))
        }
        grid[[s_i+1]][[s_j-1]][[3]] <- 1
      }
      if(grid[[s_i-1]][[s_j+1]][[3]] == 0 & grid[[s_i-1]][[s_j+1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i-1]][[s_j+1]])){
          new_f_s <- append(new_f_s, list(list(s_i-1, s_j+1)))
        }
        grid[[s_i-1]][[s_j+1]][[3]] <- 1
      }
      if(grid[[s_i-1]][[s_j-1]][[3]] == 0 & grid[[s_i-1]][[s_j-1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i-1]][[s_j-1]])){
        new_f_s <- append(new_f_s, list(list(s_i-1, s_j-1)))
        }
        grid[[s_i-1]][[s_j-1]][[3]] <- 1
      }
      if(grid[[s_i+1]][[s_j]][[3]] == 0 & grid[[s_i+1]][[s_j]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i+1]][[s_j]])){
          new_f_s <- append(new_f_s, list(list(s_i+1, s_j)))
        }
        grid[[s_i+1]][[s_j]][[3]] <- 1
      }
      if(grid[[s_i]][[s_j+1]][[3]] == 0 & grid[[s_i]][[s_j+1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i]][[s_j+1]])){
          new_f_s <- append(new_f_s, list(list(s_i, s_j+1)))
        }
        grid[[s_i]][[s_j+1]][[3]] <- 1
      }
      if(grid[[s_i-1]][[s_j]][[3]] == 0 & grid[[s_i-1]][[s_j]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i-1]][[s_j]])){
          new_f_s <- append(new_f_s, list(list(s_i-1, s_j)))
        }
        grid[[s_i-1]][[s_j]][[3]] <- 1
      }
      if(grid[[s_i]][[s_j-1]][[3]] == 0 & grid[[s_i]][[s_j-1]][[4]] == 0){
        if(will_spread(grid[[s_i]][[s_j]], grid[[s_i]][[s_j-1]])){
          new_f_s <- append(new_f_s, list(list(s_i, s_j-1)))
        }
        grid[[s_i]][[s_j-1]][[3]] <- 1
      }
    }
    
    burn_area <<- append(burn_area, new_f_s)
    
    print(d)

    grow_fire(grid, new_f_s, d+1)
  }
  
  draw_fire <- function(){
    for(pos in burn_area){
      lon <- grid[[pos[[1]]]][[pos[[2]]]][[1]]
      lat <- grid[[pos[[1]]]][[pos[[2]]]][[2]]
      leafletProxy("map") %>%
        addCircles(lng = lon, lat = lat, weight = 1, radius = 80, color = "#FF2C00", group = "fires")
    }
  }
  
  will_spread <- function(cell1, cell2){
    lon <- cell2[[1]]
    lat <- cell2[[2]]
    vegetation <- getVegetation(lon, lat)
    if (is.null(vegetation) || vegetation < .325) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  observeEvent(input$startFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    
    if (willFireStart(long, lati)) {
      leafletProxy("map") %>% 
        addCircles(lng = long, lat = lati, weight = 1, radius = 10, color = "#FF2C00", group = "fires")
    
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
