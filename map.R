library(shiny)
library(leaflet)
library(shinyjs)

long <- 0.0
lati <- 0.0

js_save_map_instance <- HTML(
  paste(
    "var mapsPlaceholder = [];",
    "L.Map.addInitHook(function () {",
    "   mapsPlaceholder.push(this); // Use whatever global scope variable you like.",
    "});", sep = "\n"
  )
)

js_open_popup <- HTML(
  paste("function open_popup(id) {",
        "   console.log('open popup for ' + id);",
        "   mapsPlaceholder[0].eachLayer(function(l) {",
        "      if (l.options && l.options.layerId == id) {",
        "         l.openPopup();",
        "      }",
        "   });",
        "}", sep = "\n"
  )
)

ui <- fluidPage(
  tags$head(
    tags$script(type = "text/javascript",
                js_save_map_instance,
                js_open_popup),
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
                                 id)#,
      #addEasyButton(map, easyButton(id = "startFire", title = "Start Fire", position = "topright"))
      )
    
    long <<- click$lng
    lati <<- click$lat
    print("")
    print("Coords are:")
    print("Long: ") 
    print(long)
    print("Lat: ") 
    print(lati)
    
    runjs(sprintf("setTimeout(() => open_popup('%s'), 10)", id))
  })
  
  observeEvent(input$startFire, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    leafletProxy("map") %>% 
      addCircles(lng = long, lat = lati, weight = 1, radius = 3000, color = "#FF2C00", group = "fires")
  })
  
  observeEvent(input$endFires, {
    
    leafletProxy("map") %>% 
      clearMarkers()
    
    leafletProxy("map") %>% 
      clearGroup("fires")
  })
}

shinyApp(ui, server)