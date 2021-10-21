library(shiny)
library(leaflet)
library(shinyjs)

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
  wellPanel(
    fluidRow(
      sliderInput("lng", "Longitude:", -126, -66, -126, 0.01),
      sliderInput("lat", "Latitude:", 24, 50, 24, 0.01),
      actionButton("add", "Add Marker")
    )
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
  
  observeEvent(input$add, {
    id <- paste0("marker", input$add)
    leafletProxy("map") %>%
      addMarkers(input$lng, input$lat, id,
                 "mymarkers",
                 popup = sprintf("%.2f / %.2f: %s", input$lng, input$lat,
                                 id))
    runjs(sprintf("setTimeout(() => open_popup('%s'), 10)", id))
  })
}

shinyApp(ui, server)