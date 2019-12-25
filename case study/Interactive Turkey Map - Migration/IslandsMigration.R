library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(readxl)

mig <- read_excel("islands_data_october-november.xlsx")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                
                selectInput("month", "Color Scheme",mig$Month, selected = mig$Month[12]),
                
                checkboxInput("legend", "Show legend", TRUE)
  ),
  absolutePanel(top = 600, right = 200,
                
                fluidRow(
                  column(6, html_print("<h4>sssssss</h4>"))),
                fluidRow(
                  column(2, (
"sdasd"                  )))
                
                )
  
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
        mig[mig$Month == input$month,]
    
  })
  
  
  max_lng = 19.336
  min_lng = 33.223
  max_lat = 42.952
  min_lat = 33.004
  
  
  output$map <- renderLeaflet({
    leaflet(mig) %>% addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(min_lng, min_lat, max_lng, max_lat)  %>%
      addLayersControl(
        baseGroups = c("Esri.WorldImagery", "OSM (default)", "Toner"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  

  showIslandMigInfoPopup <- function(isl) {
    content <- as.character(tagList(
      tags$h4("Score:", as.character(isl[1])),
    ))
  }
  

    
  # When map is clicked, show a popup with city info
  observe({
      x <- filteredData()
      leafletProxy("map",data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(~Lng, ~Lat, popup = showIslandMigInfoPopup(filteredData()) , clusterOptions = markerClusterOptions())
  })
  
  # observeEvent(input$city , { 
  #       leafletProxy("map",data = filteredData()) %>%
  #     clearMarkerClusters() %>%
  #       addMarkers(~Lng, ~Lat, popup = ~htmlEscape(Islands) , clusterOptions = markerClusterOptions())
  #   })
  
}

shinyApp(ui, server)