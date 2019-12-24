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
                
                selectInput("city", "Color Scheme",mig$Month, selected = mig$Month[12]),
                
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
        mig[mig$Month == input$city,]
  })
  
  max_lng = 19.336
  min_lng = 33.223
  max_lat = 42.952
  min_lat = 33.004
  
  
  output$map <- renderLeaflet({
    leaflet(mig) %>% addTiles() %>%
      fitBounds(min_lng, min_lat, max_lng, max_lat)  %>%
      addMarkers(mig$Lng, mig$Lat, popup = ~htmlEscape(mig$Islands))
  })
  

  showIslandMigInfoPopup <- function(data,lat, lng) {
    content <- as.character(tagList(
      tags$h4("Score:", as.character(data$Islands)),

    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showIslandMigInfoPopup(event$mig, event$lat, event$lng)
    })
  })
  
}

shinyApp(ui, server)