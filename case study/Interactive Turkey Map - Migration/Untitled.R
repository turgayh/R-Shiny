library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,

                selectInput("city", "Color Scheme",mig$Month),
                
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    #quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    mig[mig$Month == input$city]
  })
  


  
  output$map <- renderLeaflet({
    mig = filteredData()
    leaflet(mig) %>% addTiles() %>%
      addMarkers(mig$Lng, mig$Lat, popup = ~htmlEscape(mig$Islands))
  })
  
}

shinyApp(ui, server)