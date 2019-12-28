library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(readxl)
library(ggplot2)
library(shinyWidgets)


data <- read_excel("2018-2019_data.xlsx")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  tags$head(
    # Include our custom CSS
    includeCSS("www/style.css")
  ),
  
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel", fixed = TRUE,
                              draggable = TRUE, top = "auto", left = 190, right = 60, bottom = 70,
                              width = 870, height = 20,
                
              #  selectInput("month", "Month",data$Month, selected = "November"),
              # selectInput("year", "Year",data$Year, selected = "2019"),
              radioGroupButtons(inputId = "Month", choices = c("January", "February","March" ,"April","May","June","July","August","September","October","November","December") , status = "success"),
              radioGroupButtons(inputId = "Year", choices = c("2019", "2018") , status = "success"),
            
  )
)

server <- function(input, output, session) {
  
  # filteredData <- reactive({
  #       data[data$Month == input$month & data$Year == input$year,] 
  # })
  filteredData <- reactive({
    data[data$Year == input$Year & data$Month == input$Month,]
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
  

    
  observe({
      leafletProxy("map",data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(~Lng, ~Lat,
                 
                 popup = ~paste(collapse = NULL,
  
          "<head><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"></head>",
          "<h4  class=\"title\">",Islands, "</h4>" , 
          "<h5  class=\"section\">" ,"Boats Arrived  :" ,"<span class=\"number\">", `Boats Arrived`  ,"</span>", "</h5>",
          "<h5  class=\"section\">" ,"Total Arrivals :" ,"<span class=\"number\">", `Total Arrivals`  ,"</span>", "</h5>",
          "<h5  class=\"section\">" ,"Transfers to mainland :" ,"<span class=\"number\">", `Transfers to mainland`  , "</span>","</h5>",
          "<h5  class=\"section\">" ,"Total population :" ,"<span class=\"number\">", `Total population`  ,"</span>", "</h5>"                  ) , 

                  clusterOptions = markerClusterOptions()
      )
  })
  


  # observeEvent(input$city , { 
  #       leafletProxy("map",data = filteredData()) %>%
  #     clearMarkerClusters() %>%
  #       addMarkers(~Lng, ~Lat, popup = ~htmlEscape(Islands) , clusterOptions = markerClusterOptions())
  #   })
  
}

shinyApp(ui, server)