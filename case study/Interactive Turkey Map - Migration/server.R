# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

# create the server functions for the dashboard  
shinyServer(function(input, output, session) {
  filteredData <- reactive({
    data[data$Year == input$Year & data$Month == input$Month,]
  })
  
  filteredDataGraph <- reactive({
    graphData[graphData$Islands == input$Islands & graphData$Year == input$Year]
  })
  
  observeEvent(input$Islands,{
    output$BoatsArrived <- renderPlot({
      ggplot(graphData, aes(x=Month, y=`Boats Arrived`)) +
        theme(axis.text.x = element_text(angle = 75)) +
        geom_bar(stat="identity") + 
        labs(title = "Boats Arrived ")
    })    
  })
  
  # filteredData2 <- reactive({
  #   mig[mig$Year == mig$Year]
  # })
  
  max_lng = 19.336
  min_lng = 33.223
  max_lat = 42.952
  min_lat = 33.004
  
  output$map <- renderLeaflet({
    leaflet(data) %>% addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      fitBounds(min_lng, min_lat, max_lng, max_lat)  %>%
      addLayersControl(position = "bottomright",
                       baseGroups = c("Esri.WorldImagery", "OSM (default)", "Toner"),
                       options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  showIslandMigInfoPopup <- function(isl) {
    content <- as.character(tagList(tags$h4("Score:", as.character(isl[1]))))
  }
  
  
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>% 
      addMarkers(~Lng, ~Lat,
                 
                 popup = ~paste(collapse = NULL,
                                
                                "<head><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"></head>",
                                "<h4  class=\"title\">",Islands, "</h4>" , 
                                "<h5  class=\"section\">" ,"Boats Arrived  :" ,"<span class=\"number\">", `Boats Arrived`  ,"</span>", "</h5>",
                                "<h5  class=\"section\">" ,"Total Arrivals :" ,"<span class=\"number\">", `Total Arrivals`  ,"</span>", "</h5>",
                                "<h5  class=\"section\">" ,"Transfers to mainland :" ,"<span class=\"number\">", `Transfers to mainland`  , "</span>","</h5>",
                                "<h5  class=\"section\">" ,"Total population :" ,"<span class=\"number\">", `Total population`  ,"</span>", "</h5>" 
                                #"<button class=\"btn btn-success\" data-toggle=\"collapse\" data-target=\"#overview\">More Information</button>"
                                
                 ) , 
                 clusterOptions = markerClusterOptions()
      )
  })
  yazi <-function(str){
    tags$p(str, style = "font-size: 25;")
  }
  
  output$BoatsArrived2019 <- renderValueBox({
    valueBox(
      yazi("Boats Arrived"),
      BoatsArrived2019,
      color = "purple"
    )
  })
  output$TotalArrivals2019 <- renderValueBox({
    valueBox(
      yazi("Total Arrivals"),
      TotalArrivals2019,
      color = "purple"
    )
  })  
  
  output$TransfersMainland2019 <- renderValueBox({
    valueBox(
      yazi("Transfers  to Mainland"),
      TransfersMainland2019,
      color = "purple"
    )
  })
  output$TotalPopulation2019 <- renderValueBox({
    valueBox(
      yazi("Total Population"),
      TotalPopulation2019,
      color = "purple"
    )
  })
  output$BoatsArrived2018 <- renderValueBox({
    valueBox(
      yazi("Boats Arrived"),
      BoatsArrived2018,
      color = "green"
    )
  })
  output$TotalArrivals2018 <- renderValueBox({
    valueBox(
      yazi("Total Arrivals"),
      TotalArrivals2018,
      color = "green"
    )
  })
  output$TransfersMainland2018 <- renderValueBox({
    valueBox(
      yazi("Transfers to Mainland"),
      TransfersMainland2018,
      color = "green"
    )
  }) 
  output$TotalPopulation2018 <- renderValueBox({
    valueBox(
      yazi("Total Population"),
      TotalPopulation2018,
      color = "green"
    )
  })
  
}
)