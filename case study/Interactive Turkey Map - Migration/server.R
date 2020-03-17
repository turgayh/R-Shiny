# load the required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(htmltools)
library(hrbrthemes)
library(dygraphs)

# create the server functions for the dashboard  
shinyServer(function(input, output, session) {
  filteredData <- reactive({
    data[data$Year == input$Year & data$Month == input$Month,]
  })
  
  filteredStatisticsData <- reactive({
    data[data$Islands == input$statisticsDataIsland,]
  })
  
  
  filteredComparableStatisticsData <- reactive({
    input$CStatisticsDataIsland
  })
  
  #--------------------------------------------------------------------------------------------------
  #----------------------------    MAP  -------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
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
  
  
  
  #--------------------------------------------------------------------------------------------------
  #----------------------------    Makers by filter -------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  labels <- function(islands,boatsArrived,totalArrivals,tranfersMainland,totalPopulation){sprintf(
    "<strong text-align=\"center\">%s</strong>
    <br/> <h5  class=\"section\"> Boats Arrived  :    <span class=\"number\">%s</span></h5>
     <h5  class=\"section\"> Total Arrivals  :    <span class=\"number\">%s</span></h5>
     <h5  class=\"section\"> Transfers to mainland  :    <span class=\"number\">%s</span></h5>
     <h5  class=\"section\"> Total population  :    <span class=\"number\">%s</span></h5>

     ",
    islands,boatsArrived,totalArrivals,tranfersMainland,totalPopulation 

  ) %>% 
    lapply(htmltools::HTML)
  }
  
  observe({
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>% 
      addMarkers(~Lng, ~Lat,
                 
                 label = ~labels(Islands,`Boats Arrived`,`Total Arrivals`,`Transfers to mainland`,`Total population`)
                   , 
                 labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                          padding = "3px 8px"),
                                             textsize = "15px",
                                             direction = "auto"),                 clusterOptions = markerClusterOptions()
      )
  })
  
  
  
  #--------------------------------------------------------------------------------------------------
  #----------------------------    2019 -2018 Statistics  -------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
  yazi <-function(str){
    tags$p(str, style = "font-size: 25;")
  }


  ################ [Start]  Statistics Tab ##############################################################
  
  
  output$BoatArrived <- renderDygraph({
    island <- filteredStatisticsData()
    tseries <- ts(island$`Boats Arrived`, start = c(2018,1), end = c(2020,0), frequency = 12)
    dygraph(tseries, main = "Boats Arrived ") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
      dyHighlight(highlightCircleSize = 5)
  }
  )
  
  output$TotalArrivals <- renderDygraph({
    island <- filteredStatisticsData()
    tseries <- ts(island$`Total Arrivals`, start = c(2018,1), end = c(2020,0), frequency = 12)
    dygraph(tseries, main = "Total Arrivals ") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
      dyHighlight(highlightCircleSize = 5)
  }
  )
  
  output$TransferToMainland <- renderDygraph({
    island <- filteredStatisticsData()
    tseries <- ts(island$`Transfers to mainland`, start = c(2018,1), end = c(2020,0), frequency = 12)
    dygraph(tseries, main = "Transfers to mainland ") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
      dyHighlight(highlightCircleSize = 5)
  }
  )
  
  output$TotalPopulation <- renderDygraph({
    island <- filteredStatisticsData()
    tseries <- ts(island$`Total population`, start = c(2018,1), end = c(2020,0), frequency = 12)
    dygraph(tseries, main = "Total Population ") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
      dyHighlight(highlightCircleSize = 5)
  }
  )
  
  ################ [Start] Comparable Statistics Tab ##############################################################
  output$CTotalPopulation <- renderDygraph({
    island <- filteredComparableStatisticsData()
    x <- list()
    ll <- list()
    index <- 1
    for (i in island){
      ll[[index]] <- data[data$Islands == i,]
      index <- index + 1
    }
    
    index <- 1
    
    for (j in ll){
      x[[index]] <- ts(j$`Total population`, start = c(2018,1), end = c(2020,0), frequency = 12)
      index <- index + 1
    }
    
    
    
    dygraph(x, main = "Deaths from Lung Disease (UK)") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
  }
  )
}
)