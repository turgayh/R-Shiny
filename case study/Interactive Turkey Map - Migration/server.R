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


  #----------------------------    2019  -------------------------------------------------------------
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
  
  #-------------------------------- 2018 -------------------------------------------------------------
  
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
  
  
  #--------------------------------------------------------------------------------------------------
  #----------------------------    Statistics Graph Implemantion  -----------------------------------
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
  filter2019 <-reactive({
    data[data$Islands == input$Islands2019 & data$Year == "2019",]
  })
  filter2018 <-reactive({
    data[data$Islands == input$Islands2018 & data$Year == "2018",]
  })
  #------------------------------------ 2019 ----------------------------------------------------------
  
    #2019 graph render
  output$GraphBoatsArrived2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Boats Arrived`)) + 
      geom_bar(stat="identity", fill = "#52de97") + ylab("Boats Arrived to Islands") + 
      xlab("Month") +
      theme(axis.title.x = element_text(colour = "black",size = "22"),
            axis.title.y = element_text(colour = "black",size = "22"))
  })
  output$GraphTotalArrivals2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Total Arrivals`)) + 
      geom_bar(stat="identity", fill = "#52de97") + ylab("Total Arrivals to Islands") + 
      xlab("Month")  +
      theme(axis.title.x = element_text(colour = "black",size = "22"),
            axis.title.y = element_text(colour = "black",size = "22"))
  })
  output$GraphTransferMaindland2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Transfers to mainland`)) + 
      geom_bar(stat="identity", fill = "#52de97") + ylab("Transfer to mainland") + 
      xlab("Month")  +
      theme(axis.title.x = element_text(colour = "black",size = "22"),
            axis.title.y = element_text(colour = "black",size = "22"))
  })
  output$GraphTotalPopulation2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Total population`)) + 
      geom_bar(stat="identity", fill = "#52de97") + ylab("Islands - Total Populations") + 
      xlab("Month")  +
      theme(axis.title.x = element_text(colour = "black",size = "22"),
            axis.title.y = element_text(colour = "black",size = "22"))
  })
  
  #------------------------------2018 ---------------------------------------------------------------
  
  #2018 graph render
  output$GraphBoatsArrived2018 <- renderPlot({
    ggplot(data = filter2018(), 
           aes(x=Month, y=`Boats Arrived`)) + 
      geom_line() +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      theme_ipsum() +
      ggtitle("Boats Arrived to Islands")
  }
  )
  output$GraphTotalArrivals2018 <- renderPlot({
    ggplot(data = filter2018(), 
           aes(x=Month, y=`Total Arrivals`)) + 
      geom_line() +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      theme_ipsum() +
      ggtitle("Total Arrivals to Islands")
  }
  )
  output$GraphTransferMaindland2018 <- renderPlot({
    ggplot(data = filter2018(), 
           aes(x=Month, y=`Transfers to mainland`)) + 
      geom_line() +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      theme_ipsum() +
      ggtitle("Transfer to mainland")
  }
  )
  
  output$GraphTotalPopulation2018 <- renderPlot({
    ggplot(data = filter2018(), 
           aes(x=Month, y=`Total population`)) + 
      geom_line() +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      theme_ipsum() +
      ggtitle("Total population Islands")
    }
  )

    
  
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
}
)