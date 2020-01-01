# load the required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(htmltools)
library(hrbrthemes)


# create the server functions for the dashboard  
shinyServer(function(input, output, session) {
  filteredData <- reactive({
    data[data$Year == input$Year & data$Month == input$Month,]
  })

  
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
  #--------------------------------------------------------------------------------------------------
  
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
  
  #--------------------------------------------------------------------------------------------------
  
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
  
  filter2019 <-reactive({
    data[data$Islands == input$Islands2019 & data$Year == "2019",]
  })
  filter2018 <-reactive({
    data[data$Islands == input$Islands2018 & data$Year == "2018",]
  })
  #--------------------------------------------------------------------------------------------------
  
    #2019 graph render
  output$GraphBoatsArrived2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Boats Arrived`)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Boats Arrived to Islands") + 
      xlab("Month") 
  })
  output$GraphTotalArrivals2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Total Arrivals`)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Total Arrivals to Islands") + 
      xlab("Month") 
  })
  output$GraphTransferMaindland2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Transfers to mainland`)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Transfer to mainland") + 
      xlab("Month") 
  })
  output$GraphTotalPopulation2019 <- renderPlot({
    ggplot(data = filter2019(), 
           aes(x=Month, y=`Total population`)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Islands - Total Populations") + 
      xlab("Month") 
  })
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
  
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
  
  

}
)