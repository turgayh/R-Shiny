library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(readxl)
library(ggplot2)
library(shinyWidgets)
library(ggplot2)


data <- read_excel("2018-2019_data.xlsx")
graphData <- data
## 2019 Data
data2019 <- data[data$Year == "2019",]
BoatsArrived2019 <- sum(as.numeric(data2019$`Boats Arrived`), na.rm = TRUE)
TotalArrivals2019 <- sum(as.numeric(data2019$`Total Arrivals`), na.rm = TRUE)
TransfersMainland2019 <- sum(as.numeric(data2019$`Transfers to mainland`), na.rm = TRUE)
TotalPopulation2019 <- sum(as.numeric(data2019$`Total population`), na.rm = TRUE)

## 2018 Data
data2018 <- data[data$Year == "2018",]
BoatsArrived2018 <- sum(as.numeric(data2018$`Boats Arrived`), na.rm = TRUE)
TotalArrivals2018 <- sum(as.numeric(data2018$`Total Arrivals`), na.rm = TRUE)
TransfersMainland2018 <- sum(as.numeric(data2018$`Transfers to mainland`), na.rm = TRUE)
TotalPopulation2018 <- sum(as.numeric(data2018$`Total population`), na.rm = TRUE)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("www/style.css")), 
  #Map fullscrenn
  leafletOutput("map", width = "100%", height = "100%"),
 
  #Date Filter Menu
  absolutePanel(id = "controls",class = "panel",fixed = TRUE,draggable = TRUE,
                top = "auto",left = 190,right = 60,bottom = 70,width = "68%",height = 20,
    
    radioGroupButtons(
      inputId = "Month",
      choices = c("January","February","March" ,"April","May","June","July","August","September","October","November","December"
      ) ,
      status = "success"
    ),
    radioGroupButtons(
      inputId = "Year",
      choices = c("2019", "2018") ,
      status = "success"
    ), 
    
  ),
  
  #Panel show graphical [More Information]
  conditionalPanel(condition = "1==1",
  absolutePanel(
    id = "controls", class = "panel2", fixed = TRUE,
    draggable = TRUE, top = 0, left = "auto", right = 0, bottom = "auto",
    width = 320, height = "auto",
    
   
      radioGroupButtons(inputId = "Islands",choices = c("Kos","Chios","Leros","Samos","Lesvos","Other"),size = "xs",status = "warning"),
    tabsetPanel(
      tabPanel("Boats Arrived", plotOutput("BoatsArrived")),
      tabPanel("Total Arrivals", plotOutput("TotalArrivals")),
      tabPanel("Transfers to mainland", plotOutput("TransfersMainland")),
      tabPanel("Total population", plotOutput("TotalPopulation"))
      
    ),

    
  )),
  
  #Overview Year Data TOP
  
  absolutePanel(
    id = "topData", class = "panel4", fixed = TRUE,
    draggable = FALSE, top = 13, left = "3%", right = "auto", bottom = "auto",
    width = 600, height = "auto",
      fluidRow(
        conditionalPanel(condition = "input.Year == 2019",
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Year</h4>" , "<br><span class = \"yearText\"> 2019 </span>")
                                )),
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Boats Arrived</h4>" , "<span class = \"yearText\">" ,BoatsArrived2019, "</span>")
                                )),
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Total Arrivals</h4>" , "<span class = \"yearText\">" ,TotalArrivals2019, "</span>")
                                )),
                         column(3,
                                HTML(
                                  paste("<h4 class = \"yearText\">Transfers to Mainland</h4>" , "<span class = \"yearText\">" ,TransfersMainland2019, "</span>")
                                )),
                         column(3,
                                HTML(
                                  paste("<h4 class = \"yearText\">Total Population</h4>" , "<span class = \"yearText\">" ,TotalPopulation2019, "</span>")
                                ))
                         
                         ),
        
        conditionalPanel(condition = "input.Year == 2018",
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Year</h4>" , "<br><span class = \"yearText\"> 2018 </span>")
                                )),
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Boats Arrived</h4>" , "<span class = \"yearText\">" ,BoatsArrived2018, "</span>")
                                )),
                         column(2,
                                HTML(
                                  paste("<h4 class = \"yearText\">Total Arrivals</h4>" , "<span class = \"yearText\">" ,TotalArrivals2018, "</span>")
                                )),
                         column(3,
                                HTML(
                                  paste("<h4 class = \"yearText\">Transfers to Mainland</h4>" , "<span class = \"yearText\">" ,TransfersMainland2018, "</span>")
                                )),
                         column(3,
                                HTML(
                                  paste("<h4 class = \"yearText\">Total Population</h4>" , "<span class = \"yearText\">" ,TotalPopulation2018, "</span>")
                                ))
                         
                         )
      )
    )
  
)

server <- function(input, output, session) {
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
    content <- as.character(tagList(tags$h4("Score:", as.character(isl[1])),))
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
}

shinyApp(ui, server)