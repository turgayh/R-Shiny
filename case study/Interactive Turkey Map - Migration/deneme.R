# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

data <- read_excel("./data/2018-2019_data.xlsx")
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

library(leaflet)
library(shiny)
library(shinydashboard)
# header board

header <- dashboardHeader(
  title = 'Interactive Turkey Map - Migration'
  # task list for status of data processing
  , dropdownMenuOutput('task_menu'))
# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Migration Map', tabName = 'MigrationMap')
    , menuItem('Statistics - 2019', tabName = '2019Statistics')
    , menuItem('Statistics - 2018', tabName = '2018Statistics')
    
    
  )
)
# Body board
body <- dashboardBody(
  tags$head(includeCSS("www/style.css")), 
  
  tabItems(

    tabItem(
      tags$style(type = "text/css", "#map {height: calc(100vh - 30px) !important;}"),
      tags$head(tags$style(HTML(".content { padding-top: 0;padding-right:0;padding-left:0;margin-bottom:0;}"))),
      tags$style(
        HTML(".content-wrapper{margin-bottom:0px;}")
      ),
      
      tabName = 'MigrationMap'
      , leafletOutput('map')
      , verbatimTextOutput('summary')
      ,  absolutePanel(id = "controls",class = "panel",fixed = TRUE,draggable = TRUE,
                       top = "auto",left = "20%",right = "auto",bottom = "8%",width = 820,height = 20,
                       
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
    ),
    
    tabItem(
      tabName = '2019Statistics'
      , 
      valueBoxOutput("BoatsArrived2019",width = 2),
      valueBoxOutput("TotalArrivals2019",width = 2),
      valueBoxOutput("TransfersMainland2019",width = 3),
      valueBoxOutput("TotalPopulation2019",width = 2)
      
    ),
    
    tabItem(
      tabName = '2018Statistics'
      , 
      valueBoxOutput("BoatsArrived2018",width = 2),
      valueBoxOutput("TotalArrivals2018",width = 2),
      valueBoxOutput("TransfersMainland2018",width = 3),
      valueBoxOutput("TotalPopulation2018",width = 2)
      
    )
  )
)
# Shiny UI
ui <- dashboardPage(
  title = 'Interactive Turkey Map - Migration',
  dashboardHeader(),
  sidebar,
  body
)

# create the server functions for the dashboard  
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


shinyApp(ui, server)