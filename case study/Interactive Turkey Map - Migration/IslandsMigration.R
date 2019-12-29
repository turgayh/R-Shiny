library(shiny)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(readxl)
library(ggplot2)
library(shinyWidgets)
library(ggplot2)


data <- read_excel("2018-2019_data.xlsx")
BoatsArrived <- sum(as.numeric(data$`Boats Arrived`), na.rm = TRUE)
TotalArrivals <- sum(as.numeric(data$`Total Arrivals`), na.rm = TRUE)
TransfersMainland <- sum(as.numeric(data$`Transfers to mainland`), na.rm = TRUE)
TotalPopulation <- sum(as.numeric(data$`Total population`), na.rm = TRUE)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("www/style.css")), 
  #Map fullscrenn
  leafletOutput("map", width = "100%", height = "100%"),
 
  #Date Filter Menu
  absolutePanel(
    id = "controls",
    class = "panel",
    fixed = TRUE,
    draggable = TRUE,
    top = "auto",
    left = 190,
    right = 60,
    bottom = 70,
    width = "68%",
    height = 20,
    
    radioGroupButtons(
      inputId = "Month",
      choices = c(
        "January",
        "February",
        "March" ,
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      ) ,
      status = "success"
    ),
    radioGroupButtons(
      inputId = "Year",
      choices = c("2019", "2018") ,
      status = "success"
    ), 
    
  ),
  
  #Panel show graphical 
  absolutePanel(
    id = "controls", class = "panel2", fixed = TRUE,
    draggable = TRUE, top = 0, left = "auto", right = 0, bottom = "auto",
    width = 270, height = "auto",
    tags$div(id = 'overview',  class="panel3",
          
             plotOutput("plot")
    ))
  
)

server <- function(input, output, session) {
  filteredData <- reactive({
    data[data$Year == input$Year & data$Month == input$Month,]
  })
  output$plot <- renderPlot({
    data2 <- data.frame(x=rnorm(100))
    ggplot( data=data2, aes(x=x)) + 
      geom_histogram(fill="skyblue", alpha=0.5) +
      ggtitle("A blue Histogram") +
      theme_minimal()
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
          "<h5  class=\"section\">" ,"Total population :" ,"<span class=\"number\">", `Total population`  ,"</span>", "</h5>" ,
          "<button class=\"btn btn-success\" data-toggle=\"collapse\" data-target=\"#overview\">More Information</button>"
          ) , 
          clusterOptions = markerClusterOptions()
      )
  })
}

shinyApp(ui, server)