library(readxl)

data <- read_excel("./data/2018-2019_migdata.xlsx")
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

graph2019 <-function(){
  wellPanel(
    
    radioGroupButtons(inputId = "Islands2019",choices = c("Kos","Chios","Leros","Samos","Lesvos","Other"),size = "s",status = "primary"),
    tabsetPanel(
      tabPanel("Boats Arrived", plotOutput("GraphBoatsArrived2019")),
      tabPanel("Total Arrivals", plotOutput("GraphTotalArrivals2019")),
      tabPanel("Transfer to Mainland", plotOutput("GraphTransferMaindland2019")),
      tabPanel("Total Population", plotOutput("GraphTotalPopulation2019"))
      
    )
  )
}

statictics <- 
  function(){
  fluidRow(
    radioGroupButtons(inputId = "statisticsDataIsland",choices = c("Kos","Chios","Leros","Samos","Lesvos","Other"),size = "s",status = "primary",justified = TRUE),
    
    
    box(
      status = 'success',
      dygraphOutput("BoatArrived",height = "300px" ),
    ),
    
    box(
      status = 'success',
      dygraphOutput("TotalArrivals" ,height = "300px"),
    ),
    
    box(
      status = 'success',
      dygraphOutput("TransferToMainland" ,height = "300px"),
    ),
    
    box(
      status = 'success',
      dygraphOutput("TotalPopulation" ,height = "300px"),
    )
  )
  
}

graph2018 <-function(){
  wellPanel(
    
    
    radioGroupButtons(inputId = "Islands2018",choices = c("Kos","Chios","Leros","Samos","Lesvos","Other"),size = "s",status = "primary"),
    tabsetPanel(
      tabPanel("Boats Arrived", plotOutput("GraphBoatsArrived2018")),
      tabPanel("Total Arrivals", plotOutput("GraphTotalArrivals2018")),
      tabPanel("Transfer to Mainland", plotOutput("GraphTransferMaindland2018")),
      tabPanel("Total Population", plotOutput("GraphTotalPopulation2018"))
    )
  )
}

urlFacebook <- "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fitu-thesis-rshiny.shinyapps.io%2FInteractive-Turkey-Map-Migration%2F"
urlTwitter <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Fitu-thesis-rshiny.shinyapps.io%2FInteractive-Turkey-Map-Migration%2F&text=Turkey%20Interactive%20Map%20-%20Migration%20to%20Greece%20Island%20"
urlGithub <- "https://github.com/turgayh/R-Shiny"