library(readxl)

data <- read_excel("./data/2018-2019_migdata.xlsx")
data2 <- read_excel("./data/2018-2019_data.xlsx")
ISLANDS <- c("Kos","Chios","Leros","Samos","Lesvos","Other")

################ [Start]  Statistics Tab ##############################################################

statistics <- 
  function(){
  fluidRow(
    radioGroupButtons(inputId = "statisticsDataIsland",choices = ISLANDS ,size = "s",status = "primary",justified = TRUE),
    
    
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


################ [Start] Comparable Statistics Tab ##############################################################

statisticsComparable <- function(){
  fluidRow(

    
    checkboxGroupButtons(
      inputId = "CStatisticsDataIsland", label = "Make a choice :", 
      choices = ISLANDS, 
      justified = TRUE, status = "primary",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    ),

    
    box(
      status = 'success',
      dygraphOutput("CTotalPopulation" ,height = "300px"),
    )
  )
}


urlFacebook <- "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fitu-thesis-rshiny.shinyapps.io%2FInteractive-Turkey-Map-Migration%2F"
urlTwitter <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Fitu-thesis-rshiny.shinyapps.io%2FInteractive-Turkey-Map-Migration%2F&text=Turkey%20Interactive%20Map%20-%20Migration%20to%20Greece%20Island%20"
urlGithub <- "https://github.com/turgayh/R-Shiny"