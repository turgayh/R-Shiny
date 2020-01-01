library(shiny)
library(leaflet)
require(shinydashboard)
library(shinyWidgets)
url <- "https://twitter.com/intent/tweet?url=https%3A%2F%2Fitu-thesis-rshiny.shinyapps.io%2FInteractive-Turkey-Map-Migration%2F&text=Turkey%20Interactive%20Map%20-%20Migration%20to%20Greece%20Island%20"
# header board
header <- dashboardHeader(
  title = 'Interactive Turkey Map - Migration'
  # task list for status of data processing
  , dropdownMenuOutput('task_menu')
  )
# Side bar boardy
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Migration Map', tabName = 'MigrationMap')
    , menuItem('Statistics - 2019', tabName = '2019Statistics')
    , menuItem('Statistics - 2018', tabName = '2018Statistics')
    , menuItem('Introduction to RShiny', tabName = 'slider')
  ) # Combine text with url variable
  
  ,
  actionButton("twitter_share",
               label = "Share",
               icon = icon("twitter"),
               onclick = sprintf("window.open('%s')", url))
  
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
      valueBoxOutput("TotalPopulation2019",width = 2),
      graph2019()
      
    ),
    
    tabItem(
      tabName = '2018Statistics'
      , 
      valueBoxOutput("BoatsArrived2018",width = 2),
      valueBoxOutput("TotalArrivals2018",width = 2),
      valueBoxOutput("TransfersMainland2018",width = 3),
      valueBoxOutput("TotalPopulation2018",width = 2),
      graph2018()
      
    ),
    tabItem(
      tabName = 'slider'
      ,
      includeHTML("./www/slider.html")
    )
  )
)

shinyUI(
  dashboardPage(
    title = 'Interactive Turkey Map - Migration',
    dashboardHeader(),
    sidebar,
    body
  )
)
