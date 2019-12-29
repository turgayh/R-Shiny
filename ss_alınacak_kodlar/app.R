library(shiny)

ui <- fluidPage (
  
  tags$h1(”R Shiny Introduction”) ,
  tags$hr () ,
  tags$br () ,
  tags$p(strong(”Istanbul Technical University”)),
  tags$p(em(”Mathematical Engineering”)),
  tags$a(href=”https://www.itu.edu.tr”, ”Website”))
server <- function(input , output){} 
  shinyApp(ui = ui , server = server)