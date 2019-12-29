
library(shiny)

ui <-
  fluidPage (
    wellPanel(
      numericInput(inputId = 'n', 'Sample size', value = 25),
      submitButton () ) , plotOutput(outputId = 'hist'))
server <- function(input , output ){
  
} 
  shinyApp(ui = ui , server = server)