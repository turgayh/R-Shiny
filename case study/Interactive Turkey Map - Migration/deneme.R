library(shiny)


ui <- fluidPage(
  radioGroupButtons(inputId = "somevalue", choices = c("A", "B", "C")), verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$somevalue }) }
shinyApp(ui, server)


