library(shiny)

ui <-  fluidPage(
  radioButtons(inputId = "university", label = "Choose an university",
                choices =  c("Istabul Technical University" = "ITU",
                             "Bogazici University" = "BOUN",
                             "Middle East Technical University" = "ODTU" )),
  imageOutput("universityLogo",height = 600,width = 600),
  
  
)

server <- function(input,output){
  output$universityLogo <- renderImage({
    if (is.null(input$university))
      return(NULL)
    
    if (input$university == "ITU") {
      return(list(
        src = "../../images/university/itu.png",
        contentType = "image/png",
        alt = "ITU"
      ))
    } else if (input$university == "BOUN") {
      return(list(
        src = "../../images/university/boun.png",
        filetype = "image/png",
        alt = "Bogazici"
      ))
    } else if (input$university == "ODTU") {
      return(list(
        src = "../../images/university/odtu.png",
        filetype = "image/pnd",
        alt = "Ortadogu Teknik"
      ))
    }
    
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)