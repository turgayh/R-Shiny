
library(rvest)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(ggiraph)
library(shiny)
library(ggiraph)
library(readxl)


# Define the UI
ui = fluidPage(
    
    # App title
    titlePanel("Childlessness and Gender Gap Index Data"),
    
    # Sidebar layout with input and output definitions
    sidebarLayout(
        
        # Sidebar panel for inputs 
        sidebarPanel(
            
            # First input: Type of data
            selectInput(inputId = "data_type",
                        label = "Choose the type of data you want to see:",
                        choices = list("Childlessness" = "Childlessness", "Gender Gap Index" = "Gender Gap Index")),
            
            # Second input (choices depend on the choice for the first input)
            uiOutput("secondSelection"),
            
            # Third input (choices depend on the choice for the first and second input)
            uiOutput("thirdSelection")
            
        ),
        
        # Main panel for displaying outputs
        mainPanel(
            
            # Hide errors
            tags$style(type = "text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"),
            
            # Output: interactive world map
            girafeOutput("distPlot")
            
        )
    )
)

# Define the server
server = function(input, output) {
    
    # Create the interactive world map
    output$distPlot <- renderGirafe({
        ggiraph(code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
    })
    
    # Change the choices for the second selection on the basis of the input to the first selection
    output$secondSelection <- renderUI({
        choice_second <- as.list(unique(df$Period[which(df$DataType == input$data_type)]))
        selectInput(inputId = "period", choices = choice_second,
                    label = "Choose the period for which you want to see the data:")
    })
    
    # Change the choices for the third selection on the basis of the input to the first and second selections
    output$thirdSelection <- renderUI({
        lab <- ifelse(input$data_type == "Childlessness", "age group", "indicator")
        choice_third <- as.list(unique(df$Indicator[df$DataType == input$data_type & df$Period == input$period]))
        selectInput(inputId = "indicator", choices = choice_third,
                    label = paste0("Choose the type of ", lab, " you want to explore:"))
    })
}

# Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
shinyApp(ui = ui, server = server)
