library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points"),
  verbatimTextOutput('summary')
)

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$m <- NULL
  rv$p <- NULL
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points(), group = 'markers')
    rv$m <- m
    return(m)
  })
  
  observe({
    input$recalc
    ## I'm 90% confident these are the arguments you want...
    rv$p <- data.frame(x = rv$m$x$calls[[2]]$args[[1]],
                       y = rv$m$x$calls[[2]]$args[[2]])
  })
  
  output$summary <- renderPrint({
    # print points
    rv$p
  })
}

shinyApp(ui, server) 