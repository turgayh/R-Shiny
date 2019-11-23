library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
## - kendi notlarim  [silinecek]
#[SOURCE] - https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

data <- read.csv("./data/data.csv")
country <- data["place"]

data$depth_type <-      ## 
    ifelse(
        data$depth <= 70,
        "shallow",
        ifelse(
            data$depth <= 300 | data$depth > 70,
            "intermediate",
            ifelse(data$depth > 300, "deep", "other")
        )
    )


ui <- fluidPage(mainPanel(
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"),
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(
        top = 250,
        left = 30,
        checkboxInput("markers", "Depth", FALSE),
        checkboxInput("heat", "Heatmap", FALSE)
    )
))



server <- function(input, output, session) {
    #define the color pallate for the magnitidue of the earthquake
    pal <- colorNumeric(
        palette = c(
            'gold',
            'orange',
            'dark orange',
            'orange red',
            'red',
            'dark red'
        ),
        domain = data$mag
    )
    
    #define the color of for the depth of the earquakes
    pal2 <- colorFactor(palette = c('blue', 'yellow', 'red'),
                        domain = data$depth_type)
    
    #create the map
    output$mymap <- renderLeaflet({
        leaflet(data) %>%
            addTiles() %>%
            addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE) %>%
            setView(lng = 35,
                    lat = 42,
                    zoom = 5)  %>% #setting the view over
            addTiles() %>%
            addCircles(
                data = data,
                lat = ~ latitude,
                lng = ~ longitude,
                weight = 0.5,
                radius = ~ sqrt(mag) * 15000,
                popup = ~ as.character(mag),
                label = ~ as.character(paste0("Magnitude: ", sep = " ", mag)),
                color = ~ pal(mag),
                fillOpacity = 0.5
            )
    })
    
    #next we use the observe function to make the checkboxes dynamic.
    #If you leave this part out you will see that the checkboxes,
    #when clicked on the first time, display our filters...
    #But if you then uncheck them they stay on.
    #So we need to tell the server to update the map when the checkboxes are unchecked.
    observe({
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        if (input$markers) {
            proxy %>% addCircleMarkers(
                stroke = FALSE,
                color = ~ pal2(depth_type),
                fillOpacity = 0.2,
                label = ~ as.character(paste0("Magnitude: ", sep = " ", mag))
            ) %>%
                addLegend(
                    "bottomright",
                    pal = pal2,
                    values = data$depth_type,
                    title = "Depth Type",
                    opacity = 1
                )
        }
        else {
            proxy %>% clearMarkers() %>% clearControls()
        }
    })
    
    observe({
        proxy <- leafletProxy("mymap", data = data)
        proxy %>% clearMarkers()
        if (input$heat) {
            proxy %>%  addHeatmap(
                lng =  ~ longitude,
                lat =  ~ latitude,
                intensity = ~ mag,
                blur =  10,
                max = 0.05,
                radius = 15
            )
        }
        else{
            proxy %>% clearHeatmap()
        }
        
        
    })
    
}

shinyApp(ui = ui, server = server)