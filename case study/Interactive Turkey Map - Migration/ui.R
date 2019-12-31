library(shiny)
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("www/style.css")), 
  #Map fullscrenn
  leafletOutput("map", width = "100%", height = "100%"),
  
  #Date Filter Menu
  absolutePanel(id = "controls",class = "panel",fixed = TRUE,draggable = TRUE,
                top = "auto",left = 190,right = 60,bottom = 70,width = "68%",height = 20,
                
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
  
  #Panel show graphical [More Information]
  conditionalPanel(condition = "1==1",
                   absolutePanel(
                     id = "controls", class = "panel2", fixed = TRUE,
                     draggable = TRUE, top = 0, left = "auto", right = 0, bottom = "auto",
                     width = 320, height = "auto",
                     
                     
                     radioGroupButtons(inputId = "Islands",choices = c("Kos","Chios","Leros","Samos","Lesvos","Other"),size = "xs",status = "warning"),
                     tabsetPanel(
                       tabPanel("Boats Arrived", plotOutput("BoatsArrived")),
                       tabPanel("Total Arrivals", plotOutput("TotalArrivals")),
                       tabPanel("Transfers to mainland", plotOutput("TransfersMainland")),
                       tabPanel("Total population", plotOutput("TotalPopulation"))
                       
                     ),
                     
                     
                   )),
  
  #Overview Year Data TOP
  
  absolutePanel(
    id = "topData", class = "panel4", fixed = TRUE,
    draggable = FALSE, top = 13, left = "3%", right = "auto", bottom = "auto",
    width = 600, height = "auto",
    fluidRow(
      conditionalPanel(condition = "input.Year == 2019",
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Year</h4>" , "<br><span class = \"yearText\"> 2019 </span>")
                              )),
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Boats Arrived</h4>" , "<span class = \"yearText\">" ,BoatsArrived2019, "</span>")
                              )),
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Total Arrivals</h4>" , "<span class = \"yearText\">" ,TotalArrivals2019, "</span>")
                              )),
                       column(3,
                              HTML(
                                paste("<h4 class = \"yearText\">Transfers to Mainland</h4>" , "<span class = \"yearText\">" ,TransfersMainland2019, "</span>")
                              )),
                       column(3,
                              HTML(
                                paste("<h4 class = \"yearText\">Total Population</h4>" , "<span class = \"yearText\">" ,TotalPopulation2019, "</span>")
                              ))
                       
      ),
      
      conditionalPanel(condition = "input.Year == 2018",
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Year</h4>" , "<br><span class = \"yearText\"> 2018 </span>")
                              )),
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Boats Arrived</h4>" , "<span class = \"yearText\">" ,BoatsArrived2018, "</span>")
                              )),
                       column(2,
                              HTML(
                                paste("<h4 class = \"yearText\">Total Arrivals</h4>" , "<span class = \"yearText\">" ,TotalArrivals2018, "</span>")
                              )),
                       column(3,
                              HTML(
                                paste("<h4 class = \"yearText\">Transfers to Mainland</h4>" , "<span class = \"yearText\">" ,TransfersMainland2018, "</span>")
                              )),
                       column(3,
                              HTML(
                                paste("<h4 class = \"yearText\">Total Population</h4>" , "<span class = \"yearText\">" ,TotalPopulation2018, "</span>")
                              ))
                       
      )
    )
  )
)
)