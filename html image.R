<div align ="center"; padding-bottom: 25px>
  <img src="../images/reactivity result.PNG" alt="res" 
  class="center" width="1000" height="400"> 
    </div>
    
    style="width:512px;height:512px"
  
 
    
    ,
  ## Using HTML, CSS and Javascript in R Code 
  [ING] <span style="color:purple; font-weight:bold">HTML elementleri</span> tags keyword sayesinde r icinde tanimlanabiliyor. 
  ```R
  ui <- fluidPage (
    tags$h1(”R Shiny Introduction”) ,
    tags$hr () ,
    tags$br () ,
    tags$p(strong(”Istanbul Technical University”)),
    tags$p(em(”Mathematical Engineering”)),
    tags$a(href=”https://www.itu.edu.tr”, ”Website”))
  server <- function(input , output){} > 
    shinyApp(ui = ui , server = server)
  ```
  
  <div id="CodeOutput" name="CodeOutput" style="text-align: center; vertical-align: middle;">
    Code Output
  </div>
    working code:
    
    <div align ="center"; padding-bottom: 25px>
    <img src="../images/html tags.PNG" alt="tags" 
  class="center" width="500" height="150"> 
    </div>
    