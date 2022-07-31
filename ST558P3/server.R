#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define server logic required to draw a histogram
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
    
    SpeciesData <- reactive({
      v <- input$v
      
      irisSpeciesData <- iris %>% filter(Species == v)
      irisSpeciesData
    })
    
    url <- a("Google Homepage", href="https://www.google.com/")
    output$tab <- renderUI({
      paste("URL link:", url)
    })
    
    # output$info <- renderText({
      #get data
      # SpeciesData <- SpeciesData()
    # })
    
    output$table <- renderTable({
      #get data
      SpeciesData <- SpeciesData()
      SpeciesData
    })
    
     output$downloadData <- downloadHandler(
       filename = function() {
         paste('irisSpeciesData', Sys.Date(), '.csv', sep='')
       },
       content = function(SpeciesData) {
         write.csv(data, SpeciesData)
       }
     )
    
  })
}