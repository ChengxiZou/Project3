#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
# Define server logic required to draw a histogram
server <- function(input, output) {
    SpeciesData <- reactive({
      v <- input$v
      variable <- input$variable
      
      irisSpeciesData <- iris %>% filter(Species == v)
      
      if (variable > 1){irisSpeciesData <- irisSpeciesData[,c(as.numeric(variable)-1,5)]}
      
      irisSpeciesData
      })
    
    # table
    output$table <- renderTable({
      SpeciesData <- SpeciesData()
      SpeciesData
    })
    
     output$downloadData <- downloadHandler(
       filename = function() {
         paste(input$v, Sys.Date(), '.csv', sep='')
       },
       content = function(file) {
         write.csv(SpeciesData(),file)
       }
     )
    
  }
