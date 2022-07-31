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

server <- function(input, output) {
  # subset species data for tab 2
  tab2CtableData <- reactive({
    v2 <- input$v2
    
    tab2CtableData <- iris[,c(as.numeric(v2),5)]
    
    tab2CtableData
  })
  # contigency table for tab 2
  output$ctable <- renderTable({
    a <- tab2CtableData()
    table(a)
  })
  
    # 
  output$speciestable <- renderTable({
    table(iris$Species)
  })
    # subset species data for tab 4
    SpeciesData <- reactive({
      v <- input$v
      variable <- input$variable
      
      irisSpeciesData <- iris %>% filter(Species == v)
      
      if (variable > 1){irisSpeciesData <- irisSpeciesData[,c(as.numeric(variable)-1,5)]}
      
      irisSpeciesData
      })
    
    # data table for tab 4
    output$table <- renderTable({
      SpeciesData <- SpeciesData()
      SpeciesData
    })
    
    # summary
    output$summary <- renderTable({
      a <- summary(SpeciesData())
      s <- data.frame(a)
      s
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
