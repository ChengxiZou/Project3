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
library(shinyjs)

server <- function(input, output) {
  # subset species data for tab 2
  tab2Data <- reactive({
    tab2species <- input$stab2
    tab2variable <- input$vtab2
    
    if (tab2species != "all"){
      tab2Data <- iris %>% filter(Species == tab2species)
    } else {
      tab2Data <- iris
      }
    
    tab2Data <- tab2Data[,tab2variable]
    
    tab2Data
  })
  
  # contigency table for tab 2
  output$ctable <- renderTable({
    value <- tab2Data()
    table(value)
  })
  
  # numeric summary table for tab 2
  output$stable <- renderPrint({
    value <- tab2Data()
    summary(value)
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
