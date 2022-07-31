#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Exploration", tabName = "widgets", icon = icon("th")),
      menuItem("Modeling", tabName = "Modeling", icon = icon("th")),
      menuItem("Data", tabName = "Data", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      ),
      # 3rd tab content
      tabItem(tabName = "Modeling",
              h2("Modeling")
      ),
      # 4th tab content
      tabItem(tabName = "Data",
              h2("Data")
      )
    )
  )
)
