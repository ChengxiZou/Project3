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
              fluidRow(h3("∗ Describe the purpose of the app"),br(),
                       h4("This app is specially designed for new R learners to get a better feeling of the iris data set
                       that is often used when subsetting data, creating plots or doing analysis on with R."),br(),
                       h3("∗ Briefly discuss the data and its source - providing a link to more information about the data"),br(),
                       h4("This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters
                       of the variables sepal length and width and petal length and width, respectively,
                       for 50 flowers from each of 3 species of iris. The species are Iris setosa,
                       versicolor, and virginica."),br(),
                       h4("A sepal is a part of the flower of angiosperms (flowering plants). Usually green,
                       sepals typically function as protection for the flower in bud, and often as support for
                       the petals when in bloom."),br(),
                       h4("Petals are modified leaves that surround the reproductive parts of flowers.
                          They are often brightly colored or unusually shaped to attract pollinators."),br(),
                       tags$img(src='p1.png'),br(),
                       br(),a(href="https://www.statology.org/iris-dataset-r/", "For more infomation please go to this Complete Guide!"),br(),
                       h3("∗ Tell the user the purpose of each tab (page) of the app"),br(),
                       h4("3"),br(),
                       h3("∗ Include a picture related to the data (for instance, if the data was about the world wildlife fund, you might include a picture of their logo)"),br()
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      # 3rd tab content
      tabItem(tabName = "Modeling",
              h2("Modeling")
      ),
      # 4th tab content
      tabItem(tabName = "Data",
              h2("Data"),
              
              sidebarPanel(
                selectizeInput("v", "Species", selected = "setosa", choices = levels(as.factor(iris$Species))),
                br()
              ),
              
              mainPanel(
                downloadLink('downloadData', 'Download'),
                # plotOutput("sleepPlot"),
                tableOutput("table")
              )
              
      )
    )
  )
)
