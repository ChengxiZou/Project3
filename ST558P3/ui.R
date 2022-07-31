#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("th")),
      menuItem("Data Exploration", tabName = "widgets", icon = icon("th")),
      menuItem("Modeling", tabName = "Modeling", icon = icon("th")),
      menuItem("Data", tabName = "Data", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(h1("The purpose of the app"),br(),
                       h4("The iris dataset is a built-in dataset in R. This app is specially designed for new R learners to get a better feeling of the iris data set
                       that is often used when subsetting data, creating plots or doing analysis using R."),br(),
                       
                       h1("The data and its source - and a link to more information about it"),br(),
                       
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
                       
                       br(),h2(a(href="https://www.statology.org/iris-dataset-r/", "For more infomation please
                              go to this Complete Guide!")),br(),
                       
                       h1("The purpose of each tab (page) of the app"),br(),
                       
                       h4("This app has four tabs. The first is this About tab now you are in -- it describes the
                          purpose of this app and some relevant knowledge about iris and flowers. I also added some
                          good pictures!"),br(),
                       
                       h4("The second tab is for data exploration."),br(),
                       
                       h4("The third tab is for data modeling."),br(),
                       
                       h4("The fourth tab is for data browsing."),br(),
                       
                       h1("Related pictures"),br(),
                       
                       h2("setosa:"),br(),
                       
                       tags$img(src='setosa.png'),br(),
                       
                       h2("versicolor:"),br(),
                       
                       tags$img(src='versicolor.png'),br(),
                       
                       h2("virginica:"),br(),
                       
                       tags$img(src='virginica.png'),br(),
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
                selectizeInput("variable", "variable", selected = "all", choices = c("all"=1,"Sepal.Length"=2,
                                                                                     "Sepal.Width"=3,"Petal.Length"=4,
                                                                                     "Petal.Width"=5))
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
