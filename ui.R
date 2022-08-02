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
library(randomForest)
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("th")),
      menuItem("Data Exploration", tabName = "widgets", icon = icon("th")),
      menuItem("Modeling Info", tabName = "ModelingInfo", icon = icon("th")),
      menuItem("Model Fitting", tabName = "ModelFitting", icon = icon("th")),
      menuItem("Prediction", tabName = "Prediction", icon = icon("th")),
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
                       
                       h4("The second tab is for data exploration. There exists two sections: numeric summary section
                        and plotting section. For each section you can select the variables, then
                          the app will automatically present the relevant results."),br(),
                       
                       h4("The third tab is for Modeling Info. You should explain these three modeling approaches,
                       the benefits of each, and the drawbacks of each. You should include some type of math type
                          in the explanation"),br(),
                       
                       h4("The fourth tab is for Model Fitting. First, split the data into a training and test set.
                       The app has the ability to choose the proportion of data used in each. It can also choose
                          the variables used in the data set. Once you've chosen the p and variables, hit the botton
                          below then the subset data would be generated for analysis."),br(),
                       
                       h4("The fifth tab is for Modeling Info. You should give the user a way to use one of the
                       models for prediction. That is, they should be able to select the values of the predictors
                       and obtain a prediction for the response."),br(),
                       
                       h4("The final tab is for data browsing. There are two panels that you could select the
                          species and relevant variables of flowers. There is a download functionality as well."),br(),
                       
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
              
              sidebarPanel(
                # Summary Section
                h4("Summary Section"),
                h5("Choose variables for the contigency table and numeric summaries."),
                selectizeInput("stab2", "Species", selected = "all", choices = c(levels(as.factor(iris$Species)),"all")),
                selectizeInput("vtab2", "variable", selected = "Sepal.Length", choices = c("Sepal.Length",
                                                                                     "Sepal.Width","Petal.Length",
                                                                                     "Petal.Width")),
                # Plotting Section
                # histogram
                h4("Plotting Section"),
                h5("Choose variables for plotting."),
                selectizeInput("pt", "plot type", selected = "histogram", choices = c("histogram",
                                                                                      "scatter plot","box plot")),
                conditionalPanel(
                  condition = "input.pt == 'histogram'",
                  selectizeInput("histv", "histogram variable", selected = "Sepal.Length", choices = c("Sepal.Length",
                                                                                        "Sepal.Width","Petal.Length",
                                                                                        "Petal.Width"))),
                conditionalPanel(
                  condition = "input.pt == 'histogram'",
                  selectizeInput("hists", "histogram Species", selected = "setosa", choices = levels(as.factor(iris$Species)))),
                
                conditionalPanel(
                  condition = "input.pt == 'scatter plot'",
                  selectizeInput("ss", "scatter plot Species", selected = "setosa", choices = levels(as.factor(iris$Species)))),
                
                conditionalPanel(
                  condition = "input.pt == 'scatter plot'",
                  selectizeInput("vv", "variables", selected = "Sepal.Length:Sepal.Width", choices = c("Sepal.Length : Sepal.Width",
                                                                                             "Sepal.Length : Petal.Length",
                                                                                             "Sepal.Length : Petal.Width",
                                                                                             "Sepal.Width : Petal.Length",
                                                                                             "Sepal.Width : Petal.Width",
                                                                                             "Petal.Length : Petal.Width"))),
                conditionalPanel(
                  condition = "input.pt == 'box plot'",
                  selectizeInput("bv", "box plot variable", selected = "Sepal.Length", choices = c("Sepal.Length",
                                                                                                       "Sepal.Width","Petal.Length",
                                                                                                       "Petal.Width")))
              ),
                mainPanel(
                box(id = "c",title = "Contingency Table",tableOutput("ctable")),
                box(id = "s",title = "Numeric Summaries",tableOutput("stable")),
                box(id = "p",title = "Plot",plotOutput("plot"))
              )
      ),
      # 3rd tab content: 1
      tabItem(tabName = "ModelingInfo",
              sidebarPanel(selectizeInput("method", "method", selected = "GLM", choices = c("GLM", "bagging",
                                                                                            "Random Forest",
                                                                                            "boosting"))
              ),
              mainPanel(
                h3("Model Information"),
                uiOutput("minfo"),
                uiOutput("jax")
              )
      ),
      # 3rd tab content: 2
      tabItem(tabName = "ModelFitting",
              sidebarPanel(
                h4("Split the data into a training and test set."),
                h4("choose the proportion of training data set"),
                sliderInput(inputId = "ptrain", label = "proportion", min = 0.3, max = 0.9, value = 0.7, step = 0.1),
                selectizeInput("treevariable", "Select variables",
                               selected = "Sepal.Length : Sepal.Width : Petal.Length : Petal.Width",
                               
                               choices = c("Sepal.Length : Sepal.Width : Petal.Length : Petal.Width",
                                           "Sepal.Length : Sepal.Width : Petal.Length",
                                           "Sepal.Length : Sepal.Width : Petal.Width",
                                           "Sepal.Length : Petal.Length : Petal.Width",
                                           "Sepal.Width : Petal.Length : Petal.Width",
                                          "Sepal.Length : Sepal.Width",
                                          "Sepal.Length : Petal.Length",
                                          "Sepal.Length : Petal.Width",
                                          "Sepal.Width : Petal.Length",
                                          "Sepal.Width : Petal.Width",
                                          "Petal.Length : Petal.Width")),
                actionButton(inputId = "click1",label = "confirm p and variables")
              ),
              mainPanel(
                h2("Analysis Result"),
                box(plotOutput("bag1"),title = "Bagging ImpPlot"),
                box(tableOutput("bag2"),title = "importance"),
                box(tableOutput("bag3"),title = "Confusion matrix"),
                box(plotOutput("rffit1"),title = "random forest CV accuracy"),
                box(tableOutput("rffit2"),title = "random forest result"),
                box(tableOutput("rffit3"),title = "random forest best tune"),
                box(plotOutput("boosting1"),title = "boosting CV accuracy"),
                box(tableOutput("boosting2"),title = "boosting result"),
                box(tableOutput("boosting3"),title = "boosting best tune")
              )
      ),
      # 3rd tab content: 3
      tabItem(tabName = "Prediction",
              h2("Predict species using bagged tree method."),
              numericInput(inputId = "sl",label = "Sepal.Length", value = 0,min = 0,max = 10,step = 0.1),
              numericInput(inputId = "sw",label = "Sepal.Width", value = 0,min = 0,max = 10,step = 0.1),
              numericInput(inputId = "pl",label = "Petal.Length", value = 0,min = 0,max = 10,step = 0.1),
              numericInput(inputId = "pw",label = "Petal.Width", value = 0,min = 0,max = 10,step = 0.1),
              box(tableOutput("prediction"),title = "Prediction")
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
                h3("You can download the data subset!"),
                downloadLink('downloadData', 'Download'),
                br(),
                # plotOutput("sleepPlot"),
                tableOutput("table")
              )
              
      )
    )
  )
)

