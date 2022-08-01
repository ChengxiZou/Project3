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
library(caret)
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
  
  # subset species data for hist
  pData <- reactive({
    
    pspecies <- input$hists
    pvariable <- input$histv
    
    pData <- iris %>% filter(Species == pspecies)
    
    pData <- pData[,c(pvariable,"Species")]
    
    pData
  })
  
  # subset species data for scatter plot
  sData <- reactive({
    sspecies <- input$ss
    sData <- iris %>% filter(Species == sspecies)
    if (input$vv == "Sepal.Length : Sepal.Width") {sData <- sData[,c(1,2,5)]}
    if (input$vv == "Sepal.Length : Petal.Length") {sData <- sData[,c(1,3,5)]}
    if (input$vv == "Sepal.Length : Petal.Width") {sData <- sData[,c(1,4,5)]}
    if (input$vv == "Sepal.Width : Petal.Length") {sData <- sData[,c(2,3,5)]}
    if (input$vv == "Sepal.Width : Petal.Width") {sData <- sData[,c(2,4,5)]}
    if (input$vv == "Petal.Length : Petal.Width") {sData <- sData[,c(3,4,5)]}
    sData
  })
  
  # subset species data for box plot
  bData <- reactive({
    bvariable <- input$bv
    bData <- iris[,c(bvariable,"Species")]
    bData
  })
  
  # data table for tab 2 scatter 
  output$table2 <- renderTable({
    Data2 <- sData()
    Data2
  })
  # data table for tab 2 hist
  output$table3 <- renderTable({
    Data2 <- pData()
    Data2
  })
  # data table for tab 2 box plot
  output$tableb <- renderTable({
    Data2 <- bData()
    Data2
  })
  #create plot for tab 2
  output$plot <- renderPlot({
    if (input$pt == "histogram") {
    d <- pData()
    pvariable <- input$histv
    g <- ggplot(d, aes_string(x=pvariable))
      g + geom_bar(position = "dodge") + ggtitle("Histogram Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Sepal.Length : Sepal.Width") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Sepal.Length,y=Sepal.Width))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Sepal.Length : Petal.Length") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Sepal.Length,y=Petal.Length))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Sepal.Length : Petal.Width") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Sepal.Length,y=Petal.Width))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Sepal.Width : Petal.Length") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Sepal.Width,y=Petal.Length))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Sepal.Width : Petal.Width") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Sepal.Width,y=Petal.Width))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "scatter plot" & input$vv == "Petal.Length : Petal.Width") {
      dd <- sData()
      g1 <- ggplot(dd, aes(x=Petal.Length,y=Petal.Width))
      g1 + geom_point() + ggtitle("Scatter Plot")}
    
    else if (input$pt == "box plot") {
      ddd <- bData()
      bvariable <- input$bv
      g2 <- ggplot(ddd, aes_string(x=bvariable))
      g2 + geom_boxplot(aes(color = Species)) + ggtitle("Box Plot")} + coord_flip()
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
    
     # tab 4 model info
     output$minfo <- renderUI({
       if (input$method == "GLM") {text <- "If we want to predict a numeric variable based on the rest three numeric
                                           variables and the species variable, the method would be Generalized Linear
                                           Model. Pros: GLM can allow for responses from non-normal distributions. You
                                           could find from the plotting sectionthat some data distributions are not
                                           perfectly normal distributted. Glm also can have both types of predictors.
                                           Here we have numeric and catagorical predictors so this method is great. Cons:
                                           if we want to predict the species based on the rest four numeric variables, GLM
                                           is probably not a good way because the response should be continouns."}
       
       else if (input$method == "classification tree") {text <- "If we want to predict the species based on the rest 
                                                                 four numeric variables, classification tree is a
                                                                 good method. It splits up predictor space into regions,
                                                                 different predictions for each region. The goal of
                                                                 classification tree is to classify/predict group
                                                                 membership. For a given region, usually use most
                                                                 prevalent class as prediction. Pros: it has a good
                                                                 visualization, a tree plot that is easy to understand
                                                                 and make predictions based on that. Cons:
                                                                 it may require more computational time and lose
                                                                 interpretability."}
       
       else if (input$method == "Random Forest") {text <- "If we care more about prediction rather than interpretation,
                                                          we can average across many fitted trees, to decrease variance
                                                          over an individual tree fit. Often use bootstrapping to get
                                                          multiple samples to fit on. Can still look at variable importance
                                                          though, but lose interpretability. The idea of the random forest
                                                          is to create multiple trees from bootstrap samples and average
                                                          results, but the difference is that it doesn't use all the predictors,
                                                          instead, it uses a random subset of predictors for each bootstrap
                                                          sample/tree fit. Pros: if there exists strong predictors, the
                                                          random forest method can avoid strong correlation of predictors,
                                                          to make those strong predictors not to dominate the tree fits.
                                                          Cons: require more computational time and lose interpretability.
                                                          "}
       
       h4(text)
     })
       
     output$jax <- renderUI({
       if (input$method == "GLM") {withMathJax(
         helpText('GLM model example function
               $$Sepal.Length=\\beta\\_0+\\beta\\_1\\cdot\\
                  Sepal.Width+\\beta\\_2\\cdot\\
                  Petal.Length+\\beta\\_3\\cdot\\
                  Petal.Width+\\beta\\_4\\cdot\\
                  Species$$'))}
     })
       
       
     # Split train and test data for tab 3
     set.seed(1)
     trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
     train <- iris[trainIndex, ]
     test <- iris[-trainIndex, ]
     
  }
