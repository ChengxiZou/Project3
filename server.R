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
library(randomForest)
library(gbm)
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
       
       else if (input$method == "bagging") {text <- "If we want to predict the species based on the rest 
                                                                 four numeric variables, classification tree is a
                                                                 good method. It splits up predictor space into regions,
                                                                 different predictions for each region. The goal of
                                                                 classification tree is to classify/predict group
                                                                 membership. For a given region, usually use most
                                                                 prevalent class as prediction. Pros: it has a good
                                                                 visualization, a tree plot that is easy to understand
                                                                 and make predictions based on that. Predictorsdon't need
                                                                 to be scaled. No statistical assumptions needed. Cons:
                                                                 Small changes in data may strongly change the tree, need
                                                                 to prune. it may require more computational time and lose
                                                                 interpretability, but gain in prediction.
                                                                 Bagging is to resample from the data (in our case non-parametric),
                                                                 treat sample as population, resample with relpacement, method or
                                                                 estimation applied to each resample, which is quite useful
                                                                 when theoretical results are difficult to derive.
                                                                 "}
       
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
       
       else if (input$method == "boosting") {text <- "Boosting is a way that slowly trains the tree so that the tree do not
                                                      over fit. Boosting let the tree grow sequencially.
                                                      each subsequent tree is grown on a modified version of original data.
                                                      The predictions would be updated with tree growing. Pros: the tree
                                                      won't overfit. Cons: may cause a lot more computational time.
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
                  Species$$'),
         helpText('We are not using GLM here because this app focus on predicting species! '))}
       
       else if (input$method == "bagging") {withMathJax(
         helpText('Prediction error (MSE usually used)
               $$MSE = \\frac{1}{n_(testObs)}
                  \\sum_{i=1}^{n_(testObs)}(y_i-
                  \\hat(y_i))^2$$'),
         helpText('MSE can be broken down into squared bias plus variance.'),
         helpText('Each tree here has low bias but high variance.'),
         helpText('Averaging trees decreases variance.'))}
       
       else if (input$method == "Random Forest") {withMathJax(
         helpText('Classicifation: usually use
               $$m = \\sqrt
                  p$$'),
         helpText('Regression: usually use
               $$m = p/3$$'),
         helpText('If m=p then you have bagging.'),
         helpText('can determine m through OOB error.'))}
       
       else if (input$method == "boosting") {withMathJax(
         helpText('The process of boosting tree:'),
         helpText('1. Initialized prediction as 0.'),
         helpText('2. Find residuals(observed-predicted).'),
         helpText('3. Fit a tree with d splits(d + 1 terminal nodes) treating the residuals as response.'),
         helpText('4. Update predictions.'),
         helpText('5. Update residuals for new predictions and repeat B times.')
         )}
     })
       
       
     # Split train and test data for tab 3
     train <- reactive({
       set.seed(1)
       trainIndex <- createDataPartition(iris$Species, p = data1(), list = FALSE)
       train <- iris[trainIndex, ]
       train
     })
     
     test <- reactive({
       set.seed(1)
       trainIndex <- createDataPartition(iris$Species, p = data1(), list = FALSE)
       train <- iris[trainIndex, ]
       test <- iris[-trainIndex, ]
       test
     })
     
     # subset train and test data for tab 3
     trainData <- reactive({
       trainData <- train()
       "Sepal.Width : Petal.Length : Petal.Width"
       if (data2() == "Sepal.Length : Sepal.Width") {
         trainData <-trainData[,c(1,2,5)]
         }
       
       else if (data2() == "Sepal.Width : Petal.Length : Petal.Width") {
         trainData <-trainData[,c(2,3,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Petal.Length : Petal.Width") {
         trainData <-trainData[,c(1,3,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Width") {
         trainData <-trainData[,c(1,2,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Length") {
         trainData <-trainData[,c(1,2,3,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Length : Petal.Width") {
         trainData <-trainData
       }
       
       else if (data2() == "Sepal.Length : Petal.Length") {
         trainData <-trainData[,c(1,3,5)]
         }
       
       else if (data2() == "Sepal.Length : Petal.Width") {
         trainData <-trainData[,c(1,4,5)]
         }
       
       else if (data2() == "Sepal.Width : Petal.Length") {
         trainData <-trainData[,c(2,3,5)]
         }
       
       else if (data2() == "Sepal.Width : Petal.Width") {
         trainData <-trainData[,c(2,4,5)]
         }
       
       else if (data2() == "Petal.Length : Petal.Width") {
         trainData <-trainData[,c(3,4,5)]
       }
       
       trainData
     })
     
     testData <- reactive({
       testData <- test()
       "Sepal.Width : Petal.Length : Petal.Width"
       if (data2() == "Sepal.Length : Sepal.Width") {
         testData <-testData[,c(1,2,5)]
       }
       
       else if (data2() == "Sepal.Width : Petal.Length : Petal.Width") {
         testData <-testData[,c(2,3,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Petal.Length : Petal.Width") {
         testData <-testData[,c(1,3,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Width") {
         testData <-testData[,c(1,2,4,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Length") {
         testData <-testData[,c(1,2,3,5)]
       }
       
       else if (data2() == "Sepal.Length : Sepal.Width : Petal.Length : Petal.Width") {
         testData <-testData
       }
       
       else if (data2() == "Sepal.Length : Petal.Length") {
         testData <-testData[,c(1,3,5)]
       }
       
       else if (data2() == "Sepal.Length : Petal.Width") {
         testData <-testData[,c(1,4,5)]
       }
       
       else if (data2() == "Sepal.Width : Petal.Length") {
         testData <-testData[,c(2,3,5)]
       }
       
       else if (data2() == "Sepal.Width : Petal.Width") {
         testData <-testData[,c(2,4,5)]
       }
       
       else if (data2() == "Petal.Length : Petal.Width") {
         testData <-testData[,c(3,4,5)]
       }
       
       testData
     })
     
     # click1
    data1 <- eventReactive(input$click1, {input$ptrain})
    data2 <- eventReactive(input$click1, {input$treevariable})
    data3 <- eventReactive(input$click1, {trainData()})
    data4 <- eventReactive(input$click1, {testData()})
     # see train data
     output$tablett <- renderTable({
       t <- trainData()
       t
     })
     
     # fit models.
     # bagging
     bagging <- reactive({
       bagfit <- randomForest(Species ~ .,
                              data = data3(),
                              mty=ncol(iris)-1,
                              ntree=200,importance=TRUE)
       bagfit
     })
     
     # bagging predict
     baggingp <- reactive({
       bagfit <- bagging()
       baggingp <- predict(bagfit, newdata = data4())
       baggingp
     })
     
     # bagging postResample
     baggingpostResample <- reactive({
       test <- data4()
       baggingpostResample <- postResample(baggingp(), test$Species)
       baggingpostResample
     })
     
     # bagging output
     output$bag1 <- renderPlot({
       bagfit <- bagging()
       varImpPlot(bagfit)
     })
     output$bag2 <- renderTable({
       bagfit <- bagging()
       dd <- bagfit$importance
       data.frame("variable"=rownames(dd),dd)
     })
     output$bag3 <- renderTable({
       bagfit <- bagging()
       dd <- bagfit$confusion
       data.frame("species"=rownames(dd),dd)
     })
     
     # rf
     rffit <- reactive({
       rffit <- randomForest(Species ~ .,data = data3())
       #rffit <- train(Species ~ .,
       #               data = data3(),
       #               method = "rf", 
       #               trControl = trainControl(method = "cv", number = 5), 
       #               preProcess = c("center", "scale"),
       #               tuneGrid = data.frame(mtry = 1:6))
       rffit
     })
     
     # rf predict
     rfp <- reactive({
       rffit <- rffit()
       rfp <- predict(rffit, newdata = data4())
       rfp
     })
     
     # rf postResample
     rfpostResample <- reactive({
       test <- data4()
       rfpostResample <- postResample(rfp(), test$Species)
       rfpostResample
     })
     
     # rf output
     output$rffit1 <- renderPlot({
       rffit <- rffit()
       plot(rffit)
     })
     output$rffit2 <- renderTable({
       rffit <- rffit()
       dd <- rffit$importance
       data.frame("variable"=rownames(dd),"Gini"=dd)
     })
     output$rffit3 <- renderTable({
       rffit <- rffit()
       dd <- rffit$confusion
       data.frame("species"=rownames(dd),dd)
     })
     
     
     # boosting
     boosting <- reactive({
       boosting <- gbm(Species ~.,
                       data = data3(),
                       distribution = "multinomial",
                       cv.folds = 5,
                       shrinkage = .01,
                       n.minobsinnode = 10,
                       n.trees = 100)
       #boosting <- train(Species ~ .,
       #                     data = data3(),
       #                     method = "gbm", 
       #                     trControl = trainControl(method = "cv", number = 5), 
       #                     preProcess = c("center", "scale"),
       #                     tuneGrid = data.frame(expand.grid(n.trees = c(25,50,100,150,200), 
       #                                                       interaction.depth = 1:4,
       #                                                       shrinkage = 0.1,
       #                                                       n.minobsinnode = 10)),
       #                     verbose = FALSE
       #                  )
       a <- summary(boosting)
       boosting
     })
     
     # boosting predict
     boostingp <- reactive({
       boosting <- boosting()
       boostingp <- predict(boosting, newdata = data4())
       boostingp
     })
     
     # boosting postResample
     boostingpostResample <- reactive({
       test <- data4()
       boostingp <- boostingp()
       k <- data.frame(boostingp)
       for (i in 1:nrow(k)) {
         if (max(k[i,1],k[i,2],k[i,3])==k[i,1]){
           k[i,1] = 1
           k[i,2]=0
           k[i,3]=0
         } else if (max(k[i,1],k[i,2],k[i,3])==k[i,2]){
           k[i,2] = 1
           k[i,1]=0
           k[i,3]=0
         }
         else if (max(k[i,1],k[i,2],k[i,3])==k[i,3]){
           k[i,3] = 1
           k[i,1]=0
           k[i,2]=0
         }
       }
       
       for (i in 1:nrow(k)) {
         if (k[i,1] >0){
           k[i,1] = 1
         } 
         if (k[i,2] >0){
           k[i,2] = 1
         }
         if (k[i,3] >0){
           k[i,3] = 1
         }
       }
       
       boostingpostResample <- postResample(k, test$Species)
       boostingpostResample
       
     })
     
     # boosting output
     output$boosting1 <- renderPlot({
       boosting <- boosting()
       plot(boosting)
     })
     output$boosting2 <- renderTable({
       boosting <- boosting()
       data.frame(summary(boosting))
     })
     output$boosting3 <- renderPlot({
       boosting <- boosting()
       summary(boosting)
     })
     
     # predict. bagged tree.
     bprediction <- reactive({
       bagfit1 <- randomForest(Species ~ .,
                              data = iris,
                              mty=ncol(iris)-1,
                              ntree=200,importance=TRUE)
       species <- predict(bagfit1, newdata = data.frame(Sepal.Length=input$sl,
                                            Sepal.Width=input$sw,
                                            Petal.Length=input$pl,
                                            Petal.Width=input$pw))
       species
     })
     
     # predict. rf
     rfprediction <- reactive({
       rffit <- randomForest(Species ~ .,data = iris)
       species <- predict(rffit, newdata = data.frame(Sepal.Length=input$sl,
                                                        Sepal.Width=input$sw,
                                                        Petal.Length=input$pl,
                                                        Petal.Width=input$pw))
       species
     })
     
     # predict. boosting
     bsprediction <- reactive({
       boosting <- gbm(Species ~.,
                      data = iris,
                      distribution = "multinomial",
                      cv.folds = 5,
                      shrinkage = .01,
                      n.minobsinnode = 10,
                      n.trees = 100)
       
       species <- predict(boosting, newdata = data.frame(Sepal.Length=input$sl,
                                                      Sepal.Width=input$sw,
                                                      Petal.Length=input$pl,
                                                      Petal.Width=input$pw))
       species
     })
     
     output$prediction <- renderTable({
       if (input$pmethod == "bagging"){species <- bprediction()}
       if (input$pmethod == "Random Forest"){species <- rfprediction()}
       if (input$pmethod == "boosting"){species <- bsprediction()}
       data.frame(species)
     })
     
     # postresample output
     # baggingpostResample
     # rfpostResample
     # boostingpostResample
     output$post1 <- renderTable({
       baggingpostResample <- baggingpostResample()
       data.frame(baggingpostResample,"Accuracy/Kappa" = c("Accuracy","Kappa"))
     })
     
     output$post2 <- renderTable({
       rfpostResample <- rfpostResample()
       data.frame(rfpostResample,"Accuracy/Kappa" = c("Accuracy","Kappa"))
     })
     
     output$post3 <- renderTable({
       boostingpostResample <- boostingpostResample()
       data.frame(boostingpostResample,"Accuracy/Kappa" = c("Accuracy","Kappa"))
     })
  }
