# Project3
ST558 Project 3

# Brief description of the app and its purpose.

The iris dataset is a built-in dataset in R. This app is specially designed for new R learners to get a better feeling of the iris data set that is often used when subsetting data, creating plots or doing analysis using R.

This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.

A sepal is a part of the flower of angiosperms (flowering plants). Usually green, sepals typically function as protection for the flower in bud, and often as support for the petals when in bloom.

Petals are modified leaves that surround the reproductive parts of flowers. They are often brightly colored or unusually shaped to attract pollinators.

# A list of packages needed to run the app.

library(shiny)

library(ggplot2)

library(dplyr)

library(shinydashboard)

library(caret)

library(randomForest)

library(gbm)

# A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app).

install.packages(c("shiny", "ggplot2", "dplyr","shinydashboard","caret","randomForest","gbm"))

# The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.

shiny::runGitHub("Project3", "ChengxiZou")
