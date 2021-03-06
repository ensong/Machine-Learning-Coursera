---
title: "Machine Learning Coursera Project"
author: "Ed Song"
date: "March 5, 2016"
output: html_document
---

This report will cover how my machine learning model built, trained, and tested. We will be looking at the provided data sets and will be predicting for the "classe" variable.

#Packages that need loaded
THe caret and AppliedPredictive Modeling packages are loaded in order us the functions need for machine learning model construction.

```{r}
library(caret)
library(AppliedPredictiveModeling)
```

##Load training and test data
Grabbing the data from the assignments source.
```{r}
URL <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
download.file(url = URL, destfile = '~/training.csv'
               , method = 'auto')

URL <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
download.file(url = URL 
               , destfile = '~/test.csv'
               , method = 'auto')
```


##Data Exploration

Placing the data into the training and test as well as storing the columns names for limiting data in the future.
```{r}
training <- read.csv(file = 'training.csv', header = TRUE, sep = ",", na.strings = c('NA', ''))
test     <- read.csv(file = 'test.csv', header = TRUE, sep = ",", na.strings = c('NA', ''))

colnames_train<-colnames(training)
colnames_test<-colnames(test)
```

##Cleaning traning data

Below we created a function to identify all columns without "NA" present. Then we looped through each column and if the row count of nonNAs was less then the total rows then we will disregard that column. We also dropped the first 8 columns because they didn't add to the model.
```{r}
nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))}

#Build vector of missing data  or NA columns to drop
colcnt <- nonNAs(training)
drops  <- c()
for (cnt in 1:length(colcnt)){
  if(colcnt[cnt] < nrow(training)){
    drops <- c(drops, colnames_train[cnt])
    }
  }

#Drop NA data and the first 7 columns as they are unneccessary for prediciting
training <- training[,!(names(training) %in% drops)]
training <- training[,8:length(colnames(training))]

test <- test[,!(names(test) %in% drops)]
test <- test[,8:length(colnames(test))]

nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

dim(training)
```
Seeing that our columns are all sufficently unique we will proceed to build the model.


##Break up training data to have a smaller subset to build and test models on
```{r}
inTrain <- createDataPartition(y=training$classe, p=.75, list = FALSE)
big_train <- training[inTrain,]
big_test <- training[-inTrain,]
```


##Building the Model
We built two models after setting the seed (for reproducability). The first was regression partitioning and the second was random forest.
```{r}
set.seed(32556)

#recursive partitioning
rpart_modelFit <- train(classe~., data = big_train, method = 'rpart')
rpart_modelFit

#random forest model
rf_modelFit <- train(classe~., data = big_train, method = 'rf'
                  , preProcess = c('center', 'scale')
                  , trControl = trainControl(method = 'cv',
                                             number = 4))
rf_modelFit
```
From these models it looks like the random forest will be significantly more accurate. Below we will test the models to verify that it is.


##Model testing
```{r}
prediction <- predict(rpart_modelFit, newdata = big_test)
print(confusionMatrix(prediction, big_test$classe), digits = 4)

prediction <- predict(rf_modelFit, newdata = big_test)
print(confusionMatrix(prediction, big_test$classe), digits = 4)
```

Thus we can say that our regression partitioning had an accuracy of .4925 while the random forest was .9947 accurate.
