#### Machine learning Course Project for Coursera ####

##Packages that need loaded
library(caret)
library(AppliedPredictiveModeling)


##Load training and test data
URL <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
download.file(url = URL, destfile = './training.csv'
               , method = 'auto')

URL <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
download.file(url = URL 
               , destfile = './test.csv'
               , method = 'auto')

##Data Exploration

training <- read.csv(file = 'training.csv', header = TRUE, sep = ",", na.strings = c('NA', ''))
test     <- read.csv(file = 'test.csv', header = TRUE, sep = ",", na.strings = c('NA', ''))

colnames_train<-colnames(training)
colnames_test<-colnames(test)

##Cleaning traning data
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

##Break up training data to have a smaller subset to build and test models on
inTrain <- createDataPartition(y=training$classe, p=.75, list = FALSE)
big_train <- training[inTrain,]
big_test <- training[-inTrain,]

##Needs to be broken down further because run time for model is a long time with that sample size

##Building the Model
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

##Model testing
prediction <- predict(rpart_modelFit, newdata = big_test)
print(confusionMatrix(prediction, big_test$classe), digits = 4)

prediction <- predict(rf_modelFit, newdata = big_test)
print(confusionMatrix(prediction, big_test$classe), digits = 4)
