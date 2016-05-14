## Practical Machine Learning Course Project
getwd()
setwd("C:/Users/Calli6/Desktop")

pml.training <- read.csv("C:/Users/Calli6/Desktop/pml-training.csv", stringsAsFactors=FALSE)
pml.testing <- read.csv("C:/Users/Calli6/Desktop/pml-testing.csv", stringsAsFactors = FALSE)

library(caret)
#The goal of your project is to predict the manner in which they did the exercise. 
#This is the "classe" variable in the training set. 
#You may use any of the other variables to predict with. 
#You should create a report describing how you built your model, 
# how you used cross validation, 
# what you think the expected out of sample error is,  ## error rate you get on the testing data set
# and why you made the choices you did. 
#You will also use your prediction model to predict 20 different test cases. 

## in sample error- error you get with the training dataset
## out of sample errror- always greater than insample error

## Define error rate
## Split data into training/testing
## On the training set pick features- use cross validation
## On the training set pick prediction function - use cross validation
## Apply 1 time to testing set
##

## Cross Validation
# use training set
# split into training/test sets 
# build model on the training set
# evaluate on test set
# repeat and average the estimated errors
# used for picking variables for the model 
## and picking model algorithm 

# Caret package functions
# preProcess - probably won't use
# Data splitting
  # createDataPartition
  # createResample
  # createTimeSlices - won't use
# train
# predict
#confusionMatrix

# Types of algorithms
# linear discriminant analysis
# regression
# naive Bayes
# support vector machines
# classification and regression trees
# random forests
# boosting

createDataPartition(y = concrete$CompressiveStrength, p = .75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type~., data = training, method = "glm")
modelFit
modelFit$finalModel # what are fitted values
predictions = predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$type)  # to get accuracy functions

# Data slicing
# for k-folds
folds <- createFolds(y=spam$type, k = 10
                     ,list = TRUE
                     ,returnTrain = TRUE)
sapply(folds, length)
# for resampling
folds <- createResample(y=spam$type, times =  10, list =TRUE)
sapply(folds, length)

# RMSE
# RSquared

#Categorical outcomes
# accuracy - number you get correct
# kappa - a measure of concordance
args(trainControl)
method = cv


## remove variables that are completely null
emptycols <- sapply(pml.testing, function (k) all(is.na(k)))
pml.training_noNAcol <- pml.training[!emptycols]

train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)

fitModel<- train(classe~., data=pml.training_noNAcol, trControl=train_control, method="rf")
fitModel$finalModel

fitModel<- train(classe~., data=pml.training_noNAcol, method="rpart")
fitModel$finalModel

modFit_gbm <- train(classe~., method = "gbm", data = pml.training_noNAcol, trControl = train_control, verbose = FALSE)

modelFit
modelFit$finalModel # what are fitted values
predictions = predict(fitModel, newdata = pml.training)
confusionMatrix(predictions, pml.training_noNA$classe)  # to get accuracy functions

pml.testing$classe
pml.training$classe


## Practical Machine Learning Course Project
getwd()
setwd("C:/Users/Calli6/Desktop")

pml.training <- read.csv("C:/Users/Calli6/Desktop/pml-training.csv", stringsAsFactors=FALSE)
pml.testing <- read.csv("C:/Users/Calli6/Desktop/pml-testing.csv", stringsAsFactors = FALSE)

library(caret)
emptycols <- sapply(pml.testing, function (k) all(is.na(k)))
pml.training_noNAcol <- pml.training[!emptycols]
pml.training_final <- pml.training_noNAcol[,c(8:60)]

principalcomps <- prcomp(pml.training_final)
principalrot <- principalcomps$rotation


## General Linear model with principal components analysis
modelFit <- train(classe~., method = "rf",preProcess="pca"
                  ,data=pml.training_final, mtry= 5)
confusionMatrix(training)


library(caret)
emptycols <- sapply(pml.testing, function (k) all(is.na(k)))
pml.training_noNAcol <- pml.training[!emptycols]
pml.training_final <- pml.training_noNAcol[,c(8:59)]
pml.training_final_all <- pml.training_noNAcol[,c(8:60)]

preProc <- preProcess(pml.training_final, method = "pca", pcaComp=25)
preProc
trainPC <- predict(preProc, pml.training_final)
modelFit <- train(pml.training_final_all$classe~.,method="lda", data= trainPC)
modelFit$finalModel # what are fitted values
confusionMatrix(modelFit)
predict(modelFit, pml.training)


principalcomps <- prcomp(pml.training_final)
summary(principalcomps)

### This code produces 90% on the quiz
library(caret)
emptycols <- sapply(pml.testing, function (k) all(is.na(k)))
pml.training_noNAcol <- pml.training[!emptycols]
pml.training_final <- pml.training_noNAcol[,c(8:59)]
pml.training_final_all <- pml.training_noNAcol[,c(8:60)]

principalcomps <- prcomp(pml.training_final)
summary(principalcomps)

plot(principalcomps, type = "l")

preProc <- preProcess(pml.training_final, method = "pca", pcaComp=6)
preProc

trainPC <- predict(preProc, pml.training_final)
modelFit <- train(pml.training_final_all$classe~.,method="rf", data= trainPC,prox=TRUE)
modelFit$finalModel # what are fitted values
confusionMatrix(modelFit)

testPC <- predict(preProc, pml.testing)
#confusionMatrix((pml.testing$classe, predict(modelFit, testPC)))
predict(modelFit, testPC)
