library(xgboost)
library(tidyverse)
library(caret)
library(MASS)
library(readr)
library(caTools)
library(dplyr)

#Set seed and read data
set.seed(1494)
heartdata <- read_csv("~/R/Projects/HeartAttackClassification/heartdata.csv")

#Identify categorical columns and continuous columns
catcolumns <- (c(2, 3, 6, 9, 11, 12, 13,14))
contcolumns <- (c(1,4,5,7,8,10))

#Convert to factors
heartdata[catcolumns] <- lapply(heartdata[catcolumns], factor)

#Scale continuous variables. This is not needed for xgboost but is necessary for logit

heartdata[contcolumns] <- scale(heartdata[contcolumns])

#Test/Train split
parts = createDataPartition(heartdf$output, p = 0.8, list = F)
trainingdata = heartdata[parts, ]
testdata = heartdata[-parts, ]

#Splitting into predictor and response variables. Y labels are changed to yes/no due to caret
trainx = data.matrix(trainingdata[,-14])
trainy = factor(trainy, labels = c("yes","no"))

testx = data.matrix(testdata[,-14])
testy = factor(testy, labels = c("yes","no"))

#Setting grid search with 5-fold cross validation to find best parameters

tunecontrol <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE,
  search = "grid"
)

#Fitting the initial model

model1 <- train(
  x = trainx,
  y = trainy,
  trControl = tunecontrol,
  method = 'xgbTree',
  verbose = TRUE
)

#Creating confusion matrix to test accuracy, sensitivity, and specificity

xgbpred = predict(model1, newdata = testx)
confusion = confusionMatrix(testy, xgbpred)
confusion

#Checking best param results

model1$bestTune

#Second tuning grid and model with best hyperparameters

grid2 <- expand.grid(
  nrounds = 100,
  max_depth = 1,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.5
)

model2 <- train(
  x = trainx,
  y = trainy,
  trControl = tunecontrol,
  tuneGrid = gridfinal,
  method = 'xgbTree',
  verbose = TRUE
)

xgbpred2 = predict(model2, newdata = testx)
confusion = confusionMatrix(testy, xgbpred2)
confusion

#Accuracy in the confusion matrix increased significantly from 0.7667 to 0.85

#Fitting the logistic regression model

logmodel = glm(output ~ ., data = trainingdata, family =binomial(link = 'logit'))
logmodel

#p-value is significant at the 0.05 level
with(logmodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#Testing on new data

fitted.results <- predict(logmodel, newdata = testdata) 
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

#Accuracy measure

misClasificError <- mean(fitted.results != testdata$output)
accuracy <- 1-misClasificError
accuracy

#Accuracy for the linear model was found to be the same as the boosted model

##Will add other methods in the future 