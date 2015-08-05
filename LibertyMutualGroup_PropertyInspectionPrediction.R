# Liberty Mutual Group: Property Inspection Prediction
# https://www.kaggle.com/c/liberty-mutual-group-property-inspection-prediction
# Deadline: 28 aug 2015

# The evaluation metric for this competition is Normalized GINI
# (https://www.kaggle.com/c/liberty-mutual-group-property-inspection-prediction/details/evaluation)
# GINI code: https://www.kaggle.com/wiki/RCodeForGini

# TIPS/CODE:
# R:
# https://www.kaggle.com/vivekag/liberty-mutual-group-property-inspection-prediction/ensemble-of-rf-and-xgboost-in-r
# https://www.kaggle.com/benhamner/liberty-mutual-group-property-inspection-prediction/random-forest-benchmark
# Python:
# https://www.kaggle.com/devinanzelmo/liberty-mutual-group-property-inspection-prediction/xgboost-benchmark-0-38019

# TODO:

set.seed(1000)

source("tools.R")
library(caret)
library(randomForest)
#library(gbm)
#package.install("readr")
library(readr)
library(caTools)
#library(wordcloud)
#library(RColorBrewer)
#library(rpart)
#library(rpart.plot)
#library(Metrics)
#library(SnowballC)
#library(tm)
#library(png)
#library(sqldf)
#package.install("xgboost")
library(xgboost)
# etc.

SetStandardOptions()

# -------------------------------------------------------------------------------------------------------------------------------

trainfolder <- "C:/coding/Kaggle/Liberty MutualGroup_PropertyInspectionPrediction/data/"
submissionfolder <- "C:/coding/Kaggle/Liberty MutualGroup_PropertyInspectionPrediction/submissions"

#train <- read.csv(paste0(trainfolder, "train.csv"), header=T, sep=",", stringsAsFactors=T)
#test <- read.csv(paste0(trainfolder, "test.csv"), header=T, sep=",", stringsAsFactors=T)
train <- read_csv(paste0(trainfolder, "train.csv"))
test <- read_csv(paste0(trainfolder, "test.csv")) # NOTE: Retrieve warning/error details (if any) with problems()

head(train)
sapply(train, class)
names(train)
names(test)
table(train$T1_V1) # TODO: Many of the int predictors could be factors!
table(train$T1_V2)
table(train$T1_V3)
table(train$T2_V9)
table(train$T2_V10)
barplot(table(train$Hazard), col="orange", main="Hazard", las=2)
CorrelationPlot(train[, c(-1,-2)])

# --------------------------------------------------------------------------------------------------------------------------------
# Script from: 
# https://www.kaggle.com/benhamner/liberty-mutual-group-property-inspection-prediction/random-forest-benchmark

# Create the response variable
y <- train$Hazard

# Create the predictor data set and encode categorical variables using caret library.
mtrain <- train[,-c(1,2)]
mtest <- test[,-c(1)]
dummies <- dummyVars(~ ., data=mtrain)
mtrain <- predict(dummies, newdata=mtrain)
mtest <- predict(dummies, newdata=mtest)

cat("Training model - RF\n")
set.seed(8)
rf <- randomForest(mtrain, y, ntree=180, imp=TRUE, sampsize=10000, do.trace=TRUE)
predict_rf <- predict(rf, mtest)

# Set necessary parameters and use parallel threads
param <- list("objective"="reg:linear", "nthread"=8, "verbose"=0)

cat("Training model - Xgboost\n")
# Fit the model
xgb.fit <- xgboost(param=param, data=mtrain, label=y, nrounds=1700, eta=.01, max_depth=7, 
                  min_child_weight=5, scale_pos_weight=1.0, subsample=0.8) 
predict_xgboost <- predict(xgb.fit, mtest)

# Predict Hazard for the test set
submission <- data.frame(Id=test$Id)
submission$Hazard <- (predict_rf + predict_xgboost) / 2
head(submission)

# --------------------------------------------------------------------------------------------------------------------------------
# Create the submission file
# options("scipen"=100, "digits"=8)
KaggleSubmission(submission, submissionfolder, "RF_XGBoost")
