# San Fransisco Crime Classification
# https://www.kaggle.com/c/sf-crime
# Deadline: 06.06.2016

# The evaluation metric for this competition is...

# TIPS/CODE:

# TODO:

set.seed(1000)

source("tools.R")
library(caret)
library(randomForest)
library(gbm)
package.install("readr")
library(readr)
library(caTools)
library(wordcloud)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(Metrics)
library(SnowballC)
library(tm)
library(png)
# etc.
SetStandardOptions()

# -------------------------------------------------------------------------------------------------------------------------------

dataFolder <- "C:/coding/Kaggle/SanFranciscoCrimeClassification/data/"
submissionsFolder <- "C:/coding/Kaggle/SanFranciscoCrimeClassification/submissions/"

colClasses <- c("character","factor","character","factor","factor","factor","character","numeric","numeric")
names(train)
str(train)

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", colClasses=colClasses)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", colClasses=colClasses[-6])
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "test_datetime.rda"))
}

par(mar=c(12,3,2,1))
barplot(table(train$Resolution), col="wheat", las=2, cex.lab=.7, main="Resolution")
par(mar=c(6,3,2,1))
barplot(table(train$PdDistrict), col="wheat", las=2, cex.lab=.7, main="PdDistrict")
par(mar=c(3,3,2,1))

set.seed(1000)
split <- sample.split(train$Resolution, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)

fit.rf <- randomForest(Resolution ~., data=train.subset[1:100000, c(-1,-3)], ntrees=25)
varImpPlot(fit.rf)
pred.rf <- predict(fit.rf, validation.subset)
table(validation.subset$Resolution, pred.rf)

# --------------------------------------------------------------------------------------------------------------------------------
# Create the submission file
MySubmission <- data.frame(id=test$Id, prediction=pred) # TODO...
head(MySubmission)
KaggleSubmission(MySubmission, submissionFolder, "TODO")
