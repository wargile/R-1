# West Nile Virus Prediction
# http://www.kaggle.com/c/predict-west-nile-virus/data
# Deadline: 17.06.2015

# https://www.kaggle.com/c/predict-west-nile-virus/scripts
# https://www.kaggle.com/users/86616/finmore/predict-west-nile-virus/mosquito-count-time-series
# https://www.kaggle.com/users/1455/michael-jahrer/predict-west-nile-virus/xgboost-starter-code-python-0-69

library(randomForest)
library(kernlab)
library(e1071)
library(ROCR)
library(caret)
package.install("Metrics")
library(Metrics)

set.seed(16071962)

# Set some standard graphical params for plot
SetStandardOptions()

dataFolder <- "C:/coding/Kaggle/WestNileVirusPrediction/data/"
codeFolder <- "C:/coding/Kaggle/WestNileVirusPrediction/code/"
submissionsFolder <- "C:/coding/Kaggle/WestNileVirusPrediction/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  spray <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  weather <- read.csv(paste0(dataFolder, "weather.csv"), header=T, sep=",", stringsAsFactors=F)
  sampleSubmission <- read.csv(paste0(dataFolder, "sampleSubmission.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
  save(spray, file=paste0(dataFolder, "spray.rda"))
  save(weather, file=paste0(dataFolder, "weather.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "spray.rda"))
  load(paste0(dataFolder, "weather.rda"))
}

dim(train)
str(train)
names(train)
View(train[1:100,])
names(spray)
View(spray[1:100,])
names(weather)
View(weather[1:100,])
names(test)

par(mar=c(10,3,2,1))
barplot(sort(table(train$WnvPresent)), las=2, main="WnvPresent", cex.names=.6)
barplot(sort(table(train$Street)), las=2, main="Street", cex.names=.6)
barplot(sort(table(train$Trap)), las=2, main="Train trap", cex.names=.6)
length(unique(train$Trap))
barplot(sort(table(spray$Trap)), las=2, main="Spray trap", cex.names=.6)
length(unique(spray$Trap))
barplot(sort(table(weather$Trap)), las=2, main="Weather trap", cex.names=.6)
length(unique(weather$Trap))
length(unique(test$Trap))
par(mar=c(3,3,2,1))

table(complete.cases(train)) # No NA's
table(sampleSubmission$WnvPresent) # Not present at all!

# TODO: Fix missing species in train/test
# https://www.kaggle.com/users/103872/gayatri-mahesh/predict-west-nile-virus/starter-logistic-regression-in-r

train$Month <- as.factor(substr(train$Date, 6,7))
test$Month <- as.factor(substr(test$Date, 6,7))
test$Species[test$Species == "UNSPECIFIED CULEX"] <- "CULEX ERRATICUS"
table(train$Month)
levels(train$Month)
train$Species <- as.factor(train$Species)
test$Species <- factor(test$Species, levels=levels(train$Species))
table(train$Species)
levels(train$Species)
train$WnvPresent <- as.factor(make.names(train$WnvPresent))
levels(train$WnvPresent)
result <- CreateTrainAndValidationSets(train)
train.subset <- result[[1]]
validation.subset <- result[[2]]

f <- formula(as.factor(WnvPresent) ~ Month + Species + Latitude + Longitude + Block)
fitCv <- randomForest(f, data=train.subset)
fitCv
pred <- predict(fitCv, validation.subset, type="prob")
max(pred[,2])
min(pred[,2])
plot(sort(pred[,2]), col="blue")
table(validation.subset$WnvPresent, pred[,2] > 0.5)
table(pred[,2] > 0.5)
table(validation.subset$WnvPresent)
# Plot ROC curve and get AUC
PlotROC(validation.subset$WnvPresent, pred[,2])

f <- formula(WnvPresent ~ Month + Species + Latitude + Longitude + Block)
fitCv <- glm(f, data=train.subset, family=binomial(link="logit"))
fitCv
pred <- predict(fitCv, validation.subset, type="response")
max(pred)
min(pred)
plot(sort(pred), col="blue")
table(validation.subset$WnvPresent, pred > 0.5)
table(validation.subset$WnvPresent)
# Plot ROC curve and get AUC
PlotROC(validation.subset$WnvPresent, pred)

# ----------------------------------------------------------------------------------------------------------------------

f <- formula(as.factor(WnvPresent) ~ Month + Species + Latitude + Longitude + Block)
fitCv <- randomForest(f, data=train)
fitCv
pred <- predict(fitCv, test, type="response")
max(pred)
min(pred)
plot(sort(pred), col="blue")

f <- formula(WnvPresent ~ Month + Species + Latitude + Longitude + Block)
fitCv <- glm(f, data=train, family=binomial(link="logit"))
fitCv
pred <- predict(fitCv, test, type="response")
max(pred)
min(pred)
plot(sort(pred), col="blue")

# ----------------------------------------------------------------------------------------------------------------------
colnames(submission) <- c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
head(submission)

# Save the submission data
save(submission, file=paste0(submissionsFolder, "submission.rda"))

KaggleSubmission(submission, submissionsFolder, "glm")                         
