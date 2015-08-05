# UCI Million Song Dataset (subset) ridge regression practice
# -----------------------------------------------------------

# Main page: https://archive.ics.uci.edu/ml/datasets/YearPredictionMSD

# Attribute Information:
# 90 attributes, 12 = timbre average, 78 = timbre covariance 
# The first value is the year (target), ranging from 1922 to 2011. 
# Features extracted from the 'timbre' features from The Echo Nest API. 
# We take the average and covariance over all 'segments', each segment 
# being described by a 12-dimensional timbre vector.

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(ggmap)
library(mapproj)
library(ggvis)
library(dplyr)
library(tm)
library(h2o)

# Set locale to US, to ensure that there aren't formatting issues in assignments/inputs:
Sys.setlocale("LC_ALL", "C")

set.seed(1000)
SetStandardOptions()
n <- 91
colClasses <- rep("numeric", n)
colNames <- c("Year", sapply(1:(n-1), function(x) paste0("Timbre", x)))
data <- read.csv("C:/coding/R/TestData/YearPredictionMSD.txt", header=F, colClasses=colClasses)
names(data) <- colNames
# Split in train and test like this:
train.rows <- 463715
test.rows <-  51630
head(data)
dim(data)
nrow(data) == train.rows + test.rows
train.rows + test.rows

plot(log(table(data$Year)), las=2, col="blue", main="Year")
plot((table(data$Year)), las=2, col="blue", main="Year")

# NOTE: A very left-skewed dataset, distinct peak last 20 years
barplot(table(data$Year), col="orange", main="Songs by Year", las=2)
hist(data$Year, col="violetred3", main="Year distribution", xlab="Year", xlim=c(1920, 2020),
     breaks=40, ylab="")
data$AverageTimbre <- rowSums(data[, 2:91])
barplot(with(data, tapply(AverageTimbre, Year, mean)), col="powderblue", main="Average timbre by year", las=2)
image(scale(cov(data[, 2:91])))

CorrelationPlot(data[, 2:30])
CorrelationPlot(data[, 31:60])
CorrelationPlot(data[, 61:91])

# TODO: Remove predictors that have close to zero variance (if any?)
variance <- var(data)

train <- data[1:train.rows, ]
test <- data[(train.rows + 1):nrow(data), ]
dim(train)
dim(test)

# ----------------------------------------------------------------------------------------------------------

#library(caTools)
set.seed(1000)
# NOTE: Can also use caret's createDataPartition here
split <- sample.split(train$Year, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)

graph.maxshow <- 80
model <- lm(Year ~ ., data=train[, 1:91])
summary(model)
pred <- predict(model, newdata=validation.subset)
plot(validation.subset$Year[1:graph.maxshow], type="o", col="blue")
lines(pred[1:graph.maxshow], col="red")
MyRMSE(validation.subset$Year, pred)
# Try on full test set:
pred.test <- predict(model, newdata=test)
plot(test$Year[1:graph.maxshow], type="o", col="blue")
lines(pred.test[1:graph.maxshow], type="o", col="red")
MyRMSE(test$Year, pred.test)

ctrl <- trainControl(method="cv")
tuneGrid <- expand.grid(lambda=seq(0.01, 0.1, 0.01))
ridgeFit <- train(Year ~ ., data=train.subset[1:5000, 1:91], method="ridge",
                  preProc=c("center", "scale"), trControl=ctrl, tuneGrid=tuneGrid, metric="RMSE")
# Try without trControl/cv and preproc
ridgeFit <- train(Year ~ ., data=train.subset[1:100000, 1:91], method="ridge",
                  preProc=c("center", "scale"), tuneGrid=tuneGrid, metric="RMSE", verbose=T)
ridgeFit # Show RMSE and other model info
plot(ridgeFit)
pred.caret <- predict(ridgeFit, newdata=validation.subset)
plot(validation.subset$Year[1:graph.maxshow], type="o", col="blue")
lines(pred.caret[1:graph.maxshow], type="o", col="red")
MyRMSE(validation.subset$Year, pred.caret)

# ----------------------------------------------------------------------------------------------------------

# TODO: h2o, good tutorial on Generalized Linear Modeling (GLM):
# https://leanpub.com/glm/read

# TEST: Set Year as Log(Year):
# train.subset$YearLog <- log(train.subset$Year)
localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
dat_h2o <- as.h2o(train.subset, conn=localH2O, destination_frame="train")
dat_h2o.test <- as.h2o(validation.subset, conn=localH2O, destination_frame="test")
y.col <- 1
x.cols <- 2:91

train.glm <- h2o.glm(x=x.cols,
  y=y.col,
  training_frame=dat_h2o,
  model_id = "glm_model",
  family="gaussian",
  lambda_search = TRUE)

train.glm  
h2o_yhat_test <- h2o.predict(train.glm, dat_h2o.test)
df_yhat_test <- as.data.frame(h2o_yhat_test)
min.y <- min(min(exp(df_yhat_test$predict[1:80])), min(validation.subset$Year[1:80]))
max.y <- min(max(exp(df_yhat_test$predict[1:80])), max(validation.subset$Year[1:80]))
min.y <- min(min(df_yhat_test$predict[1:80]), min(validation.subset$Year[1:80]))
max.y <- min(max(df_yhat_test$predict[1:80]), max(validation.subset$Year[1:80]))
plot(df_yhat_test$predict[1:80], col="red", type="o", ylim=c(min.y, max.y))
plot(exp(df_yhat_test$predict[1:80]), col="red", type="o", ylim=c(min.y, max.y))
lines(validation.subset$Year[1:80], col="blue", type="o")
MyRMSE(validation.subset$Year, df_yhat_test$predict)
MyRMSE(validation.subset$Year, exp(df_yhat_test$predict))
