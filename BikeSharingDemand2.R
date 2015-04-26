# Bike Sharing Demand

# Deadline: May 29, 2015
# http://www.kaggle.com/c/bike-sharing-demand/data

# http://phillymotu.files.wordpress.com/2013/04/the-impact-of-weather-conditions-on-capital-bikeshare-trips.pdf
# http://mobilitylab.org/2014/01/27/explore-the-links-between-weather-and-capital-bikeshare-ridership/
# https://class.coursera.org/datasci-002/forum/thread?thread_id=1761
# http://stackoverflow.com/questions/24903228/unexpected-predict-result-for-linear-regression-in-r
# http://www.kaggle.com/c/bike-sharing-demand/forums/t/9840/sharing-approaches

# http://beyondvalence.blogspot.com/2014/06/predicting-capital-bikeshare-demand-in.html
# http://beyondvalence.blogspot.com/2014/07/predicting-capital-bikeshare-demand-in.html
# http://beyondvalence.blogspot.com/2014/07/predicting-capital-bikeshare-demand-in_10.html
# http://www.capitalbikeshare.com/system-data
# http://brandonharris.io/kaggle-bike-sharing/

# TIPS/EXPERIENCES:
# Predict Casual and Registered outcome vars instead of Count directly, since they have different patterns.
# Then combine to get count. (Seems to worsen score...)
# Can timeseries forecasting be used here?
# Year is important as predictor!
# ------------------------------------------------------------------------------------------------------------

set.seed(16071962)
source("tools.R")

dataFolder <- "C:/coding/Kaggle/BikeSharingDemand/data/"
codeFolder <- "C:/coding/Kaggle/BikeSharingDemand/code/"
submissionsFolder <- "C:/coding/Kaggle/BikeSharingDemand/submissions/"

colClasses = c(
  "character", # datetime
  "factor", # season
  "factor", # holiday
  "factor", # workingday
  "factor", # weather
  "numeric", # temp
  "numeric", # atemp
  "integer", # humidity
  "numeric", # windspeed
  "integer", # casual
  "integer", # registered
  "integer") # count

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", colClasses=colClasses)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", colClasses=colClasses[c(-10,-11,-12)])
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "test_datetime.rda"))
}

dim(train)
dim(test)

head(train)
head(test)

str(train)
str(test)

summary(train)
summary(test)

trainrows <- dim(train)[1]
testrows <- dim(test)[1]

sapply(train, class)

# Save datetime col from test set
datetime.test <- test$datetime
save(datetime.test, file=paste0(dataFolder, "test_datetime.rda"))

# Normalize/scale temp, atemp, humidity and windspeed
cdata$temp <- scale(cdata$temp, center=T, scale=T)
cdata$atemp <- scale(cdata$atemp, center=T, scale=T)
cdata$humidity <- scale(cdata$humidity, center=T, scale=T)
cdata$windspeed <- scale(cdata$windspeed, center=T, scale=T)

# Add dummy values to test dataframe (not needed for prediction, but for binding train/test for transform)
test$casual <- 0
test$registered <- 0
test$count <- 0

# Add test and train together before doing transforms
cdata <- rbind(train, test)
# Extract hour, weekday, month, and year from datetime
datetime <- as.POSIXlt(cdata$datetime)
# hour = datetime$hour
# Can not get hour, seems to be an issue with one or more of the datetime rows, so using strptime
hour <- as.factor(strptime(cdata$datetime, format="%Y-%m-%d %H:%M:%S")$hour)
weekday <- as.factor(datetime$wday)
month <- as.factor(datetime$mon)
year <- 1900 + datetime$year
cdata$datetime <- datetime

# Round temp, atemp and windspeed cols
cdata$temp <- round(cdata$temp)
cdata$atemp <- round(cdata$atemp)
cdata$windspeed <- round(cdata$windspeed)

# Add the new features to the combined dataframe
cdata = cbind(cdata, hour, weekday, month, year)
head(cdata)

# Split in the corresponding train/test datasets
train = cdata[0:trainrows, ]
test = cdata[(trainrows + 1):(trainrows + testrows), ]
head(train)
head(test)

# Save the fixed datasets:
save(train, file=paste0(dataFolder, "train.rda"))
save(test, file=paste0(dataFolder, "test.rda"))

# TODO: Nice heatmap cor matrix:
# http://stackoverflow.com/questions/10680658/how-can-i-create-a-correlation-matrix-in-r
CorrelationPlot(train[, c(6,7,8,9,10,11,12)])

# NOTE: There is a clear trend/cycle in casual/registered/count. Add the same type of trend to test data for
# the same time period/month/dates? These predictors are (naturally) very important, but is it more important to find 
# the DIFFERENCE between registered and casual, and use that as some kind of "weight" predictor in train and test sets??
plot(train$registered[1:150], type="l", col="green", lwd=2, main="Registered/casual/count",
     ylab="Frequency", xlab="Time", xaxt="n", cex.main=1, cex.axis=.8)
lines(train$casual[1:150], col="red")
lines(train$count[1:150], col="blue")
axis(side=1, at=c(1, 50, 100, 150), cex.axis=.8,
     labels=c(train$datetime[1], train$datetime[50], train$datetime[100], train$datetime[150]))

plot(train$count ~ train$month, col="orange", main="Count by month", cex.main=1, cex.axis=.8, cex.lab=.8)
plot(train$casual ~ train$month, col="orange", main="Count by month", cex.main=1, cex.axis=.8, cex.lab=.8)
plot(train$registered ~ train$month, col="orange", main="Count by month", cex.main=1, cex.axis=.8, cex.lab=.8)
# NOTE: Registered: More even count throughout the year. Casual: Very summer/warm season focused.

# Check some predictors
op <- par()
par(mar=c(5,4.5,2,.8))
result <- aggregate(train$count, by=list(train$year), mean)
barplot(result$x, col=c("wheat","cornflowerblue"), main="Train: Mean of count", names.arg=unique(train$year),
        cex.axis=.8, cex.lab=.8, cex.main=1, cex.names=.8)
plot(count ~ as.factor(atemp), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by average temp")
cor(train$count, train$atemp)
plot(count ~ as.factor(temp), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by temp")
cor(train$count, train$temp)
plot(count ~ as.factor(humidity), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by humidity")
cor(train$count, train$humidity)
plot(count ~ as.factor(weather), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by weather")
plot(casual ~ as.factor(weather), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by weather")
plot(registered ~ as.factor(weather), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by weather")
cor(train$count, as.integer(train$weather))
plot(count ~ as.factor(hour), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by hour")
plot(casual ~ as.factor(hour), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by hour")
plot(registered ~ as.factor(hour), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by hour")
cor(train$count, as.integer(train$hour))
plot(count ~ month, data=train, col="cornflowerblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by month")
cor(train$count, as.integer(train$month))
plot(count ~ as.factor(season), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by season")
plot(count ~ as.factor(weekday), data=train, col="orangered",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by weekday")
plot(casual ~ as.factor(weekday), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by weekday")
plot(registered ~ as.factor(weekday), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by weekday")
plot(count ~ as.factor(windspeed), data=train, col="orangered",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by windspeed")

boxplot(train$humidity, test$humidity, col=c("red", "green"), main="Train/test humidity")
boxplot(train$atemp, test$atemp, col=c("red", "green"), main="Train/test atemp")
boxplot(train$windspeed, test$windspeed, col=c("red", "green"), main="Train/test windspeed")
par <- op

par(mfrow=c(1,2))
days <- train[train$workingday == 1, ]
plot(days$count ~ days$hour, col="steelblue4", cex.lab=.8, xlab="Hour", ylab="Count",
     cex.axis=.8, cex.main=.8, main="Count by hour, workingdays")
mean.count <- aggregate(count ~ hour, data=days, FUN=mean)
lines(mean.count, col="orange2", lwd=2)
days <- train[train$workingday == 0, ]
plot(days$count ~ days$hour, col="orange2", cex.lab=.8, xlab="Hour", ylab="Count",
     cex.axis=.8, cex.main=.8, main="Count by hour, non-workingdays")
mean.count <- aggregate(count ~ hour, data=days, FUN=mean)
lines(mean.count, lwd=2, col="steelblue4")
par(mfrow=c(1,1))

library(reshape)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
WeekHour=aggregate(count ~ hour + weekday, data=train, FUN=mean)
ggplot(data=WeekHour, aes(x=hour, y=count)) +
  geom_line(aes(group=weekday, color=weekday), size=1.5, alpha=.8)
  #scale_fill_manual(values=as.factor(weekday))

# -------------------------------------------------------------------------------------------------------
# Split train in train and validation sets

result <- CreateTrainAndValidationSets(data=rbind(train, train, train, train, train),
                                       percent.train=85, random.selection=T)
result <- CreateTrainAndValidationSets(data=train, percent.train=85, random.selection=T)
train.subset <- result$train
#train.subset <- rbind(result$train, result$train)
validation.subset <- result$validation
dim(train.subset)
dim(validation.subset)

# TODO: Scale scalars
# Include year??

# Try poisson regression
# http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
fit <- glm(count ~ hour + year + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=quasipoisson())
fit.casual <- glm(casual ~ hour + year + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=quasipoisson())
fit.registered <- glm(registered ~ hour + year + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=quasipoisson())
# Testing log transform on outcome variable
fit.log <- glm(as.integer(log(count)) ~ as.factor(hour) + as.factor(year) + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=poisson())

summary(fit)
summary(fit.casual)
summary(fit.registered)
summary(fit.log)
# NOTE: If residual deviance is larger than DF, consider using quasipoisson family instead. Here it is:
# Residual deviance: 1748.5  on 8117  degrees of freedom
pred <- predict(fit, validation.subset, type="response")
pred.casual <- predict(fit.casual, validation.subset, type="response")
pred.registered <- predict(fit.registered, validation.subset, type="response")
pred[1:50]
pred.log <- predict(fit.log, validation.subset, type="response")
pred.log <- exp(pred.log)
pred.log[1:50]
validation.subset$count[1:50]
rmsle <- MyRMSLE(validation.subset$count, pred)
rmsle.registered.casual <- MyRMSLE(validation.subset$count, pred.registered + pred.casual)
rmsle.log <- MyRMSLE(validation.subset$count, pred.log)

# TODO: Put this section in tools.R?
# Compare normal and log curves for count. Log more evenly cyclic:
op <- par()
par(mfrow=c(2,1))
par(mar=c(2.5,4,2,1))
n <- 50
plot(validation.subset$count[1:n], type="o", col="blue", xlab="", ylab="Count", cex.lab=.8, cex.axis=.8, cex.main=.8,
     main=paste0("y/pred (RMSLE: ", round(rmsle, 3), ")"))
points(pred[1:n], col="red", type="o")
legend("topright", legend=c("y","pred"), col=c("blue","red"), lwd=2, cex=.7)
plot(validation.subset$count[1:n], type="o", col="blue", xlab="", ylab="Count", cex.lab=.8, cex.axis=.8, cex.main=.8,
     main=paste0("y/pred r+c (RMSLE: ", round(rmsle.registered.casual, 3), ")"))
points(pred.registered[1:n] + pred.casual[1:n], col="red", type="o")
legend("topright", legend=c("y","pred r+c"), col=c("blue","red"), lwd=2, cex=.7)
par <- op

# Trying RF for comparison:
cols.to.include <- c(13, 7, 15, 5, 8, 9, 14, 16) # NOTE: Year included now
fit.rf <- randomForest(as.integer(train.subset$count) ~ .,
                       train.subset[, cols.to.include], proximity=F, keep.forest=T, ntree=100, nodesize=50) # ntree=150, 80
pred.rf <- predict(fit.rf, validation.subset[, cols.to.include], type="response")
rmsle.rf <- MyRMSLE(validation.subset$count, pred.rf)

op <- par()
par(mar=c(5,4,2,.8))
par(mfrow=c(2,2))
plot(pred.casual + pred.registered, col="blue", ylab="Count", main="Casual+registered")
grid()
plot(pred, col="orangered", ylab="Count", main="Count")
grid()
plot(validation.subset$count, col="green4", ylab="Count", main="Validation: Count")
grid()
par <- op

# -------------------------------------------------------------------------------------------------------
# Playing with timeseries...
library(forecast)
library(zoo)
library(xts)
library(chron)

data <- train$count[1:(24 * 19)]
data.date <- train$datetime[1:(24 * 18)-1]

# http://stackoverflow.com/questions/17156143/how-to-create-a-r-timeseries-for-hourly-data
# http://r.789695.n4.nabble.com/R-Hourly-Time-Series-td814382.html
time_index <- seq(from=as.POSIXct("2011-01-01 00:00:00"), 
                  to=as.POSIXct("2011-01-19 23:00:00"), by="hour")
tseries <- ts(data, start=1, freq=24)
plot(tseries)
plot(forecast(tseries))
hw <- HoltWinters(tseries)
fc <- forecast(hw, n.ahead=100, level=95)
plot(fc)
abs(min(fc$fitted)) # Some of the fitted values are below zero...

tseries.zoo <- zoo(x=data, order.by=data.date, frequency=24)
plot(tseries.zoo)

wind_ts <- ts(cumsum(rnorm(24*19)), start=1, frequency=24)
plot(forecast(HoltWinters(wind_ts), n.ahead=24)) # n.ahead - why no effect?
decompose(wind_ts)

# Arima?
fit <- arima(x=data, order=c(3,0,0))
forecasts <- predict(fit, n.ahead=12)
plot(data, type="l", col="blue")
lines(forecasts$pred+1.96*forecasts$se, col="red")
lines(forecasts$pred-1.96*forecasts$se, col="green")

value <- rnorm(n=length(time_index))
eventdata <- xts(train$count[1:(24 * 19)], order.by=time_index)
head(eventdata)
op <- par()
par(mfrow=c(1,1))
par(mar=c(7,4,2,1))
plot(eventdata, cex.lab=.7, cex.axis=.7, las=2)
ets(eventdata)
par <- op

plot(tseries)


# -------------------------------------------------------------------------------------------------------
# Do GBM

library(gbm)

fit <- gbm(train$count ~ .
            ,data=train[, -c(1,10,11,12)] # remove [datetime, casual, registered, count] columns
            ,var.monotone=NULL
            ,distribution="gaussian"
            ,n.trees=1200
            ,shrinkage=0.1
            ,interaction.depth=3
            ,bag.fraction=0.5
            ,train.fraction=1
            ,n.minobsinnode=10
            ,cv.folds=10
            ,keep.data=TRUE
            ,verbose=TRUE)

summary(fit, main="GBM variable importance", cex.axis=.8, cex.lab=.8, cex.main=1, cex.names=.7, las=1)
best.iter <- gbm.perf(fit, method="cv") # Find the best iteration number
pred <- predict(fit, test, best.iter, type="response")
pred[1:50]
pred[pred < 0] <- 0 # NOTE: Must remove those below zero!

submission=data.frame(datetime=datetime.test, count=round(pred))
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GBM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# -------------------------------------------------------------------------------------------------------
# Do LM: Note: Need family=poisson for count outcome. Note: Warnings about rank-deficient fit on ~.!
all.skip.cols <- -c(1,6,10,11,12)
all.include.cols <- c(3,5,6,8,9,13,15,16)
# NOTE: Skipping temp since atemp has high cor with temp, same with season (skipped)/month
fit <- glm(train$count ~ ., data=train[, all.skip.cols], family=quasipoisson())
fit <- glm(as.integer(count) ~ as.factor(hour) + atemp + season + weather + humidity + year, data=train,
          na.action=na.exclude, family="poisson")

fit <- glm(train$count ~ ., data=train[, all.include.cols], , family="poisson")
# Tried the one below for last glm attempt (26.08)
fit <- glm(count ~ hour + atemp + month + weather + humidity + windspeed + weekday + year,
           data=train, na.action=na.exclude, family="quasipoisson")

# Try log on the outcome variable:
fit.log <- glm(log(as.integer(count)) ~ as.factor(hour) + atemp + season + weather + humidity + year, data=train,
           na.action=na.exclude, family="poisson")

require(sandwich)
require(msm)
cov.m1 <- vcovHC(fit, type="HC0")
plot(cov.m1)

summary(fit)
summary(fit.log)

test$hour <- as.integer(test$hour)
test$weekday <- as.integer(test$weekday)
pred <- predict(fit, test, type="response")
pred[1:50]
pred.log <- predict(fit.log, test, type="response")
exp(pred.log[1:50])

pred[pred < 0] <- 0

par(mfrow=c(2,1))
plot(train$count, ylim=c(0, 1000), col="green4")
grid()
plot(pred, ylim=c(0, 1000), col="blue")
grid()
par(mfrow=c(1,1))

submission=data.frame(datetime=datetime.test, count=pred)
head(submission)
submission.log=data.frame(datetime=datetime.test, count=round(exp(pred.log)))
head(submission.log)

write.csv(submission, file=paste0(submissionsFolder, "GLM_Poisson_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
write.csv(submission.log, file=paste0(submissionsFolder, "GLM_Poisson_Log_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# -------------------------------------------------------------------------------------------------------
# Do Random Forest
library(randomForest)

# TODO: Split train set and loop to find best ntree number
# TESTED: Create separate rf models for Casual and Registered, predict with both, and then average results (worse score!)

# NOTE: Transformed train$hour and test$hour to categorical:
train$hour <- as.factor(train$hour)
test$hour <- as.factor(test$hour)

# Split train in test and train, and get best ntrees value:
rows <- dim(train)[1]
rows <- sample(1:rows, rows / 2, replace=T)
all.skip.cols <- -c(1,3,6,10,11,12,16) # NOTE: Skipping temp since atemp has high cor with temp
trainSubset <- train[rows, ]
testSubset <- train[-rows, ]

pos <- 1
result <- integer()

for (counter in seq(1, 30, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(trainSubset$count ~ ., trainSubset[, all.skip.cols],
                               proximity=TRUE, keep.forest=TRUE, ntree=counter)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=testSubset[, all.skip.cols], type="response")
  #prediction <- ifelse(prediction == "false", 0, 1)
  the.result <- (round(prediction) == testSubset$count)
  table(the.result)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()

# NOTE: When combining, all models must have same predictors!
forestTrain1 <- randomForest(train$count ~ ., train[, -c(1,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2 <- randomForest(train$count ~ ., train[, -c(1,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3 <- randomForest(train$count ~ ., train[, -c(1,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
# ntree=200 gives worse results....

forestTrain1.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestTrain1.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestCombined <- combine(forestTrain1, forestTrain2, forestTrain3)
print(forestCombined)
forestCombined$importance
# Nice importance plot:
varImpPlot(forestCombined, cex=.8, col="blue", pch=19, main="RF Importance Plot")

pred <- predict(forestCombined, test, type="response")
pred[1:50]

forestCombined.casual <- combine(forestTrain1.casual, forestTrain2.casual)
forestCombined.registered <- combine(forestTrain1.registered, forestTrain2.registered)
pred.casual <- predict(forestTrain1.casual, test, type="response") 
pred.registered <- predict(forestTrain1.registered, test, type="response") 
pred.combined <- (pred.casual + pred.registered) / 2 
pred.combined[1:100]

submission=data.frame(datetime=datetime.test, count=round(pred))
submission=data.frame(datetime=datetime.test, count=pred) # Better not to round???
submission=data.frame(datetime=datetime.test, count=pred.combined) # Better not to round???
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "RF_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# Combination of 2 RF's predicting Count with 150 trees and train[, -c(1,3,6,10,11,12,16)] gives best entry: 0.57538

# -------------------------------------------------------------------------------------------------------------------
# Caret package (tip: Test on Sonar dataset)
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

library(caret)
library(mlbench)

all.skip.cols <- -c(1,2,3,4,5,6,10,11)

inTrain <- createDataPartition(y=train$count,
                                ## the outcome data are needed
                                p=.75,
                                ## The percentage of data in the
                                ## training set
                                list=F)
str(inTrain)
training <- train[inTrain, all.skip.cols]
testing <- train[-inTrain, all.skip.cols]
nrow(testing)
nrow(training)

ctrl <- trainControl(method="repeatedcv", # oob, rf only
                      repeats=3,
                      classProbs=F,
                      summaryFunction=defaultSummary)

plsFit <- train(training$count ~ .
                ,data=training
                ,method="rf" # Was: rf. nb = Naive Bayes?
                #,tuneLength=15
                ,trControl=ctrl
                ,metric="PROC" # Was: RMSE
                #,preProc=c("center", "scale")
                )

plsClasses <- predict(plsFit, newdata=testing)
plot(plsFit)
plsProbs <- predict(plsFit, newdata=testing, type="raw")
head(plsProbs)
plot(plsProbs[1:100], type="l")
confusionMatrix(data=plsClasses, testing$count)
