# Bike Sharing Demand
# -------------------
# Deadline: May 29, 2015
# http://www.kaggle.com/c/bike-sharing-demand/data

# http://datasciencedojo.com/build-a-random-forest-in-azure-ml-using-r/
# http://phillymotu.files.wordpress.com/2013/04/the-impact-of-weather-conditions-on-capital-bikeshare-trips.pdf
# http://mobilitylab.org/2014/01/27/explore-the-links-between-weather-and-capital-bikeshare-ridership/
# https://class.coursera.org/datasci-002/forum/thread?thread_id=1761
# http://stackoverflow.com/questions/24903228/unexpected-predict-result-for-linear-regression-in-r
# http://www.kaggle.com/c/bike-sharing-demand/forums/t/9840/sharing-approaches
# http://beyondvalence.blogspot.com/2014/06/predicting-capital-bikeshare-demand-in.html
# http://beyondvalence.blogspot.com/2014/07/predicting-capital-bikeshare-demand-in.html
# http://beyondvalence.blogspot.com/2014/07/predicting-capital-bikeshare-demand-in_10.html
# http://brandonharris.io/kaggle-bike-sharing/
# http://technospeaknow.blogspot.in/2015/02/prediction-of-registered-bike-sharing.html
# http://www.kaggle.com/c/bike-sharing-demand/forums/t/11525/tutorial-0-433-score-with-randomforest-in-r/64333#post64333

# TIPS:
# Predict Casual and Registered outcome vars instead of Count directly, since they have different patterns. Combine to get count.
# Can timeseries forecasting be used here?
# Year is important as predictor!
# log(train$count) + 1 better outcome variable for model and no Poisson regression, just standard LM?
# --------------------------------------------------------------------------------------------------------------------------------

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/BikeSharingDemand/data/"
codeFolder <- "C:/coding/Kaggle/BikeSharingDemand/code/"
submissionsFolder <- "C:/coding/Kaggle/BikeSharingDemand/submissions/"

# Set some standard graphical params for plot
oldpar <- par()
SetStandardoptions()

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
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

summary(train)
summary(test)

pairs(train) # Look at all vars against each other (NOTE: all args must be numeric)

trainrows <- dim(train)[1]
testrows <- dim(test)[1]

sapply(train, class)

# Get the variance. Eliminate near zero var predictors (but not always! Test for length(unique(...) or min/max range first)
# Predictors should have a reasonable number of unique values. 0/1 will of course have a tiny variance anyway.
train2 <- train[, c(-1,-10,-11,-12)]
variance <- sapply(train2, var)
op <- par()
par(mar=c(5.5,4.5,2.5,.8))
# plot(sort(variance), type="l", col="blue", main="Variance", cex.axis=.8, cex.lab=.8)
limit <- 0.05
cols <- which(variance > limit)
plot(variance[cols], type="o", col="blue", main=paste0("Variance > ", limit), cex.axis=.7, cex.lab=.7,
     xaxt="n", xlab="", ylab="Var", pch=21, bg="cyan", cex.main=.8)
axis(1, at=1:length(names(train2[cols])), labels=names(train2[cols]), las=2, cex.axis=.7)
par <- op

# Save datetime col from test set
datetime.test <- test$datetime
save(datetime.test, file=paste0(dataFolder, "test_datetime.rda"))

# Add dummy values to test dataframe (not needed for prediction, but for binding train/test for transform)
test$casual <- 0
test$registered <- 0
test$count <- 0

# NOTE: There is a clear trend/cycle in casual/registered/count. Add the same type of trend to test data for
# the same time period/month/dates? These predictors are (naturally) very important, but is it more important to find 
# the DIFFERENCE between registered and casual, and use that as some kind of "weight" predictor in train and test sets??
plot(train$registered[1:150], type="l", col="green", lwd=2, main="Registered/casual/count",
     ylab="Frequency", xlab="Time", xaxt="n", cex.main=.8, cex.axis=.7, cex.lab=.7)
lines(train$casual[1:150], col="red")
lines(train$count[1:150], col="blue")
axis(side=1, at=c(1, 50, 100, 150), cex.axis=.7,
     labels=c(train$datetime[1], train$datetime[50], train$datetime[100], train$datetime[150]))

plot(train$count ~ train$month, col="orange", main="Count by month", cex.main=.8, cex.axis=.7, cex.lab=.7)
plot(train$casual ~ train$month, col="orange", main="Count (casual) by month", cex.main=.8, cex.axis=.7, cex.lab=.7)
plot(train$registered ~ train$month, col="orange", main="Count (registered) by month", cex.main=.8, cex.axis=.7, cex.lab=.7)
# NOTE: Registered: More even count throughout the year. Casual: Very summer/warm season focused.

# NOTE: Log(y) versus y:
par(mfrow=c(2,1))
plot(train$count[1:350], type="l", col="red")
plot(log(train$count[1:350] + 1), type="l", col="blue")
par(mfrow=c(1,1))

# Add test and train together before doing transforms
cdata <- rbind(train, test)

# Convert some features to factors
cdata$season <- as.factor(cdata$season)
cdata$holiday = as.factor(cdata$holiday)
cdata$workingday <- as.factor(cdata$workingday)
cdata$weather <- as.factor(cdata$weather)

# Extract hour, weekday, month, and year from datetime
datetime <- as.POSIXlt(cdata$datetime)
# hour = datetime$hour
# Can not get hour, seems to be an issue with one or more of the datetime rows, so using strptime
hour <- strptime(cdata$datetime, format="%Y-%m-%d %H:%M:%S")$hour
weekday <- as.factor(datetime$wday)
month <- as.factor(datetime$mon)
year <- 1900 + datetime$year
cdata$datetime <- datetime

# Round temp, atemp and windspeed cols (TODO: Rounding has adverse effect??)
cdata$temp <- ceil(cdata$temp)
cdata$atemp <- ceil(cdata$atemp)
cdata$windspeed <- ceil(cdata$windspeed)

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
train2 <- train[, c(-1,-10,-11)]
train2$season <- as.numeric(train2$season)
train2$holiday <- as.numeric(train2$holiday)
train2$workingday <- as.numeric(train2$workingday)
train2$weather <- as.numeric(train2$weather)
train2$weekday <- as.numeric(train2$weekday)
train2$month <- as.numeric(train2$month)

CorrelationPlot(train2)

# ------------------------------------------------------------------------------------------------------------

# TODO: Fix the col class once and for all...

# ------------------------------------------------------------------------------------------------------------

# Check some predictors
#result <- aggregate(train$count, by=list(train$year), mean)
# Can also use tapply:
barplot(tapply(train$count, train$year, mean), col=c("wheat","cornflowerblue"), main="Train: Mean of count",
        names.arg=unique(train$year), cex.names=.8)

boxplot(count ~ as.factor(atemp), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by average temp")
cor(train$count, train$atemp)
boxplot(count ~ as.factor(temp), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by temp")
cor(train$count, train$temp)
boxplot(count ~ as.factor(humidity), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by humidity")
cor(train$count, train$humidity)
boxplot(count ~ as.factor(weather), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by weather")
boxplot(casual ~ as.factor(weather), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by weather")
boxplot(registered ~ as.factor(weather), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by weather")
cor(train$count, as.integer(train$weather))
boxplot(count ~ as.factor(hour), data=train, col="orange",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by hour")
boxplot(casual ~ as.factor(hour), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by hour")
boxplot(registered ~ as.factor(hour), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by hour")
cor(train$count, as.integer(train$hour))
boxplot(count ~ month, data=train, col="cornflowerblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by month")
cor(train$count, as.integer(train$month))
boxplot(count ~ as.factor(season), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by season")
boxplot(count ~ as.factor(weekday), data=train, col="orangered",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by weekday")
boxplot(casual ~ as.factor(weekday), data=train, col="wheat",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (casual) by weekday")
boxplot(registered ~ as.factor(weekday), data=train, col="powderblue",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand (registered) by weekday")
boxplot(count ~ as.factor(windspeed), data=train, col="orangered",
     cex.axis=.7, cex.lab=.7, cex.main=1, cex.names=.7, las=2, main="Bike demand by windspeed")

boxplot(train$humidity, test$humidity, col=c("red", "green"), main="Train/test humidity")
boxplot(train$atemp, test$atemp, col=c("red", "green"), main="Train/test atemp")
boxplot(train$windspeed, test$windspeed, col=c("red", "green"), main="Train/test windspeed")

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

result <- CreateTrainAndValidationSets(data=train, percent.train=75, random.selection=T)
train.subset <- rbind(result$train, result$train)
validation.subset <- result$validation
dim(train.subset)
dim(validation.subset)

# TODO: Scale scalars
# Include year??

# Try poisson regression
# http://www.ats.ucla.edu/stat/r/dae/poissonreg.htm
validation.subset$year <- as.factor(validation.subset$year)
validation.subset$hour <- as.factor(validation.subset$hour)

fit <- glm(as.integer(count) ~ as.factor(hour) + as.factor(year) + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=quasipoisson())
# Testing log transform on outcome variable
fit.log <- glm(as.integer(log(count)) ~ as.factor(hour) + as.factor(year) + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude, family=poisson())
# Testing log transform of outcome variable with regular lm
fit.lm <- lm(as.integer(log(count+1)) ~ as.factor(hour) + as.factor(year) + atemp + month + weather + humidity + windspeed + weekday,
           data=train.subset, na.action=na.exclude)

summary(fit)
summary(fit.log)
summary(fit.lm)
# NOTE: If residual deviance is larger than DF, consider using quasipoisson family instead. Here it is:
# Residual deviance: 1748.5  on 8117  degrees of freedom
pred <- predict(fit, validation.subset, type="response")
pred[1:50]
pred.log <- predict(fit.log, validation.subset, type="response")
pred.log <- exp(pred.log)
pred.log[1:50]
pred.lm <- predict(fit.lm, validation.subset, type="response")
pred.lm <- exp(pred.lm)
pred.lm[1:50]
validation.subset$count[1:50]
rmse <- MyRMSE(validation.subset$count, pred)
rmsle.log <- MyRMSLE(validation.subset$count, pred.log)
rmsle.lm <- MyRMSLE(validation.subset$count, pred.lm)

par(mfrow=c(1,1))
n <- 30
plot(validation.subset$count[1:n], col="blue", type="o", lwd=2,
     ylim=c(0, max(validation.subset$count[1:n])+20), main="Prediction results", ylab="Prediction", xlab="Observations")
lines(pred.lm[1:n], col="red", type="o")
lines(pred[1:n], col="green4", type="o")
lines(pred.log[1:n], col="gray", type="o")
grid()

# Compare normal and log curves for count. Log more evenly cyclic:
op <- par()
par(mfrow=c(2,1))
par(mar=c(4,4,2,1))
plot(validation.subset$count[1:100], type="l", col="blue", ylab="Count", cex.lab=.8, cex.axis=.8, cex.main=.8,
     main=paste0("y/pred (RMSLE: ", round(rmsle, 3), ")"))
lines(pred[1:100], col="red")
legend("topright", legend=c("y","pred"), col=c("blue","red"), lwd=2, cex=.7)
plot(validation.subset$count[1:100], type="l", col="blue", ylab="Count", cex.lab=.8, cex.axis=.8, cex.main=.8,
     main=paste0("log(y)/pred (RMSLE: ", round(rmsle.log, 3), ")"))
lines(pred.log[1:100], col="red")
legend("topright", legend=c("log(y)","pred"), col=c("blue","red"), lwd=2, cex=.7)
par <- op

# Trying RF for comparison:
train.subset$hour <- as.integer(train.subset$hour)
validation.subset$hour <- as.integer(validation.subset$hour)
train.subset$year <- as.factor(train.subset$year)
validation.subset$year <- as.factor(validation.subset$year)
cols.to.include <- c(13, 7, 15, 5, 8, 9, 14, 16) # NOTE: Year included now
fit.rf <- randomForest(as.integer(train.subset$count) ~ .,
                       train.subset[, cols.to.include], proximity=F, keep.forest=T, ntree=100, nodesize=50) # ntree=150, 80
pred.rf <- predict(fit.rf, validation.subset[, cols.to.include], type="response")
rmsle.rf <- MyRMSLE(validation.subset$count, pred.rf)

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
plot(predict(fc, n.ahead=20, prediction.interval=FALSE, level=0.95))
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
# TODO: Try GBM with various numbers of trees, and average? Try "poisson" distribution for counts
library(gbm)

# TODO: 2 models on registered and casual instead of 1 on count, and combine predictions
fit <- gbm(train$count ~ .
            ,data=train[, -c(1,10,11,12)] # remove [datetime, casual, registered, count] columns. TODO: Change!
            ,var.monotone=NULL
            ,distribution="gaussian" # TODO: Try poisson
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
all.skip.cols <- -c(1,2,3,4,6,10,11,12)
all.skip.cols <- -c(1,6,10,11,12)
all.include.cols <- c(3,5,6,8,9,13,15,16)
# NOTE: Skipping temp since atemp has high cor with temp, same with season (skipped)/month
train$hour <- as.integer(train$hour)
train$weekday <- as.integer(train$weekday)
fit <- glm(as.integer(train$count) ~ ., data=train[, all.skip.cols], family=poisson())
fit <- glm(as.integer(count) ~ as.factor(hour) + atemp + season + weather + humidity + year, data=train,
          na.action=na.exclude, family="poisson")

fit <- glm(train$count ~ ., data=train[, all.include.cols], , family="poisson")
fit <- glm(as.integer(count) ~ as.factor(hour) + atemp + month + weather + humidity + windspeed + weekday + year,
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

submission=data.frame(datetime=datetime.test, count=round(pred))
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
forestTrain1 <- randomForest(train$count ~ ., train[, -c(1,3,6,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2 <- randomForest(train$count ~ ., train[, -c(1,3,6,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3 <- randomForest(train$count ~ ., train[, -c(1,3,6,10,11,12)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
# ntree=200 gives worse results....

forestTrain1.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3.casual <- randomForest(train$casual ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestTrain1.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain3.registered <- randomForest(train$registered ~ ., train[, -c(1,3,6,10,11,12,16)], proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestTrain1.casual <- randomForest(casual ~ hour + year + season + workingday + weather + atemp + humidity + windspeed + month + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.casual <- randomForest(casual ~ hour + year + season + workingday + weather + atemp + humidity + windspeed + month + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestTrain1.registered <- randomForest(registered ~ hour + year + season + workingday + weather + atemp + humidity + windspeed + month + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80
forestTrain2.registered <- randomForest(registered ~ hour + year + season + workingday + weather + atemp + humidity + windspeed + month + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=150) # 150, 80

forestTrain1.casual <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=500, mtry=5) # 150, 80
forestTrain2.casual <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday,
                                    data=train, proximity=F, keep.forest=T, nodesize=50, ntree=500, mtry=5) # 150, 80

forestTrain1.registered <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp,
                                        data=train, proximity=F, keep.forest=T, nodesize=50, ntree=500, mtry=5) # 150, 80
forestTrain2.registered <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp,
                                        data=train, proximity=F, keep.forest=T, nodesize=50, ntree=500, mtry=5) # 150, 80


forestCombined <- combine(forestTrain1, forestTrain2, forestTrain3)
print(forestCombined)
forestCombined$importance
# Nice importance plot:
varImpPlot(forestCombined, cex=.8, col="blue", pch=19, main="RF Importance Plot")

pred <- predict(forestCombined, test, type="response")
pred[1:50]

forestCombined.casual <- combine(forestTrain1.casual, forestTrain2.casual)
forestCombined.registered <- combine(forestTrain1.registered, forestTrain2.registered)

varImpPlot(forestTrain1.casual, cex=.8, col="blue", pch=19, main="RF Importance Plot")
varImpPlot(forestTrain1.registered, cex=.8, col="blue", pch=19, main="RF Importance Plot")

pred.casual <- predict(forestTrain1.casual, test, type="response") 
pred.registered <- predict(forestTrain1.registered, test, type="response") 

pred.combined  <- round(pred.casual + pred.registered,	0) # TODO: Try with and without rounding!
pred.combined  <- pred.casual + pred.registered # TODO: Try with and without rounding!
pred.combined[1:100]
plot(pred.combined, col="blue", main="Registered and casual combined", cex.main=.8, cex.lab=.7, cex.axis=.7)

submission=data.frame(datetime=datetime.test, count=round(pred))
submission=data.frame(datetime=datetime.test, count=pred) # Better not to round?
submission=data.frame(datetime=datetime.test, count=pred.combined) # Better not to round?
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "RF_C_plus_R_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# 1 RF for casual and 1 RF for registered (IMPORTANT: Sum c+r) with 500 trees and mtry = 5 gives best entry: 0.48464

# -------------------------------------------------------------------------------------------------------------------
# Caret package (tip: Test on Sonar dataset)
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf

library(caret)
library(mlbench)

all.skip.cols <- -c(1,2,3,4,5,6,10,11,16)

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

# ------------------------------------------------------------------------------------------------------------------
# Try h2o

package.install("h2o")
suppressMessages(library(h2o))

# TODO: Get cols to use
y.col.casual <- 10
y.col.registered <- 11
x.cols.casual <- c(2,4,5,7,8,13,14,15,16)
x.cols.registered <- c(2,4,5,7,8,13,14,15,16)

localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()

test2 <- test[, c(-10,-11,-12)] # TEST: Remove count, registered and casual, any effect? Reduces count number in preds??

dat_h2o <- as.h2o(localH2O, train, key='train')
dat_h2o.test <- as.h2o(localH2O, test, key='test')

model.casual <-
  h2o.randomForest(x=x.cols.casual, y=y.col.casual, data=dat_h2o, key = "rf", classification = F, ntree = 500, 
                   depth = 20, mtries = -1, sample.rate = 2/3, nbins = 20, seed = -1, 
                   importance = FALSE, score.each.iteration = FALSE, nfolds = 0, 
                   holdout.fraction = 0, nodesize = 1, balance.classes = FALSE, 
                   max.after.balance.size = 5, class.sampling.factors = NULL, 
                   doGrpSplit = TRUE, verbose = FALSE, oobee = TRUE, stat.type = "ENTROPY", 
                   type = "BigData") # NOTE: Needed for rf regression

model.registered <-
  h2o.randomForest(x=x.cols.registered, y=y.col.registered, data=dat_h2o, key = "rf", classification = F, ntree = 500, 
                   depth = 20, mtries = -1, sample.rate = 2/3, nbins = 20, seed = -1, 
                   importance = FALSE, score.each.iteration = FALSE, nfolds = 0, 
                   holdout.fraction = 0, nodesize = 1, balance.classes = FALSE, 
                   max.after.balance.size = 5, class.sampling.factors = NULL, 
                   doGrpSplit = TRUE, verbose = FALSE, oobee = TRUE, stat.type = "ENTROPY", 
                   type = "BigData") # NOTE: Needed for rf regression

model.registered <-
  h2o.glm(x=x.cols.registered, y=y.col.registered, data=dat_h2o, key = "glm",
        offset = NULL, family="poisson",
        prior = NULL, nfolds = 0, alpha = 0.5, lambda = 1e-5,
        lambda_search = FALSE, nlambda = -1, lambda.min.ratio = -1,
        max_predictors = -1, return_all_lambda = FALSE,
        strong_rules = TRUE, standardize = TRUE, intercept = TRUE,
        non_negative = FALSE, use_all_factor_levels = FALSE,
        variable_importances = FALSE, epsilon = 1e-4, iter.max = 100,
        higher_accuracy = FALSE, beta_constraints = NULL, 
        disable_line_search = FALSE)

model.casual <-
  h2o.glm(x=x.cols.casual, y=y.col.casual, data=dat_h2o, key = "glm",
          offset = NULL, family="poisson",
          prior = NULL, nfolds = 0, alpha = 0.5, lambda = 1e-5,
          lambda_search = FALSE, nlambda = -1, lambda.min.ratio = -1,
          max_predictors = -1, return_all_lambda = FALSE,
          strong_rules = TRUE, standardize = TRUE, intercept = TRUE,
          non_negative = FALSE, use_all_factor_levels = FALSE,
          variable_importances = FALSE, epsilon = 1e-4, iter.max = 100,
          higher_accuracy = FALSE, beta_constraints = NULL, 
          disable_line_search = FALSE)

h2o_yhat_test.casual <- h2o.predict(model.casual, dat_h2o.test)
df_yhat_test.casual <- as.data.frame(h2o_yhat_test.casual)
plot(df_yhat_test.casual$predict, col="blue", pch=19)

h2o_yhat_test.registered <- h2o.predict(model.registered, dat_h2o.test)
df_yhat_test.registered <- as.data.frame(h2o_yhat_test.registered)
points(df_yhat_test.registered$predict, col="red")

# NOTE: Add predictions to get count
df_yhat_test.combined <- round(df_yhat_test.casual$predict + df_yhat_test.registered$predict)

submission=data.frame(datetime=datetime.test, count=df_yhat_test.combined)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "h2o_rf_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
