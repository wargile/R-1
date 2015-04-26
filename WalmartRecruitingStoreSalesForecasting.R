# Walmart Recruiting - Store Sales Forecasting
# --------------------------------------------
# Use historical markdown data to predict store sales
# Deadline: Monday May 5 2014

# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/data

# MODEL TIPS:
# y <- ts(rnorm(120,0,3) + 1:120 + 20*sin(2*pi*(1:120)/12), frequency=12)
# fit <- tslm(y ~ trend + season)
# plot(forecast(fit, h=20))
# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8033/simple-models-and-straight-averages
# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8023/thank-you-and-2-rank-model
# http://ideone.com/pUw773
# https://github.com/mikeskim/Walmart/blob/master/makeSubmission.R
# https://bitbucket.org/dthal/kaggle_walmart (Winner)
# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8028/a-key-adjustment (for above)

# TIPS:
# http://faculty.washington.edu/ezivot/econ584/notes/varModels.pdf
# http://stackoverflow.com/questions/1714280/multivariate-time-series-modelling-in-r
# http://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf
# http://www.r-bloggers.com/forecasting-weekly-data/
# 2-STAGE REGRESSION?

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/WalmartRecruitingStoreSalesForecasting/data/"
codeFolder <- "C:/coding/Kaggle/WalmartRecruitingStoreSalesForecasting/code/"
submissionsFolder <- "C:/coding/Kaggle/WalmartRecruitingStoreSalesForecasting/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  features <- read.csv(paste0(dataFolder, "features.csv"), header=T, sep=",")
  stores <- read.csv(paste0(dataFolder, "stores.csv"), header=T, sep=",")
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
  save(features, file=paste0(dataFolder, "features.rda"))
  save(stores, file=paste0(dataFolder, "stores.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "features.rda"))
  load(paste0(dataFolder, "stores.rda"))
}

head(train)
head(test)
head(stores)
head(features)
dim(train)
dim(test)
sapply(train, class)

# Look at missing values:
package.install("Amelia")
library(Amelia)
par(mfrow=c(1,2))
missmap(train[1:500, ], main = "Missingness Map Train", col = c("wheat", "cornflowerblue"))
missmap(test[1:500, ], main = "Missingness Map Test", col = c("wheat", "blue"))
par(mfrow=c(1,1))

sum(!complete.cases(train))
sum(!complete.cases(test))

library(e1071)
impute(train, what="median")
impute(test, what="median")

sum(!complete.cases(train))
sum(!complete.cases(test))

# Do some plotting:
# Look at store types and their size (NOTE: Store type A and B has weird low outliers):
plot(Size ~ Type, data=stores, main="Stores by Type", col="cornflowerblue", cex.axis=.8)
# TODO: There are a couple of weird outliers (size) in store type. Convert these to store type 'C' (smallest)
# Not sure if this is an error in the dataset...
stores[stores$Size < 50000 & stores$Type %in% c('A', 'B'), ]$Type <- 'C'
plot(Size ~ Type, data=stores, main="Stores by Type", col="cornflowerblue", cex.axis=.8)

# Date need to be fixed in both train and test sets!
train$DateConv <- as.integer(paste0(substring(train$Date, 1, 4), substring(train$Date, 6, 7),
                                    substring(train$Date, 9, 10)))
test$DateConv <- as.integer(paste0(substring(test$Date, 1, 4), substring(test$Date, 6, 7),
                                    substring(test$Date, 9, 10)))

merged.store.train <- merge(train, stores, all.x=T, by.x="Store")
merged.store.test <- merge(test, stores, all.x=T, by.x="Store")

# Look at sale for a certain type of store over time (looks like distinct seasonality)
# Use seasonal exponential smoothing? https://www.otexts.org/fpp/7/
sort(unique(merged.store.train$Dept)) # Almost 100 Dept's!

# What date ranges do we have?
min(merged.store.train$DateConv)
max(merged.store.train$DateConv)
min(merged.store.test$DateConv)
max(merged.store.test$DateConv)

# NOTE: Some stores have negative weekly sales. Convert to zero or abs(weekly_sales)?
train[train$Weekly_Sales < 0, ]
dim(train[train$Weekly_Sales < 0, ])
train$Weekly_Sales[train$Weekly_Sales < 0] <- 0

# Plot the seasonality:
# NOTE: Some dept's have just seasonal variations, but some have a non-seasonal, but clear down/up trend!
#       How to differentiate automatically? Would need a log decay or similar for some stores?
sales.agg <- aggregate(Weekly_Sales ~ Date, data=merged.store.train, mean) # Get average for all stores/depts
par(mfrow=c(2,1))
oldmar=par()$mar
par(mar=c(2.5,4.2,2,1))
# Plot for a particular store and dept for the period:
dept <- 45
sales <- merged.store.train[merged.store.train$Store == 8 & merged.store.train$Dept == dept, ]
sales.agg <- aggregate(Weekly_Sales ~ Date,
                       data=merged.store.train[merged.store.train$Dept == dept, ], mean) # Get average for all stores/depts

# Plot for a particular dept for the period:
# sales <- aggregate(Weekly_Sales ~ Date, data=merged.store.train[merged.store.train$Dept == 44, ], mean)
# Get the max sale day:
sales[which.max(sales$Weekly_Sales), ]
plot(sales.agg$Weekly_Sales ~ as.Date(sales.agg$Date), type="l", lwd=2, col="cornflowerblue", las=1, cex.axis=.7,
     main="Sales by Date, all dept's", ylab="Weekly Sales", xlab="")
plot(sales$Weekly_Sales ~ as.Date(sales$Date), type="l", lwd=2, col="orange", las=1, cex.axis=.7,
     main="Sales by Date, one store/dept", ylab="Weekly Sales", xlab="")
par(mfrow=c(1,1))
par(mar=oldmar)

# http://stackoverflow.com/questions/1714280/multivariate-time-series-modelling-in-r
# Could a Multivariate ARIMA Model work for using more IV's in the prediction?
storeDepts.train <- list(stores=vector(), depts=list())
pos <- 0
for (counter in sort(unique(train$Store))) {
  pos <- pos + 1
  storeDepts.train$stores[pos] <- counter
  storeDepts.train$depts[[pos]] <- sort(unique(train[train$Store == counter, ]$Dept))
}

storeDepts.test <- list(stores=vector(), depts=list())
pos <- 0
for (counter in sort(unique(test$Store))) {
  pos <- pos + 1
  storeDepts.test$stores[pos] <- counter
  storeDepts.test$depts[[pos]] <- sort(unique(test[test$Store == counter, ]$Dept))
}

# TIP about forecasting weekly data:
# http://www.r-bloggers.com/forecasting-weekly-data/

sales$DateConv <- as.integer(paste0(substring(sales$Date, 1, 4), substring(sales$Date, 6, 7),
                                    substring(sales$Date, 9, 10)))
sales$POSIXDate <- as.POSIXlt(sales$Date)
# Example: Find the next date match:
sales[sales$POSIXDate == min(sales$POSIXDate)+(3600*24)*21, ]
# TEST: Create a new df with the missing weeks in the train set range:
# Looks like all dates are a Friday.
start <- min(sales$POSIXDate)
newDates <- as.Date(100)
counter = 1
nextDate <- start

while (1) {
  if (length(sales$POSIXDate[sales$POSIXDate == nextDate]) > 0) { # We have a new date
    newDates[counter] <- as.Date(sales$POSIXDate[sales$POSIXDate == nextDate])
    counter <- counter + 1
  } else {
    newDates[counter] <- as.Date(nextDate)
    counter <- counter + 1
  }
  
  nextDate <- nextDate + (3600 * 24) * 7

  if (nextDate > max(sales$POSIXDate)) {
    break
  }
}

# Find the start and end date's week numbers:
c(min(as.Date(train$Date)), max(as.Date(train$Date)))
c(min(as.Date(test$Date)), max(as.Date(test$Date)))
dateRange <- c(min(as.Date(sales$Date)), max(as.Date(sales$Date)))
x <- as.POSIXlt(dateRange)
weeks <- strftime(x, format="%W") 
# Create the time series:
my.ts <- ts(data=sales$Weekly_Sales, start=c(2010, as.integer(weeks[1])), end=c(2012, as.integer(weeks[2])),
            frequency=52) # Can f=52 work??
# TODO: Find a store with a declining and/or non-seasonal trend, and see how prediction/forecast works
fit <- forecast(my.ts)
plot(fit, type="l", col="blue", lwd=2) # NICE ONE!
plot(fit$model, col="blue")
plot(fit$mean, col="blue") # Forecasted values are: fit$mean
# Create a new time series based on the fit, starting 1 week after the end time of the time series we're predicting:
my.ts.fit <- ts(fit$mean, start=c(2012, as.integer(weeks[2]) + 1), frequency=52)
# Find the corresponding test rows:
sales.test <- merged.store.test[merged.store.test$Store == 8 & merged.store.test$Dept == 46, ]
sales.test$Weekly_Sales_Predicted <- NA
sales.test$Weekly_Sales_Predicted <- as.integer(my.ts.fit[1:dim(sales.test)[1]])
head(sales.test, n=2)
tail(sales.test, n=2)

# Try Holt-Winters on this ts:
plot(forecast(HoltWinters(my.ts)))

# TODO for code block above: Go through all the records in the test set, get the store and dept, and find the
# corresponding rows in train set. Estimate weekly_sales for this store/dept combo, and add to test
# set like in example above.


# In the forecast package, try arima(df[,1:4], order=(0,0,0), xreg=df[,6:8]) for forecasting u, cci, gdp.
# To predict dx from that, try the VAR model. Here's a good tutorial:
# http://faculty.washington.edu/ezivot/econ584/notes/varModels.pdf
fit <- arima(my.ts, xreg=sales[,5])
plot(forecast(fit, h=50))


# Plot average sales for all stores for the period:
# Get the max sale day:
sales.agg[which.max(sales.agg$Weekly_Sales), ]
plot(sales.agg$Weekly_Sales ~ as.Date(sales.agg$Date), type="l", lwd=2, col="cornflowerblue",
     las=1, cex.axis=.7, main="Sales by Date, all stores/dept's", ylab="Average Weekly Sales", xlab="")
grid()
par(mfrow=c(1,1))
par(mar=oldmar)

# TODO: The only col's that can be merged from feature set is maybe Markdown<n>, they *might* reccur.
# All other col's are just historical data that would change in the test period!
merged.store.features.train <- merge(merged.store.train, features[, c(1,2,5,6,7,8,9)],
                                     all.x=T, by.x=c("Store", "Date"))
merged.store.features.test <- merge(merged.store.test, features[, c(1,2,5,6,7,8,9)],
                                     all.x=T, by.x=c("Store", "Date"))

# Fix MarkDown NA's:
merged.store.features.train$MarkDown1[is.na(merged.store.features.train$MarkDown1)] <- 0
merged.store.features.train$MarkDown2[is.na(merged.store.features.train$MarkDown2)] <- 0
merged.store.features.train$MarkDown3[is.na(merged.store.features.train$MarkDown3)] <- 0
merged.store.features.train$MarkDown4[is.na(merged.store.features.train$MarkDown4)] <- 0
merged.store.features.train$MarkDown5[is.na(merged.store.features.train$MarkDown5)] <- 0

merged.store.features.test$MarkDown1[is.na(merged.store.features.test$MarkDown1)] <- 0
merged.store.features.test$MarkDown2[is.na(merged.store.features.test$MarkDown2)] <- 0
merged.store.features.test$MarkDown3[is.na(merged.store.features.test$MarkDown3)] <- 0
merged.store.features.test$MarkDown4[is.na(merged.store.features.test$MarkDown4)] <- 0
merged.store.features.test$MarkDown5[is.na(merged.store.features.test$MarkDown5)] <- 0

unique(is.na(merged.store.features.train))
unique(is.na(merged.store.features.test))

# Check that some info is equal between train and test (Dept and Store seems to be the same!):
par(mfrow=c(2,1))
plot(sort(unique(merged.store.features.train$Store)))
lines(sort(unique(merged.store.features.test$Store)))
plot(sort(unique(merged.store.features.train$Dept)))
lines(sort(unique(merged.store.features.test$Dept)))
par(mfrow=c(1,1))


# ----------------------------------------------------------------------------------------------------------------
# EXAMPLE: Prediction Function for seasonal Fitted Holt-Winters Models
# http://stat.ethz.ch/R-manual/R-patched/library/stats/html/HoltWinters.html
require(graphics)
require(stats)

# Try to forecast on sales.agg:
sales.agg.ts <- sales.agg
sales.agg.ts$Date <- as.Date(sales.agg.ts$Date)
head(sales.agg.ts)
timeseries <- ts(sales.agg.ts, frequency=52)
#timeseries <- zoo(sales.agg.ts)
head(timeseries)

xs <- xts(sales.agg.ts$Weekly_Sales, order.by=sales.agg.ts$Date)
plot(xs)
xs.ts <- as.ts(xs)

xs.ets <- ets(xs, model="MAM")
xs.fore <- forecast(xs.ets, h=12)
plot(xs.fore)

# http://stats.stackexchange.com/questions/9385/
# forecasting-beyond-one-season-using-holt-winters-exponential-smoothing?rq=1
fit <- HoltWinters(timeseries)
summary(fit)
fit <- ets(timeseries)
summary(fit)
fit <- stl(xs.ts)
summary(fit)
fit <- auto.arima(xs.ts, seasonal=T, stationary=T)
summary(fit)

library(forecast)
accuracy(fit)

library(forecast)
forecast(fit, 52)
plot(forecast(fit, 52))

result <- forecast(xs.ts)
plot(result, col="blue", lwd=2)


p <- predict(fit, n.ahead=52, prediction.interval=T)
# ?predict.HoltWinters
plot(m, p)

# ----------------------------------------------------------------------------------------------------------------
# Do lm
fit1 <- lm(Weekly_Sales ~ DateConv + Store + Dept + IsHoliday, data=train)
summary(fit1)
pred1 <- predict(fit1, newdata=test, type="response")
pred1[1:20]

fit2 <- lm(Weekly_Sales ~ Store + Dept + Size + Type + IsHoliday, data=merged.store.train)
summary(fit2)
pred2 <- predict(fit2, newdata=merged.store.test, type="response")
pred2[1:20]

fit3 <- lm(Weekly_Sales ~ Store + Dept + Size + Type + MarkDown3 + MarkDown5,
           data=merged.store.features.train)
summary(fit3)
pred3 <- predict(fit3, newdata=merged.store.features.test[, c(-2,-4,-5)], type="response")
pred3[1:20]

fit4 <- lm(Weekly_Sales ~ DateConv + Store + Dept + Size + Type + IsHoliday + Fuel_Price,
           data=merged.store.features.train)
summary(fit4)
pred4 <- predict(fit4, newdata=merged.store.features.test, type="response")
pred4[1:20]


# ------------------------------------------------------------------------------------------------------
# Turn a weekly time range into a daily time series
IncreaseDate <- function(dateFrom, dateTo) {
  dateDiff <- as.Date(dateTo) - as.Date(dateFrom)
  datesArray <- character()
  
  for (pos in 0:(as.integer(dateDiff))) {
    datesArray[pos + 1] <- as.character(as.Date(dateFrom) + pos)
  }
  
  datesArray
}

theDates <- IncreaseDate("2012-01-24", "2012-03-24")
theDates

# ------------------------------------------------------------------------------------------------------
# Can we change the dates in the test set to be the same period as the train set? And then be able to
# include date in the linear regression model?

# dateDiff <- as.Date(min(as.character(test$Date))) - as.Date(min(as.character(train$Date))) 
dateDiff <- as.Date(min(as.character(test$Date))) - 366 
dateDiff
as.Date(min(as.character(test$Date)))
as.Date(min(as.character(test$Date))) - dateDiff
as.Date(min(as.character(train$Date)))

# Set dates back to train dates in test set:
dateChange <- as.character(as.Date(min(as.character(test$Date))) - dateDiff)
dateChange
# Create a new column for these "set back" dates:
train$BackDate <- NA
test$BackDate <- NA
train$DateConv <- NA
test$DateConv <- NA

train$BackDate <- train$Date
test$BackDate <- as.character(as.Date(as.character(test$Date)) - 364)

train$DateConv <- as.integer(paste0(substring(train$BackDate, 1, 4), substring(train$BackDate, 6, 7),
                                    substring(train$BackDate, 9, 10)))
test$DateConv <- as.integer(paste0(substring(test$BackDate, 1, 4), substring(test$BackDate, 6, 7),
                                   substring(test$BackDate, 9, 10)))

test$Weekly_Sales <- NA
# Loop through all test rows and get train weekly sales
for (counter in 1:nrow(test)) {
#for (counter in 1:1000) {
  date <- test$BackDate[counter]
  dept <- test$Dept[counter]
  store <- test$Store[counter]
  sales <- train[(train$Date == date & train$Store == store & train$Dept == dept), ]$Weekly_Sales
  
  if (length(sales) > 0)
    test$Weekly_Sales[counter] <- sales
  else
    test$Weekly_Sales[counter] <- 0 # OR: = test$Weekly_Sales[counter - 1]
}

# Save the back dated test set for later use:
testBackDated <- test
save(testBackDated, file=paste0(dataFolder, "testBackDated.rda"))

# NOTE: Need to convert date to integer before including it in lm!
# NOTE: Need to keep original test dates for submission file!
fit1 <- lm(Weekly_Sales ~ DateConv + Store + Dept + IsHoliday, data=train)
summary(fit1)
pred1 <- predict(fit1, newdata=test, type="response")
pred1[1:20]

# ------------------------------------------------------------------------------------------------------
# Submission

submission1 <- as.data.frame(cbind(paste0(test[, 1], "_", test[, 2], "_", test[, 3]), round(pred4, 2)))
colnames(submission1) <- c("Id", "Weekly_Sales")
submission1[1:10,]
unique(is.na(submission1))
submission2 <- as.data.frame(cbind(paste0(test[, 1], "_", test[, 2], "_", test[, 3]), test[, 7]))
colnames(submission2) <- c("Id", "Weekly_Sales")
submission2[1:10,]
unique(is.na(submission2))

# Best: submission3, score: 19520.77955
write.csv(x=as.data.frame(submission1), file=paste0(submissionsFolder, "submission8.csv"),
          quote=FALSE, row.names=FALSE)
write.csv(x=as.data.frame(submission2), file=paste0(submissionsFolder, "submissionBackDated1.csv"),
          quote=FALSE, row.names=FALSE)

