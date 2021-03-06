# Restaurant Revenue Prediction
# Deadline: 04.05.2015
# http://www.kaggle.com/c/restaurant-revenue-prediction

# TASK: "Welcome to the TFI Restaurant Revenue Prediction competition. Your task is to predict the ANNUAL revenue
# of a particular location given a set of datapoints associated with that location."

# TIPS:
# https://www.kaggle.com/c/restaurant-revenue-prediction/forums/t/13011/beat-the-benchmark-with-support-vector-machine-in-r
# https://en.wikipedia.org/wiki/Mu%C4%9Fla
# https://kaggle2.blob.core.windows.net/forum-message-attachments/67762/2221/Rfbenchmark.py?sv=2012-02-12&se=2015-03-26T20%3A27%3A52Z&sr=b&sp=r&sig=uq9sg6rb%2B3w0mo6onPyTkkzoQ3tT%2Fq3gIrz7VLLfHuY%3D


library(randomForest)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html
library(stats) # For using fft() Fast Fourier Transform
library(kernlab)
library(e1071)
library(lubridate)
library(foreach)
library(doParallel)

set.seed(16071962)

# Set some standard graphical params for plot
SetStandardOptions()

dataFolder <- "C:/coding/Kaggle/RestaurantRevenuePrediction/Data/"
submissionsFolder <- "C:/coding/Kaggle/RestaurantRevenuePrediction/Submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=T, encoding="UTF-8")
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=T, encoding="UTF-8")
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

dim(train)
str(train)
summary(train)
pairs(train)
sapply(train, class)
describe(train) # library(psych)
View(train[1:500,])

table(is.na(train))
table(is.na(test))

head(train, n=1)
sapply(train, class)
unique(is.na(train))
table(is.na(train)) # No NA's

CorrelationPlot(train[3:45]) # TODO: Massive correlation! Build a lm model bottom-up and check adj.R2?
# P14-P18 versus P30-P37
# P24-P27 versus P30-P37
# P30-P37 internally too
# But generally VERY correlated overall! So which technique handles correlated predictors well?

# TODO: Look at the distribution of the P1-P37 variables. Ordered categorical values?
hist(train$P1) # Can factor these, but only if test has FEWER levels. Use: as.factor(test.predictor, levels=train.predictor)
table(train$P1)
table(test$P1)
hist(train$P2) # Scale them?
table(train$P2)
hist(train$P5)
table(train$P10)
hist(train$P10)
hist(train$P20)
hist(train$P30)
# TODO: Log transform outcome var Revenue and all P<n> predictors!
hist(train$revenue)
hist(log(train$revenue))

# Plot the revenue based on the City.Group
par(mfrow=c(2,1))
hist(train$revenue[which(train$City.Group == "Big Cities")], col="wheat3", main="Revenue given 'Big Cities'")
hist(train$revenue[train$City.Group == "Other"], col="powderblue", main="Revenue given 'Other'")
par(mfrow=c(1,1))

par(mar=c(5,3,2,1))
plot(revenue ~ City, data=train, col="wheat", las=2, main="Revenue by city", xlab=NA, ylab=NA)
par(mar=c(3,3,2,1)) # Istanbul bad outlier on revenue!
plot(sort(train$revenue), type="o", main="Revenue", col="blue")

# TODO: If this is ACCUMULATED revenue, it is possible to create a new predictor for AVERAGE revenue (or a ratio),
# based on Open.Date?? And then add that info to the test set by looking at same city/type, etc.?
dates <- as.POSIXlt(train$Open.Date, format="%m/%d/%Y")
cur.date <- cur.date <- as.POSIXlt(format(Sys.time(), "%m/%d/%Y"), format="%m/%d/%Y")
train$Days.Open <- as.numeric(cur.date - dates) # NOTE: difftime class returned from subtraction
dates <- as.POSIXlt(test$Open.Date, format="%m/%d/%Y")
cur.date <- cur.date <- as.POSIXlt(format(Sys.time(), "%m/%d/%Y"), format="%m/%d/%Y")
test$Days.Open <- as.numeric(cur.date - dates) # NOTE: difftime class returned from subtraction
par(mfrow=c(2,1))
hist(train$Days.Open, col="cornflowerblue", main="Days.Open distribution, training set")
hist(test$Days.Open, col="cornflowerblue", main="Days.Open distribution, test set")
par(mfrow=c(1,1))

# TODO: Is there some info to be gotten from the Open.Date and Year/Month versus Revenue?
# Create a cumulative revenue col? See description page and:
# https://www.kaggle.com/c/restaurant-revenue-prediction/forums/t/13021/revenue-cumulative

# TODO: Convert OpenDate to Date? FIX mm/dd/yyyy format? as.POSIX?
train$Open.Date <- as.character(train$Open.Date)
test$Open.Date <- as.character(test$Open.Date)

# Extract year, month and day from dates:
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
test$day<-as.factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$month<-as.factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$year<-as.factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))

# Convert categorical features that have more levels in test to integer:
train$Type <- as.numeric(train$Type)
test$Type <- as.numeric(test$Type)
train$City <- as.numeric(train$City)
test$City <- as.numeric(test$City)
# TODO: Can do simpler: train_cols <- data.frame(lapply(train_cols,as.numeric))

#Log Transform P Variables and Revenue
train[, paste("P", 1:37, sep="")] <- log(1 + train[, paste("P", 1:37, sep="")])
test[, paste("P", 1:37, sep="")] <- log(1 + test[, paste("P", 1:37, sep="")])
train$revenue <- (1 + log(train$revenue))

names(train)
names(test)
train.rows <- c(3:46)
test.rows <- c(3:45)

train_cols <- train[,c(3:42,44:47)]
labels <- as.matrix(train[,43])
testdata <- test[,3:46]
train_cols <- data.frame(lapply(train_cols, as.numeric)) # NOTE: all are converted to numeric here!
testdata <- data.frame(lapply(testdata, as.numeric))
train_cols <- cbind(train_cols, labels)
sapply(train_cols, class)
sapply(testdata, class)


# Split in train and validation:
# TODO: duplicate train set several times, to create a larger train set? Bootstrap?
#which(labels > 1.3e+07)
# Removing outliers in such a small training set, seems to worsen prediction
#result <- CreateTrainAndValidationSets(train_cols[-which(labels > 1.3e+07), ])

test$City <- as.factor(test$City)

train2 <- train
train2$revenue <- log(train2$revenue + 1)
train2$Days.open <- log(train2$Days.Open + 1)
train2$City <- factor(train2$City, levels=levels(test$City))

result <- CreateTrainAndValidationSets(train2)
train.subset <- result[[1]]
validation.subset <- result[[2]]
train.subset$City <- factor(train.subset$City, levels=levels(validation.subset$City))

# Try to duplicate train set:
# http://stackoverflow.com/questions/8753531/repeat-data-frame-n-times
n <- 15
#train.subset2 <- train.subset[rep(seq_len(nrow(train.subset)), n), ]
train.subset2 <- do.call("rbind", replicate(n, train.subset, simplify = FALSE))

# Try in full train set:
model.glm <- lm(revenue ~ year + Type + City + City.Group + Days.Open, data=train.subset)
summary(model.glm)
model.rf <- randomForest(log(labels) ~ ., data=train_cols, ntrees=50, importance=T)
model.lm <- lm(labels ~ ., data=train_cols)
model.svm <- svm(y=labels, x=train_cols, cost=10, scale=T, type="eps-regression")

p <- predict(model.glm, newdata=validation.subset)
p <- predict(model.rf, newdata=testdata)
p <- predict(model.lm, newdata=testdata)
p <- predict(model.svm, newdata=testdata)

#------------------------------------------------------------------------------------------------------------------
# Try train and validation subsets:

# NOTE: If you log-transform revenue, you have to do exp(predict(...)) !!!
# ------------------------------------------------------------------------

model.rf <- randomForest(labels ~ ., data=train.subset, ntrees=250, importance=T)
varImpPlot(model.rf, cex=.7, col="blue", pch=16)
model.glm <- glm(labels ~ ., data=train.subset, family="poisson")
model.lm <- lm(labels ~ ., data=train.subset)
summary(model.lm)
model.svm <- svm(labels ~ ., data=train.subset, cost=10, scale=T, type="eps-regression")

library(caret)
library(Boruta)
important <- Boruta(labels ~ ., data=train.subset)
#Caret Random Forest
model.caret <- train(labels ~ ., data=train.subset[, c(important$finalDecision != "Rejected", TRUE)])

p <- predict(model.caret, newdata=validation.subset)
p <- predict(model.rf, newdata=validation.subset, type="response")) # Using log on outcome
p <- predict(model.glm, newdata=validation.subset, type="response")
p <- predict(model.lm, newdata=validation.subset, type="response")
p <- predict(model.svm, newdata=validation.subset, type="response")

if (min(p) < min(validation.subset$labels)) min.p <- min(p) else min.p <- min(validation.subset$labels)
if (max(p) > max(validation.subset$labels)) max.p <- max(p) else max.p <- max(validation.subset$labels)
    
plot(p, type="o", col="blue", main="Prediction result", ylim=c(min.p, max.p))
lines(validation.subset$labels, col="red", type="o")
p[1:10]
hist(p, col="wheat")

score <- abs(validation.subset$labels - p) / validation.subset$labels
score
score <- sum(abs(validation.subset$labels - p))
score
if (max(p) > max(validation.subset$labels)) max.y <- max(p) else max.y <- max(validation.subset$labels)
if (min(p) < min(validation.subset$labels)) min.y <- min(p) else min.y <- min(validation.subset$labels)
plot(validation.subset$labels, type="o", col="blue", ylim=c(min.y, max.y), main="Pred ~ y", ylab="Revenue")
lines(p, col="red", type="o")

# TODO: Do a hist on predictions for svm as well as randomforest
hist(p)

submission <- data.frame(Id=0:(nrow(test)-1), Prediction=p)
head(submission)
# ---------------------------------------------------------------------------------------------------
#save(submission.gbm, file=paste0(submissionsFolder, "submission.rda"))
KaggleSubmission(submission, submissionsFolder, "SVM")   

# ---------------------------------------------------------------------------------------------------------------------------

# NOT MINE!

# Restaurant Revenue Prediction

package.install("Boruta")
library(Boruta)
library(caret)

train <- read.csv(paste0(dataFolder, "data/train.csv"))
test  <- read.csv(paste0(dataFolder, "data/test.csv"))

n.train <- nrow(train)

test$revenue <- 1
myData <- rbind(train, test)
rm(train, test)

#Tranform Time
myData$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(myData$Open.Date, format="%m/%d/%Y")
myData$Open.Date <- as.numeric(myData$Open.Date / 1000) #Scale for factors
myData$Open.Date[1:10]

#Consolidate Cities
myData$City                                      <- as.character(myData$City)
myData$City[myData$City.Group == "Other"]        <- "Other"
myData$City[myData$City == unique(myData$City)[4]] <- unique(myData$City)[2]
myData$City                                      <- as.factor(myData$City)
myData$City.Group                                <- NULL

#Consolidate Types
myData$Type <- as.character(myData$Type)
myData$Type[myData$Type=="DT"] <- "IL"
myData$Type[myData$Type=="MB"] <- "FC"
myData$Type <- as.factor(myData$Type)

#Log Transform P Variables and Revenue
myData[, paste("P", 1:37, sep="")] <- log(1 + myData[, paste("P", 1:37, sep="")])
myData$revenue <- log(myData$revenue)
min(myData$revenue)
max(myData$revenue)

important <- Boruta(revenue~., data=myData[1:n.train, ])
important

#Random Forest
model <- train(revenue~., 
               data=myData[1:n.train, c(important$finalDecision != "Rejected", TRUE)])

#Make a Prediction
prediction <- predict(model, myData[-c(1:n.train), ])
min(prediction)
max(prediction)

#Make Submission
submit <- as.data.frame(cbind(seq(0, length(prediction) - 1, by=1), exp(prediction)))
colnames(submit) <- c("Id","Prediction")
head(submit)
min(submit$Prediction)
max(submit$Prediction)
write.csv(submit, paste0(submissionsFolder, "submission.csv"), row.names=FALSE, quote=FALSE)
