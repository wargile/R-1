# Kaggle Avazu Click-Through Rate Prediction
# Ends: 20 jan 2015

# id: ad identifier
# click: 0/1 for non-click/click
# hour: format is YYMMDDHH, so 14091123 means 23:00 on Sept. 11, 2014
# C1, C17-C24: anonymized variables
# C2-C16: named variables
# 2  banner_pos
# 3  site_id
# 4  site_domain
# 5  site_category
# 6  app_id
# 7  app_domain
# 8  app_category
# 9  device_id
# 10 device_ip
# 11 device_os
# 12 device_make
# 13 device_model
# 14 device_type
# 15 device_conn_type
# 16 device_geo_country

# TODO: Get h2o running again!

# TIPS:
# http://www.kaggle.com/c/avazu-ctr-prediction/forums/t/10783/reading-training-data-in-r/56972#post56972
# https://kaggle2.blob.core.windows.net/forum-message-attachments/56731/1699/fast_solution.py?
#   sv=2012-02-12&se=2014-11-02T05%3A36%3A55Z&sr=b&sp=r&sig=5KQp%2FFwo%2FGBMngdQZGJlL7vLXINiE6TTaLPYmDcNW%2Bo%3D
# http://www.kaggle.com/c/avazu-ctr-prediction/forums/t/10783/reading-training-data-in-r/57026#post57026
# http://www.kaggle.com/c/avazu-ctr-prediction/forums/t/10821/beat-the-benchmark-with-h2o-lb-0-393/57224#post57224

set.seed(16071962)
source("tools.R")

#library(ff) # For huge files
#library(ffbase)

datafolder <- "C:/coding/Kaggle/Avazu_Click-ThroughRatePrediction/Data/"
submissionsfolder <- "C:/coding/Kaggle/Avazu_Click-ThroughRatePrediction/Submissions/"

if (file.exists(paste0(datafolder, "train_out_141001.rda")) == F) {
  train <- fread(paste0(datafolder, "train_out_141001.csv"), header=T, sep=",")
  train <- as.data.frame(train)
  test <- fread(paste0(datafolder, "test.csv"), header=T, sep=",")
  test <- as.data.frame(test)
  
  save(train, file=paste0(datafolder, "train_out_141001.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  load(paste0(datafolder, "train_out_141001.rda"))
  load(paste0(datafolder, "test.rda"))
}

dim(train)
head(train, n=1)
sapply(train, class)
summary(train[1:1000000, ])
# numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))


dim(test)
head(test, n=1)
sapply(test, class)
summary(test[1:1000000, ])

# Create an hour columns with just the hour from the timestamp:
# train$hour <- as.character(train$hour)
train$hour2 <- substr(train$hour, 7,8)
# test$hour2 <- substr(test$hour, 7,8)

# Check some predictors

# TODO: Filter out cols that have no variation?
# lengths <- sapply(train, function(x) length(unique(x)))
# lengths <- which(lengths == 1)

# TODO: Create factor vars where few unique values, convert hex-strings to numeric vals?
train$hour <- as.factor(train$hour)
train$hour2 <- as.factor(train$hour2)
train$device_make <- as.factor(train$device_make)
train$click <- as.factor(train$click)
train$device_conn_type <- as.factor(train$device_conn_type)
train$site_category <- as.factor(train$site_category)
train$device_geo_country <- as.factor(train$device_geo_country)
train$app_category <- as.factor(train$app_category)
train$C1 <- as.factor(train$C1)
train$C2 <- as.factor(train$C2)
train$C24 <- as.factor(train$C24)

unique(train$device_make)
barplot(table(train$device_make), cex.axis=.7, cex.names=.7, cex.main=1,
        main="Device_make", col="orange")
barplot(table(train$click), cex.axis=.7, cex.names=.7, cex.main=1,
        main="Click", col="orange")
barplot(table(train$device_conn_type), cex.axis=.7, cex.names=.7,
        cex.main=1, main="Device_conn_type", col="orange")
barplot(table(train$device_os), cex.axis=.7, cex.names=.7,
        cex.main=1, main="Device_os", col="orange")
barplot(table(train$site_category), cex.axis=.7, cex.names=.7,
        cex.main=1, main="Site_category", las=2, col="orange")

barplot(table(train$device_geo_country), cex.axis=.7, cex.names=.7,
        cex.main=1, main="Device_geo_country", las=2, col="orange")
barplot(log(table(train$device_geo_country) + 1), cex.axis=.7,
        cex.names=.7, cex.main=1, main="Device_geo_country", las=2, col="cornflowerblue") # Do log of same

barplot(table(train$app_category), cex.axis=.7, cex.names=.7,
        cex.main=1, main="App_category", las=2, col="orange")
barplot(table(train$C1), cex.axis=.7, cex.names=.7, cex.main=1,
        main="C1", las=2, col="orange")
barplot(table(train$C2), cex.axis=.7, cex.names=.7, cex.main=1,
        main="C2", las=2, col="orange")
barplot(table(train$C24), cex.axis=.7, cex.names=.7, cex.main=1,
        main="C24", las=2, col="orange") # TODO: Remove \r at end?


# Split training data in training and validation subsets
result <- CreateTrainAndValidationSets(train[1:100000, ],
                                       percent.train=75, random.selection=F)
train.subset <- result$train
validation.subset <- result$validation

# Predict and get RMSE on validation set
# hour2 + device_make + device_conn_type + device_os + site_category + device_geo_country + C1 + C2 + C24

# -------------------------------------------------------------------------------------------------------------------
# Try GLM
fit <- glm(click ~ device_make + device_conn_type + device_os + C1 + C2 + C3 + C4 + C21 + C22 + C23 + C24,
           data=train.subset, family=binomial(link="logit"))
summary(fit)
prediction <- predict(fit, newdata=validation.subset, type="response")
prediction[1:10]
rmse <- MyRMSE(as.numeric(validation.subset$click), as.numeric(prediction))


# Try randomForest
library(randomForest)
result <- integer()
Sys.time()
for (counter in seq(1, 25, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(click ~ device_make + device_conn_type + device_os + C1 + C24,
                               data=train.subset, proximity=TRUE, keep.forest=TRUE, ntree=counter,
                               nodesize=5)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=validation.subset, type="response")
  the.result <- (prediction == validation.subset$click)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()
best.ntrees <- which(result == min(result))


# Try GBM
library(gbm)
# TODO: Filter out cols that have no variation?
# lengths <- sapply(train, function(x) length(unique(x)))
# lengths <- which(lengths == 1)
GBM_NTREES = 50
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 3
GBM_MINOBS = 5
y <- train.subset$click
x <- train.subset[, c("device_make","device_conn_type","device_os","C1","C24")]
x$C24 <- sub('\\r', '', x$C24)
x$C24 <- as.factor(x$C24)
fit <- gbm(click ~ hour2 + device_make + device_conn_type + device_os + C1 + C24, data=train.subset,
           distribution="bernoulli", n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE,
           interaction.depth=GBM_DEPTH, n.minobsinnode=GBM_MINOBS) 
fit <- gbm.fit(x=x, y=y, distribution="bernoulli", n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE,
               interaction.depth=GBM_DEPTH, n.minobsinnode=GBM_MINOBS) 
prediction <- predict(fit, newdata=validation.subset, type="response", n.trees=GBM_NTREES)
prediction[1:10]
rmse <- MyRMSE(as.numeric(validation.subset$click), as.numeric(prediction))

oldmar=par()$mar
par(mar=c(3, 10, 2, 1))
summary(fit, GBM_NTREES, main="GBM variable importance", cex.axis=.8, cex.lab=.8,
        cex.main=1, cex.names=.7, las=1)
par(mar=oldmar)
oldmar=par()$mar
par(mar=c(10, 3.5, 2, 1))
influence <- relative.influence(fit, GBM_NTREES, sort=T)
influence <- influence[influence > 0]
barplot(influence, col="cornflowerblue", las=2, cex.axis=.7, cex.names=.7, cex.main=1, main="GBM variable importance")
par(mar=oldmar)


# Try H2o
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-markov/1http://h2o-release.s3.amazonaws.com/h2o/rel-markov/1/R", getOption("repos"))))
library(h2o)
localH2O = h2o.init()

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)

cols <- c("hour2","device_make","device_conn_type","device_os","C1","C24")
train.subset <- train.subset[, cols]
validation.subset <- validation.subset[, cols]

train_h20 <- as.h2o(localH2O, train.subset, key='train')
test_h20 <- as.h2o(localH2O, validation.subset, key='test')

## Split the dataset into 80:20 for training and validation
train_split <- h2o.splitFrame(train_h20, ratios=0.8, shuffle=F)

# TODO: Implement model
model <- h2o.deeplearning(x = cols,
                          y = 2,
                          data = train_split[[1]],
                          validation = train_split[[2]],
                          activation = "Rectifier",
                          hidden = c(50, 50, 50),
                          epochs = 100,
                          classification = FALSE,
                          balance_classes = FALSE)


# Try SVM
# SVM:
# http://stackoverflow.com/questions/1753299/help-using-predict-for-kernlabs-svm-in-r
library(kernlab)
library(e1071)

# kernel='rbf' for nonlinear SVM
rbf <- rbfdot(sigma=0.1)

#ksvm1 <- ksvm(Label ~ ., data=train, type="C-bsvc", kernel=rbf, C=10, prob.model=TRUE)
cols <- c("hour2","device_make","device_conn_type","device_os","C1","C24")
ksvm1 <-  ksvm(train.subset$click ~., data=scale(train.subset[, cols], center=T, scale=T),
               kernel="rbfdot", kpar=list(sigma=0.015),
               C=70, cross=4, prob.model=TRUE)
ksvm1 <-  ksvm(train.subset$click ~., data=train.subset[, cols],
               kernel="rbfdot", kpar=list(sigma=0.015),
               C=70, cross=4, prob.model=TRUE)
fitted(ksvm1)
#result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="probabilities")
#result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="response")
result <- predict(ksvm1, newdata=validation.subset[, cols], type="response")
#solution <- ifelse(result[,1] > result[,2], 0, 1)
result <- scale(result, center=T, scale=T)
result <- plogis(result)
solution <- ifelse(result < 0.5, 0, 1)


# -------------------------------------------------------------------------------------------------------------------

op <- par()
par(mfrow=c(1,1))
par(mar=c(2.5,4,2,1))
n <- 50 # If plotting subset
y.min <- ifelse(min(validation.subset$y) < min(prediction),
                min(validation.subset$y), min(prediction))
y.max <- ifelse(max(validation.subset$y) > max(prediction),
                max(validation.subset$y), max(prediction))
plot(validation.subset$y, type="o", col="blue", xlab="x", ylab="y/pred",
     main=paste0("y/pred (RMSE: ", round(rmse, 3), ")"), ylim=c(y.min, y.max))
points(prediction, col="red", type="o")
legend("topleft", legend=c("y","pred"), col=c("blue","red"), lwd=2)
par <- op


# Create the submission file
KaggleSubmission(submission, submissionsfolder, "GLM_binom")
