# The Analytics Edge
# https://www.kaggle.com/c/the-analytics-edge-mit-15-071x
# Deadline: 05.05.2014

# Model tips:
# http://www.kaggle.com/c/the-analytics-edge-mit-15-071x/forums/t/8030/refreshing
# http://www.kaggle.com/c/the-analytics-edge-mit-15-071x/forums/t/8054/request-to-share-your-code-to-all-the-top-20s-on-leaderboard
# https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/forums/t/8021/final-standings-are-in
# https://www.kaggle.com/blobs/download/forum-message-attachment-files/1242/model.ipynb
# http://rpubs.com/pronojitsaha/showofhands-kaggle
# Model tips:
# Scaling all variables to [-1,0,1] and doing gbm? (0 = NA)
# Main models I use are glm/SVM/glmnet with variable selection (based on importance from random forest model)
# Transforming YOB to a quantitative age variable
# Imputing with MICE package
# Transform question responses to be quantitative (-1,0,1). Good for Logistic regression
# Logistic ridge regression (glmnet) seems to do good here
# https://www.kaggle.com/blobs/download/forum-message-attachment-files/1264/A-Edge%20Kaggle%20Response.xlsx

set.seed(16071962)

# ------------------------------------------------------------------------------------------------------------------
# Function def's:
library(ROCR)

GetAUC = function(model, dataset, depvar) {
  result = as.numeric(performance(prediction(predict(model, type="response", newdata=dataset), depvar), "auc")@y.values)
  return(result)
}

# ------------------------------------------------------------------------------------------------------------------

# memory.limit()
folder <- "C:/coding/Kaggle/TheAnalyticsEdge/code"
datafolder <- "C:/coding/Kaggle/TheAnalyticsEdge/Data/"
submissionfolder <- "C:/coding/Kaggle/TheAnalyticsEdge/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
  test <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
  save(train, file=paste0(datafolder, "train.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  load(paste0(datafolder, "train.rda"))
  load(paste0(datafolder, "test.rda"))
  load(paste0(datafolder, "trainCleaned.rda"))
  load(paste0(datafolder, "testCleaned.rda"))
}

head(train, n=1)

# TODO:
# Get rid of UserID, not important
# Remove votes
# Age group bracketing of YOB column, can improve prediction?
# Check that the factors are consistent, i.e. that there aren't any "free text"/spelling mistakes
# Convert income levels to numeric ranges?

sapply(train, class)

# TODO: Code below just needs to be done of train/testCleaned are not loaded

# Fix train set
for (rowcounter in 1:nrow(train)) {
  for (colcounter in 1:ncol(train)) {
    # check for char/factor col?
    if (class(train[, colcounter]) == "character") {
      if (is.na(train[rowcounter, colcounter]) || train[rowcounter, colcounter] == "") {
        train[rowcounter, colcounter] <- "Unknown"
      }
    }
    if (class(train[, colcounter]) == "integer") {
      if (is.na(train[rowcounter, colcounter]) || train[rowcounter, colcounter] == "") {
        train[rowcounter, colcounter] <- mean(train[, colcounter], na.rm=T)
      }
    }
  }
}

for (colcounter in 1:ncol(train)) {
  # check for char/factor col?
  if (class(train[, colcounter]) == "character") {
    train[, colcounter] <- as.integer(as.factor(train[, colcounter]))
  }
}

# Fix test set
for (rowcounter in 1:nrow(test)) {
  for (colcounter in 1:ncol(test)) {
    # check for char/factor col?
    if (class(test[, colcounter]) == "character") {
      if (is.na(test[rowcounter, colcounter]) || test[rowcounter, colcounter] == "") {
        test[rowcounter, colcounter] <- "Unknown"
      }
    }
    if (class(test[, colcounter]) == "integer") {
      if (is.na(test[rowcounter, colcounter]) || test[rowcounter, colcounter] == "") {
        test[rowcounter, colcounter] <- mean(test[, colcounter], na.rm=T)
      }
    }
  }
}

for (colcounter in 1:ncol(test)) {
  # check for char/factor col?
  if (class(test[, colcounter]) == "character") {
    test[, colcounter] <- as.integer(as.factor(test[, colcounter]))
  }
}

# Fix NA's in YOB:
train$YOB[is.na(train$YOB)] <- mean(train$YOB, na.rm=T)
test$YOB[is.na(test$YOB)] <- mean(test$YOB, na.rm=T)
# Fix YOB's later than Year.Now. Just not possible...
train$YOB[train$YOB > 2013] <- mean(train$YOB, na.rm=T)
test$YOB[test$YOB > 2013] <- mean(test$YOB, na.rm=T)


# Create age brackets (use cut or ave). NOT SURE IF THIS IS GOOD....
train$AgeBracket <- NA
test$AgeBracket <- NA
#train$AgeBracket <- cut(train$YOB, breaks=c(1900,1920,1940,1960,1980,2000))
train$AgeBracket <- ((train$YOB - 1900) %/% 10) * 10
#test$AgeBracket <- cut(test$YOB, breaks=c(1900,1920,1940,1960,1980,2000))
test$AgeBracket <- ((test$YOB - 1900) %/% 10) * 10
View(train[, c(1:3,106:111)])

testCleaned <- test
trainCleaned <- train

# Save the cleaned sets:
save(trainCleaned, file=paste0(datafolder, "trainCleaned.rda"))
save(testCleaned, file=paste0(datafolder, "testCleaned.rda"))

# Keep userId's from test set for submission:
submission.userId <- test$UserID
# Get rid of some col's (UserID, votes:
trainCleaned2 <- trainCleaned[, c(-1, -110)] # c(-1, -2, -110), keeping YOB for now
testCleaned2 <- testCleaned[, c(-1, -109)]

# Look at missing values:
package.install("Amelia")
library(Amelia)
par(mfrow=c(1,2))
missmap(trainCleaned2, main = "Missingness Map Train")
missmap(testCleaned2, main = "Missingness Map Test")
par(mfrow=c(1,1))

# ---------------------------------------------------------------------------------------------------------------------
# Do Tree

library(tree)
# Do a Classification tree:
trainTree <- tree(Happy ~ ., trainCleaned2)
plot(trainTree)
text(trainTree, cex=.8)
summary(trainTree)

# Do CART tree:
library(rpart)
# Do a Classification tree:
library(caret)
library(e1071)
# First find best cp param (cp with lowest MSE):
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Happy ~ ., data=trainCleaned2,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
# Then use the cp value with lowest MSE:
trainCART <- rpart(Happy ~ ., trainCleaned2, control=rpart.control(xval=10, minbucket=2, cp=0.01))
summary(trainCART)
prp(trainCART, cex=.8, col="blue")
plot(trainCART)
text(trainCART, cex=.8)
summary(trainCART)
pred <- predict(trainCART) # Predict on training set
result <- table(trainCleaned2$Happy, pred > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy # Very high accuracy.... overfitting?

# ---------------------------------------------------------------------------------------------------------------------
# Do Random forest
library(randomForest)

# Partition trainCleaned2 in train and test subsets:
trainSubset <- trainCleaned2[1:(nrow(trainCleaned2)/2), ]
testSubset <- trainCleaned2[(nrow(trainCleaned2)/2):nrow(trainCleaned2), ]

par(mar=c(5,4.5,3,1))
Sys.time()
pos <- 1
result <- integer()

for (counter in seq(1, 100, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(as.factor(Happy) ~ ., trainSubset, proximity=TRUE, keep.forest=TRUE, ntree=counter)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=testSubset, type="response")
  the.result <- (prediction == testSubset$Happy)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()

n <- 50
forestTrain1 <- randomForest(as.factor(Happy) ~ ., trainCleaned2, proximity=TRUE, keep.forest=TRUE, ntree=n) # 50 best so far
forestTrain2 <- randomForest(as.factor(Happy) ~ ., trainCleaned2, proximity=TRUE, keep.forest=TRUE, ntree=n)
forestTrain3 <- randomForest(as.factor(Happy) ~ ., trainCleaned2, proximity=TRUE, keep.forest=TRUE, ntree=n)
forestTrain4 <- randomForest(as.factor(Happy) ~ ., trainCleaned2, proximity=TRUE, keep.forest=TRUE, ntree=n)
forestTrain5 <- randomForest(as.factor(Happy) ~ ., trainCleaned2, proximity=TRUE, keep.forest=TRUE, ntree=n)
# TODO: Create error curve for many retries of ntree=1:N, to find best ntree value

forestCombined <- combine(forestTrain1, forestTrain2, forestTrain3, forestTrain4, forestTrain5)
#print(forestCombined)
#forestCombined$predicted
plot(forestCombined$predicted)
forestCombined$importance
# Nice importance plot:
varImpPlot(forestCombined, cex=.7, col="blue")

# Tip: Use importance result in new glmnet model?

# Another nice importance plot:
importance <- round(forestCombined$importance, 2)
importanceLabels <- paste0(rownames(forestCombined$importance), " (", importance, ")")
importance[importance < max(importance)/3] <- ""
par(mar=c(7.2,2.5,1,1))
plot(forestCombined$importance, pch=16, cex=1.2, col="blue", xaxt="n",
     main="Var.Importance MeanDecreaseGini")
symbols(x=1:nrow(forestCombined$importance), y=forestCombined$importance, xlab="Variable", ylab="Importance",
        circles=forestCombined$importance, inches=1/6, ann=F, bg="steelblue3", fg="blue", xaxt="n")
axis(side=1, at=1:nrow(forestCombined$importance),
     cex.axis=.6, labels=importanceLabels, col="black", col.axis="steelblue4", las=2)
text(x=1:nrow(forestCombined$importance), y=forestCombined$importance - 2, labels=importance, cex=.6)

# Gives same result??
result.predicted.prob <- predict(forestCombined, testCleaned2, type="prob")
result.predicted.prob <- predict(forestCombined, testCleaned2, type="vote")
result.predicted.prob <- predict(forestCombined, testCleaned2, type="class") # Gives 0/1

# Create submission for RF:
submission <- cbind(submission.userId, round(result.predicted.prob[,2], 9))
colnames(submission) <- c("UserId", "Probability1")
head(submission)
write.csv(submission, file=paste0(submissionfolder, "RF_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F)


# -----------------------------------------------------------------------------------------------------------------------
# Do GLM

fit1 <- glm(as.factor(Happy) ~ ., data=trainCleaned2, family=binomial(link="logit"), na.action=na.exclude)
summary(fit1)
result <- predict(fit1, newdata=testCleaned2, type="response")

fit2 <- glm(as.factor(Happy) ~ Party + Q123621 + Q121011 + Q120014 + Q119334 + Q118237 + Q116953 + Q116441 +
              Q115610 + Q115899 + Q114961 + Q108855 + Q107869 + Q102906 + Q102289 + Q101162 + Q101163 + Q98869,
            data=trainCleaned2, family=binomial(link="logit"), na.action=na.exclude)
summary(fit2)
fit3 <- glm(as.factor(Happy) ~ HouseholdStatus + Party + Q118237 + Q121011 +
              Q120014 + Q119334 + Q118237 + Q116953 + Q116441 +
              Q115610 + Q115899 + Q114961 + Q108855 + Q107869 + Q102906 + Q102289 + Q101162 + Q101163 + Q98869,
            data=trainCleaned2, family=binomial(link="logit"), na.action=na.exclude)
summary(fit3)
fit4 <- glm(as.factor(Happy) ~ HouseholdStatus + Party + AgeBracket + Q118237 + Q121011 +
              Q120014 + Q119334 + Q118237 + Q116953 + Q116441 +
              Q115610 + Q115899 + Q114961 + Q108855 + Q107869 + Q102906 + Q102289 + Q101162 + Q101163 + Q98869,
            data=trainCleaned2, family=binomial(link="logit"), na.action=na.exclude)
summary(fit4) # AgeBracket, not much effect....

fit5 <- glm(as.factor(Happy) ~ EducationLevel + Income + HouseholdStatus + Party + AgeBracket + Q107869 + Q119334 +
              Q102906 + Q102289 + Q106997,
            data=trainCleaned2, family=binomial(link="logit"), na.action=na.exclude)
summary(fit5) # AgeBracket, not much effect....

result1 <- predict(fit1, newdata=testCleaned2, type="response")
result2 <- predict(fit2, newdata=testCleaned2, type="response")
result3 <- predict(fit3, newdata=testCleaned2, type="response")
result4 <- predict(fit4, newdata=testCleaned2, type="response")
result5 <- predict(fit5, newdata=testCleaned2, type="response")

auc = GetAUC(fit1, trainCleaned2, trainCleaned2$Happy)
auc
auc = GetAUC(fit2, trainCleaned2, trainCleaned2$Happy)
auc
auc = GetAUC(fit3, trainCleaned2, trainCleaned2$Happy)
auc
auc = GetAUC(fit4, trainCleaned2, trainCleaned2$Happy)
auc

# Create submission for glm:
submission <- cbind(submission.userId, round(result, 9))
colnames(submission) <- c("UserId", "Probability1")
head(submission)
write.csv(submission, file=paste0(submissionfolder, "GLM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F)

# -------------------------------------------------------------------------------------------------------------
# TODO: Try SVM
library(e1071)

fit <- svm(Happy ~., data=trainCleaned2)
summary(fit)
result <- predict(fit, newdata=testCleaned2, response="prob")
head(result)
# Create submission for glm:
submission <- cbind(submission.userId, as.integer(result)-1)
colnames(submission) <- c("UserId", "Probability1")
head(submission)
write.csv(submission, file=paste0(submissionfolder, "SVM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F)

# Or ksvm (see Titanic.Rmd):
library(kernlab)

rbf <- rbfdot(sigma=0.1)
fit <- ksvm(Happy ~ ., data=trainCleaned2, type="C-bsvc", kernel=rbf, C=10, prob.model=TRUE)


# -------------------------------------------------------------------------------------------------------------
# Caret (none of these are really working...)

library(caret)
# http://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# http://caret.r-forge.r-project.org/training.html
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                           ## Estimate class probabilities
                           classProbs = T,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

fit <- train(as.factor(Happy) ~ ., data=trainCleaned2, method="gbm", verbose=F,
             tuneGrid = data.frame(interaction.depth = 4, n.trees = 100, shrinkage = .1),
             metric = "ROC")
fit <- train(as.factor(Happy) ~ ., data=trainCleaned2, method="gbm", verbose=F, trControl=fitControl,
             metric = "ROC")
plot(fit)
result <- predict(fit, newdata=testCleaned2, type="prob")
head(result)

# Create submission for caret:
submission <- cbind(submission.userId, round(result[,2], 9))
colnames(submission) <- c("UserId", "Probability1")
head(submission)
write.csv(submission, file=paste0(submissionfolder, "Caret_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F)
