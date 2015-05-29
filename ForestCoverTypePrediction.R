# Forest Cover Type Prediction

# Deadline: May 11, 2015
# http://www.kaggle.com/c/forest-cover-type-prediction/data
# Evaluation: Submissions are evaluated on multi-class classification accuracy.

# https://archive.ics.uci.edu/ml/datasets/Covertype

# TIPS:
# http://nbviewer.ipython.org/github/aguschin/kaggle/blob/master/forestCoverType_featuresEngineering.ipynb

# Scripts:
# https://www.kaggle.com/c/forest-cover-type-prediction/scripts

# TODO: Feature engineering big time (see TIPS linke above) + try package extraTrees
# TODO: Work with a subset of feature 1+2 (and 3+6) to find out why these are troublesome for classifier

# ------------------------------------------------------------------------------------------------------------

package.install("extraTrees")
library(extraTrees)
package.install("Amelia")
library(Amelia)
library(e1071)
library(caTools)
library(tree)
library(caret)
require(gbm)

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/ForestCoverTypePrediction/data/"
codeFolder <- "C:/coding/Kaggle/ForestCoverTypePrediction/code/"
submissionsFolder <- "C:/coding/Kaggle/ForestCoverTypePrediction/submissions/"

SetStandardOptions()

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  # Save col Id from test set before munging data:
  test.id <- test$Id
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
  save(test.id, file=paste0(dataFolder, "testId.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "testId.rda"))
  load(paste0(dataFolder, "predictor_influence.rda"))
}

#train <- lapply(train, function(x) {
#  x[sapply(x, is.null)] <- NA
#  unlist(x)
#})
#do.call("rbind", train)

head(train)
head(test)
dim(train)
dim(test)
sapply(train, class)
pairs(train[,1:26], col="blue")
pairs(train[26:53], col="blue")
CorrelationPlot(train)
CorrelationPlot(train[,1:14])
cor(train$Slope, train$Hillshade_9am)
cor(train$Slope, train$Hillshade_3pm)
cor(train$Slope, train$Hillshade_Noon)

# Add a predictor (factor) for whether Vertical_Distance_To_Hydrology < 0 or not
train$IsBelowSeaLevel <- as.factor(ifelse(train$Vertical_Distance_To_Hydrology <= 0, 1, 0))
test$IsBelowSeaLevel <- as.factor(ifelse(test$Vertical_Distance_To_Hydrology <= 0, 1, 0))
table(train$IsBelowSeaLevel)
table(test$IsBelowSeaLevel)

# Look at missing values:
par(mfrow=c(1,2))
missmap(train[1:500, ], main = "Missingness Map Train", col = c("wheat", "cornflowerblue"))
missmap(test[1:500, ], main = "Missingness Map Test", col = c("wheat", "blue"))
par(mfrow=c(1,1))

sum(!complete.cases(train))
sum(!complete.cases(test))

mpute(train, what="median")
impute(test, what="median")

sum(!complete.cases(train))
sum(!complete.cases(test))

# Get the variance. Remove predictors with zero variance (lots here):
op <- par()
par(mar=c(11.2,5,2,.5))
par(mfrow=c(1,2))
variance.train <- sapply(train[, c(-53)], var)
sort(variance.train, decreasing=T)[1:10]
# Compare to:
influence[1:10]

par(mfrow=c(1,1))
par(mar=c(10,3,2,1))
plot(sort(variance.train, decreasing=T), pch=16, col="blue", main="Variance in training set", xaxt="n",
     xlab="", ylab="Variance")
axis(side=1, at=1:length(variance.train), labels=names(sort(variance.train, decreasing=T)), las=2)
par(mar=c(3,3,2,1))

variance.test <- sapply(test[, -1], var)
plot(variance.test, pch=16, col="blue", main="Variance", cex.lab=.8, cex.axis=.8)
par(mfrow=c(1,1))
par <- op

FixData <- function(data) {
  # Convert all hex cols, add a "0x" prefix, and then use as.integer(hex_val)
  for (counter in 1:length(names(data))) {
    if (substr(names(data)[counter], 1, 4) %in% c("Soil", "Wild")) {
      data[, counter] <- as.factor(data[, counter])
    }
  }
  return (data)
}

train <- FixData(train)
test <- FixData(test)
train$Cover_Type <- as.factor(train$Cover_Type)

# Remove null variance cols
train$Soil_Type7 <- NULL
train$Soil_Type15 <- NULL
test$Soil_Type7 <- NULL
test$Soil_Type15 <- NULL

# Remove Id col
train$Id <- NULL
test$Id <- NULL

# Do some exploratory plots:
par(mfrow=c(1,2))
par(mar=c(3,3,2,.8))
plot(Horizontal_Distance_To_Hydrology ~ Elevation, data=train, pch=21, bg=Cover_Type)
plot(Vertical_Distance_To_Hydrology ~ Elevation, data=train, pch=21, bg=Cover_Type)
par(mfrow=c(2,2))
plot(Hillshade_Noon ~ Hillshade_3pm, data=train, pch=21, bg=Cover_Type, main="Noon to 3PM")
plot(Hillshade_9am ~ Hillshade_3pm, data=train, pch=21, bg=Cover_Type, main="9AM to 3PM")
plot(Hillshade_9am ~ Hillshade_Noon, data=train, pch=21, bg=Cover_Type, main="9AM to Noon")
par(mar=c(5,5,3,1))
par(mfrow=c(1,1))

# TODO: Check all the Soil_Type variables and all the Wilderness_Area variables, to see if there is a pattern
# that could be feature engineered.
barplot(as.numeric(train[4,15:52]))

# TODO: Create subsets of Cover_Type 1+2 and 3+6, they often get confused
CoverType1_2 <- subset(train, Cover_Type %in% c(1,2))
CoverType3_6 <- subset(train, Cover_Type %in% c(3,6))
CoverType4_5 <- subset(train, Cover_Type %in% c(4,5))
barplot(with(CoverType1_2, tapply(Elevation, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Slope, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Aspect, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Horizontal_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Horizontal_Distance_To_Fire_Points, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Horizontal_Distance_To_Roadways, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Vertical_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Vertical_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Vertical_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Hillshade_9am, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Hillshade_3pm, Cover_Type, mean)))
barplot(with(CoverType1_2, tapply(Hillshade_Noon, Cover_Type, mean)))

barplot(with(train, tapply(Elevation, Cover_Type, mean)), main="Elevation (mean)", col="powderblue")
barplot(with(train, tapply(Slope, Cover_Type, mean)), main="Slope (mean)", col="powderblue")

PlotSoilTypeDifference <- function(data, st1, st2) {
  CoverType1 <- subset(data, Cover_Type == st1)
  CoverType2 <- subset(data, Cover_Type == st2)
  
  x <- integer()
  y <- integer()
  for (counter in 1:38) {
    x[counter] <- table(CoverType1[,(counter + 14)])[2]
    y[counter] <- table(CoverType2[,(counter + 14)])[2]              
  }
  df <- data.frame(C1=x, C2=y)
  df
  y.min <- ifelse(min(df$C1) < min(df$C2), min(df$C1), min(df$C2))
  y.max <- ifelse(max(df$C1) > max(df$C2), max(df$C1), max(df$C2))
  
  plot(df$C1, type="o", col="blue", main="Soil type=1, difference between CoverType 1 and 2",
       ylim=c(0, y.max))
  lines(df$C2, type="o", col="red")
}
GetMaxSoilType <- function(st1) {
  CoverType1 <- subset(train, Cover_Type == st1)
  x <- integer()
  for (counter in 1:38) {
    x[counter] <- table(CoverType1[,(counter + 14)])[2]
    #x[counter] <- var(CoverType1[,(counter + 14)])
  }
  return (which.max(x) + 14)
  #return (x[which.max(x)])
}
sapply(1:7, function(x) GetMaxSoilType(x)) # Look at result for 1,2 (41,41) and 3,6 (23,23)....

PlotSoilTypeDifference(train, 1, 2)
PlotSoilTypeDifference(train, 3, 6)
PlotSoilTypeDifference(train, 4, 5)
PlotSoilTypeDifference(train, 4, 7)

par(mar=c(4,3,2,1))
par(mfrow=c(2,1))
barplot(diag(var(train[, 15:52])), las=2, col="wheat", main="Variance soil type, train")
barplot(diag(var(test[, 15:52])), las=2, col="powderblue", main="Variance soil type, test")
par(mfrow=c(1,1))
par(mar=c(3,3,2,1))

barplot(with(CoverType4_5, tapply(Elevation, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Slope, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Aspect, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Horizontal_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Horizontal_Distance_To_Roadways, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Horizontal_Distance_To_Fire_Points, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Vertical_Distance_To_Hydrology, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Hillshade_9am, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Hillshade_3pm, Cover_Type, mean)))
barplot(with(CoverType4_5, tapply(Hillshade_Noon, Cover_Type, mean)))



# ----------------------------------------------------------------------------------------------------------------
# Split train in train and verification, and get RMSE:
result <- CreateTrainAndValidationSets(train)
train.subset <- result$train
verification.subset <- result$validation

set.seed(1000)
split <- sample.split(train$Cover_Type, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)
dim(train.subset)
dim(validation.subset)


oldmar <- par()$mar
par(mar=c(5,4.5,3,1))
pos <- 1
result <- integer()

for (counter in seq(1, 50, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(train.subset$Cover_Type ~ ., data=train.subset[, names(influence)[1:15]],
                               proximity=TRUE, keep.forest=TRUE, ntree=counter)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=verification.subset[, -53], type="response")
  the.result <- (prediction == verification.subset$Cover_Type)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(verification.subset)))
  pos <- pos + 1
  message(paste("Iteration", pos))
}

# Best: 0.177 with 47 trees and train.subset[, names(influence)[1:5]
# Best: 0.167 with 44 trees and train.subset[, names(influence)[1:10]

plot(result, pch=21, col="blue", bg="cyan", type="o",
     main=paste0("Random Forest Error Rate (best=", round(min(result), 3), ", trees=",
                 which(result == min(result)), ")"), cex.axis=.8, cex.main=1, cex.lab=.8,
     xlab="Number of trees", ylab="Error rate")
grid()
abline(v=which(result == min(result)), col="red")
par=oldmar
# TODO: Move this nice graph function into a tools.R method?

# Try Neural Network
package.install("nnet")
library(nnet)

# Normalize the data between 0 and 1
NormalizeData <- function(data) {
  for (counter in 2:ncol(data)-1) {
    data[, counter] <- data[, counter] / max(data)
  }
  
  return(data)
}

train.normalized <- NormalizeData(train.subset)
verification.normalized <- NormalizeData(verification.subset)

ideal <- class.ind(verification.normalized$Cover_Type)

# Train the model. Leave out the class attribute
nnet.result = nnet(Cover_Type ~ ., data=train.normalized, size=5) #, softmax=TRUE)

# Predict on testset
result <- predict(nnet.result, verification.normalized)
table(result)

# Calculate Classification accuracy
table(verification.normalized$cover_Type, predict(nnet.result, verification.normalized, type="class"))

# ----------------------------------------------------------------------------------------------------------------
# Grow a tree...

cols <- as.integer(which(variance.train < .15))
cols
train2 <- train[, as.integer(which(variance.train > .15))]
names(train2)

myTree <- tree(train$Cover_Type ~ ., data=train2)
plot(myTree)
title(main="Decision Tree Forest Cover Type", cex.main=.8)
text(myTree, cex=.8, col="blue")
summary(myTree) # Misclassification error rate: 0.402 = 6080 / 15120 (not very good...)
par(mfrow=c(1,2))
plot(cv.tree(myTree, FUN=prune.tree, method="misclass"))
plot(cv.tree(myTree))
par(mfrow=c(1,1))
pred.tree <- predict(myTree, newdata=test, type="class")
pred.tree[1:100]

# Do lm, with normalized subset of cols
cols <- as.integer(which(variance.train < 1))
cols
train2 <- train[, as.integer(which(variance.train > 1))]
names(train2)

fit1 <- lm(as.integer(train$Cover_Type) ~ ., data=train2)
summary(fit1)
pred1 <- predict(fit1, newdata=test[,-53], type="response")
pred.values <- round(pred1)
pred.values[pred.values <= 0] <- 1
pred.values[pred.values > 7] <- 7
table(pred.values)

submission <- data.frame(ID=test$Id, Cover_Type=pred.values)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "LM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Do GBM

# GBM model settings, these can be varied 
GBM_NTREES = 250
GBM_SHRINKAGE = 0.2 
GBM_DEPTH = 22
GBM_MINOBS = 50

#cols <- as.integer(which(variance.train == 0))
#cols
#train2 <- train[, as.integer(which(variance.train > 0)) + 1]
#names(train2)

# Build the GBM model 
GBM_model <- gbm.fit(x = train[, -53], y = train$Cover_Type, distribution = "multinomial", 
                     n.trees = GBM_NTREES, shrinkage = GBM_SHRINKAGE, interaction.depth = GBM_DEPTH,
                     n.minobsinnode = GBM_MINOBS, verbose = TRUE) 

# List variable importance, re-model only with predictors that have importance > 0?
oldmar=par()$mar
par(mar=c(3,10,2,1))
summary(GBM_model, GBM_NTREES, main="GBM variable importance", cex.axis=.8, cex.lab=.8,
        cex.main=1, cex.names=.7, las=1)
par(mar=oldmar)
oldmar=par()$mar
par(mar=c(10,3.5,2,1))
influence <- relative.influence(GBM_model, GBM_NTREES, sort=T)
save(influence, file=paste0(dataFolder, "predictor_influence.rda"))
influence <- influence[influence > 0]
barplot(influence, col="cornflowerblue", las=2, cex.axis=.6, cex.names=.6, cex.main=1, main="GBM variable importance")
par(mar=oldmar)

#axis(2, las=2)
#axis(1, las=1)

# Predict for the leaderboard data 
prediction <- predict.gbm(object=GBM_model, newdata=test, GBM_NTREES)
prediction[1:20]
min(prediction)
max(prediction)
prediction2 <- Normalize(min(prediction), max(prediction), 1, 7, prediction)
prediction2[1:20]

# Put on correct scale and cap 
prediction2 <- expm1(prediction)
prediction2
prediction2 <- pmax(7, prediction) 
prediction2
prediction2 <- pmin(0, prediction2) 
prediction2

# Plot the submission distribution 
hist(prediction, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction", xlab="Prediction")

submission <- data.frame(ID=test$Id, Cover_Type=prediction)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GBM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------

forestTrain1 <- randomForest(train$Cover_Type ~ ., data=train[, names(influence)[1:10]], # 1:10
                             proximity=TRUE, keep.forest=TRUE, ntree=150, importance=F, norm.votes=T) # 150
forestTrain2 <- randomForest(train$Cover_Type ~ ., data=train[, names(influence)[1:10]],
                             proximity=TRUE, keep.forest=TRUE, ntree=150, importance=F, norm.votes=T)


plot(importance(forestTrain1, type=2), pch=19, col="blue")

# TEST: Just use the cols that have var > 0
cols <- as.integer(which(variance.train == 0))
cols
train2 <- train[, as.integer(which(variance.train > 0)) + 1]
names(train2)
forestTrain1 <- randomForest(as.factor(train$Cover_Type) ~ ., data=train2,
                             proximity=TRUE, keep.forest=TRUE, ntree=1000, importance=F, norm.votes=T)

forestCombined <- combine(forestTrain1, forestTrain2)

#prediction <- predict(forestTrain1, newdata=test, type="response")
#prediction <- as.integer(predict(forestTrain1, newdata=test, type="response"))
prediction <- as.numeric(predict(forestCombined, newdata=test, type="response"))
prediction <- as.numeric(predict(forestTrain1, newdata=test, type="response"))
c(min(as.numeric(prediction)), max(as.numeric(prediction)))
table(prediction)

submission <- data.frame(ID=test.id, Cover_Type=prediction)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "RF_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# --------------------------------------------------------------------------------------------------------
# Do party
library(party)

cf1 <- cforest(as.factor(train$Cover_Type) ~ ., data=train[, -c(1, 22, 30, 56)],
               control=cforest_unbiased(mtry=2, ntree=50))

my.varimp <- varimp(cf1)
oldmar=par()$mar
par(mar=c(8,4,2,1))
plot(my.varimp, pch=19, col="blue", main="Variable importance", cex.axis=.7, cex.lab=.7,
     xaxt="n", xlab="", ylab="Importance")
axis(at=1:length(my.varimp), labels=names(my.varimp), side=1, cex.axis=.5, las=2)
grid()
par(mar=oldmar)
#varimp(cf1, conditional=TRUE)

# Find the cols that has variable importance higher than <N> and only include those cols
include.cols <- names(train)[which(my.varimp > 0.005)]
train[1:10, include.cols]
cf1 <- cforest(as.factor(train$Cover_Type) ~ ., data=train[, include.cols[-1]], # Skip id
               control=cforest_unbiased(mtry=2, ntree=50))

prediction <- round(predict(cf1, newdata=test[, include.cols[-1]], type="response"))
c(min(prediction), max(prediction))

submission <- data.frame(ID=test$Id, Cover_Type=prediction)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "Party_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# --------------------------------------------------------------------------------------------------------
# Try caret

# Fix for confusion matrix (other effects too??)
train$Cover_Type <- as.character(train$Cover_Type)
train$Cover_Type <- paste0("Seg", train$Cover_Type)
train$Cover_Type <- as.factor(train$Cover_Type)

# Tip: Get variable diff between sets, or to exclude outcome var, etc.:
x_vars <- setdiff(names(train), c("Cover_Type"))

Grid <- expand.grid(n.trees=c(250), interaction.depth=c(22), shrinkage=0.2)
fitControl <- trainControl(method="none", classProbs=T)

GBM_model <- train(Cover_Type ~ ., data=train, method="gbm", # TODO: Try rf too
                   trControl=fitControl, verbose=T,
                   tuneGrid=Grid, metric="ROC")

pred.train <- predict(GBM_model, newdata=train)
confusionMatrix(GBM_model, train$Cover_Type)

prediction <- predict(GBM_model, newdata=test)

submission <- data.frame(ID=test.id, Cover_Type=as.character(gsub("Seg", "", prediction)))
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "Caret_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ------------------------------------------------------------------------------------------------------------
# Deep learning with h2o...
# https://github.com/woobe/blenditbayes
# http://blenditbayes.blogspot.co.uk/
package.install("h2o")
suppressMessages(library(h2o))
library(mlbench) # For testing on datasets in example referenced at http://blenditbayes.blogspot.co.uk/

cols <- c(which(names(train) %in% names(influence)[influence > 100]), 53) # Was: 15. TODO: Try various (higher?) levels here

# Correlation:
# 2                         Elevation                Wilderness_Area4 -0.7837
# 7                     Hillshade_9am                   Hillshade_3pm -0.7800
# 6  Horizontal_Distance_To_Hydrology  Vertical_Distance_To_Hydrology  0.6521
# 4                            Aspect                   Hillshade_3pm  0.6350
# 8                    Hillshade_Noon                   Hillshade_3pm  0.6145
# 5                             Slope                  Hillshade_Noon -0.6126
# 3                            Aspect                   Hillshade_9am -0.5940
# 1                         Elevation Horizontal_Distance_To_Roadways  0.5787
# 10                 Wilderness_Area3                Wilderness_Area4 -0.5692
# 9                  Wilderness_Area1                     Soil_Type29  0.5463
names(train)
remove.cols <- c("Wilderness_Area4","Hillshade_3pm","Horizontal_Distance_To_Hydrology","Hillshade_Noon","Hillshade_9am",
  "Horizontal_Distance_To_Roadways","Wilderness_Area3","Soil_Type29")
remove.cols <- which(names(train) %in% remove.cols) * -1

localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()

dat_h2o <- as.h2o(localH2O, train, key='train')
dat_h2o.test <- as.h2o(localH2O, test, key='test')

dat_h2o <- as.h2o(localH2O, train[, remove.cols], key='train')
dat_h2o.test <- as.h2o(localH2O, test[, remove.cols], key='test')

dat_h2o <- as.h2o(localH2O, train.subset[, cols], key='train')
dat_h2o.test <- as.h2o(localH2O, validation.subset[, cols], key='test')

dat_h2o <- as.h2o(localH2O, train.subset, key='train')
dat_h2o.test <- as.h2o(localH2O, validation.subset, key='test')

# train/test:
y.col <- 53
x.cols <- c(1:52,54) # Added IsBelowSeaLevel
# train.subset/validation.subset with more cols:
y.col <- 53
x.cols <- c(1:52,54) # Added IsBelowSeaLevel
y.col <- 53
x.cols <- c(1:6,10:52) # TEST: Skip hillshade predictors

model.dl <- 
  h2o.deeplearning(x=x.cols, # column numbers for predictors
                   y=y.cols, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   activation="Tanh", # or 'TanhWithDrouput'
                   #input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=F,
                   # l2, # TODO: How to set L1 or L2 regularization?
                   hidden=c(50, 50, 50), # three layers of 50 nodes
                   epochs=100) # max. no. of epochs
model.dl

model.gbm <-
  h2o.gbm(x=x.cols, y=y.col, distribution = "multinomial", data=dat_h2o, key = "gbm", n.trees = 250, 
        interaction.depth = 5, n.minobsinnode = 10, shrinkage = 0.1, n.bins = 20,
        group_split = TRUE, importance = FALSE, nfolds = 0, holdout.fraction = 0,
        balance.classes = FALSE, max.after.balance.size = 5, class.sampling.factors = NULL,
        grid.parallelism = 1)
model.gbm

model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, data=dat_h2o, key = "rf", classification = TRUE, ntree = 500, # Was: 250 
    depth = 20, mtries = -1, sample.rate = 2/3, nbins = 20, seed = -1, 
    importance = FALSE, score.each.iteration = FALSE, nfolds = 0, 
    holdout.fraction = 0, nodesize = 1, balance.classes = FALSE, 
    max.after.balance.size = 5, class.sampling.factors = NULL, 
    doGrpSplit = TRUE, verbose = FALSE, oobee = TRUE, stat.type = "ENTROPY", 
    type = "fast")
model.rf

h2o_yhat_test.dl <- h2o.predict(model.dl, dat_h2o.test)
df_yhat_test.dl <- as.data.frame(h2o_yhat_test.dl)
table(df_yhat_test.dl$predict)
h2o_yhat_test.gbm <- h2o.predict(model.gbm, dat_h2o.test)
df_yhat_test.gbm <- as.data.frame(h2o_yhat_test.gbm)
table(df_yhat_test.gbm$predict)
h2o_yhat_test.rf <- h2o.predict(model.rf, dat_h2o.test)
df_yhat_test.rf <- as.data.frame(h2o_yhat_test.rf)
table(df_yhat_test.rf$predict)

# CV score on validation subset:
table(validation.subset$Cover_Type) # The ground truth
result.dl <- table(validation.subset$Cover_Type, df_yhat_test.dl$predict)
result.dl
result.gbm <- table(validation.subset$Cover_Type, df_yhat_test.gbm$predict)
result.gbm
result.rf <- table(validation.subset$Cover_Type, df_yhat_test.rf$predict)
result.rf
table(df_yhat_test.rf$predict, df_yhat_test.gbm$predict)
ConfusionMatrix(result.dl, sort(unique(validation.subset$Cover_Type)))
ConfusionMatrix(result.gbm, sort(unique(validation.subset$Cover_Type)))
ConfusionMatrix(result.rf, sort(unique(validation.subset$Cover_Type)))
score.dl <- sum(diag(result.dl)) / sum(result.dl)
score.dl
score.gbm <- sum(diag(result.gbm)) / sum(result.gbm)
score.gbm
score.rf <- sum(diag(result.rf)) / sum(result.rf)
score.rf

submission <- data.frame(ID=test.id, Cover_Type=df_yhat_test$predict)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "h2o_rf_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
# h2o randomForest (h2o_rf_benchmark_20150413_1358.csv) with ntrees=250 using all predictors from .RDA file
# gives the best score so far: 0.74496

# -----------------------------------------------------------------------------------------------------------------
# TODO: Try rpart for multi class prediction. Ensemble with rf or other classification models?
library(rpart)
library(rpart.plot)

fol <- formula(Cover_Type ~ Elevation + Vertical_Distance_To_Hydrology + Aspect + IsBelowSeaLevel)
fol <- formula(Cover_Type ~ .)
#model <- rpart(fol, method="class", data=train.subset, cp=0.01)
model <- rpart(fol, method="class", data=train.subset, cp=0.01, minbucket=50)
model <- rpart(fol, method="class", data=train, cp=0.01)
print(model)
summary(model)
prp(model, cex=.7, col="blue")

pred.rpart <- predict(model, newdata=validation.subset, type="class")
pred.rpart <- predict(model, newdata=test, type="class")
plot(sort(pred.rpart), type="l", col="blue", main="pred.rpart")
barplot(table(train$Cover_Type), col="powderblue", main="train$Cover_Type") # NOTE: Equal frequency!
barplot(table(pred.rpart), col="powderblue", main="pred.rpart")
accuracy <- table(validation.subset$Cover_Type, pred.rpart)
accuracy
ConfusionMatrix(accuracy, labels=c(1:7))
score <- sum(diag(accuracy)) / sum(accuracy)
score

submission <- data.frame(ID=test.id, Cover_Type=pred.rpart)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "rpart",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
