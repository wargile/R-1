# Kaggle Tradeshift Text Classification
# Ends: Mon 10 Nov 2014

# http://www.kaggle.com/c/tradeshift-text-classification/forums/t/10518/random-forest-benchmark
# https://www.kaggle.com/c/tradeshift-text-classification/details/evaluation

# TIPS/CODE:
# http://www.kaggle.com/c/tradeshift-text-classification/forums/t/10561/help-hash-values
# http://www.kaggle.com/blobs/download/forum-message-attachment-files/1629/fast_solution.py

# Solutions:
# http://www.kaggle.com/c/tradeshift-text-classification/forums/t/10901/solution-sharing

set.seed(16071962)
source("tools.R")

datafolder <- "C:/coding/Kaggle/TradeshiftTextClassification/Data/"
submissionsfolder <- "C:/coding/Kaggle/TradeshiftTextClassification/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- fread(paste0(folder, "train.csv"), header=T, sep=",")
  trainLabels <- fread(paste0(datafolder, "trainLabels.csv"), header=T, sep=",")
  test <- fread(paste0(datafolder, "test.csv"), header=T, sep=",")
  
  # Train/Test file from Python conversion
  train_out <- fread(paste0(datafolder, "train_out.csv"), header=T, sep=",")
  train_out <- as.data.frame(train_out)
  View(train_out[1:500,])
  test_out <- fread(paste0(datafolder, "test_out.csv"), header=T, sep=",")
  test_out <- as.data.frame(test_out)
  View(test_out[1:500,])
  
  train <- as.data.frame(train)
  trainLabels <- as.data.frame(trainLabels)
  test <- as.data.frame(test)
  test.ids <- test[,1]
  
  save(train, file=paste0(datafolder, "train.rda"))
  save(trainLabels, file=paste0(datafolder, "trainLabels.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
  save(test.ids, file=paste0(datafolder, "testIds.rda"))
} else {
  load(paste0(datafolder, "train.rda"))
  load(paste0(datafolder, "trainLabels.rda"))
  load(paste0(datafolder, "test.rda"))
  load(paste0(datafolder, "testIds.rda"))
}


head(train, n=1)
dim(train)
summary(train)
sapply(train, class)
numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))

head(trainLabels[,-1], n=5)
names(trainLabels)
plot(colSums(trainLabels[,c(-1)]))

dim(test)

# -------------------------------------------------------------------------------------------------------------------------------
# TODO: Data cleaning
# Add a col or convert the Base64 hashes to line counts ("X6dDAI/DZOWvu0Dg6gCgRoNr2vTUz/mc4SdHTNUPS38=" = 1, "" = 0 ??)

# Fix missing values (""). Replace with what: YES/NO? Needed?
missing.vals.cols <- c(1,2,10:14,24:26,30:33,41:45,55:57,62:63,71:75,85:87,92:93,101:105,115:117,126:130,140:142)
which(nchar(test$x10[1:10]) == 0)

# Fix YES/NO cols (either convert to factor and/or set to 0/1)
# -------------------------------------------------------------------------------------------------------------------------------

my.bitarray <- c(0,1,0,0,1)
CreateNumberFromBitArray(my.bitarray)
my.bitarray <- c(0,1,1,1,1,1,1,1)
CreateNumberFromBitArray(my.bitarray)
my.bitarray <- c(1,0,0,0,0,0,0,0,0)
CreateNumberFromBitArray(my.bitarray)

my.numbers <- integer(0)
for (counter in 1:nrow(trainLabels)) {
  my.bitarray <- unlist(trainLabels[counter, -1])
  my.numbers <- c(my.numbers, CreateNumberFromBitArray(my.bitarray))
}
my.numbers[1:100]

CreateNumberFromBitArray <- function(my.bitarray) {
  my.number <- 0
  for (counter in length(my.bitarray):1) {
    if (my.bitarray[counter] == 1) {
      #print(counter)
      my.number <- bitwOr(my.number, bitwShiftL(1, length(my.bitarray) - counter))
    }
  }
  return (my.number)
}

# Create subsets of the train numeric cols and cbind y's
train2 <- train[1:250000, numeric.cols]
train2 <- cbind(train2, trainLabels[1:250000, -1])
train.subset <- train2[1:150000, ]
validation.subset <- train2[150001:250000, ]
names(train.subset)

CorrelationPlot(train.subset[, c(2:86)])
good.cols <- c("x131","x132","x66","x76","x21")
CorrelationPlot(train.subset[, good.cols])

fit <- glm(train.subset[, 87] ~ ., data=train.subset[, c(2:86)], family=binomial(link="logit"))
fit1 <- glm(train.subset[, 87] ~ ., data=train.subset[, good.cols], family=binomial(link="logit"))
fit2 <- glm(train.subset[, 88] ~ ., data=train.subset[, good.cols], family=binomial(link="logit"))
fit3 <- glm(train.subset[, 95] ~ ., data=train.subset[, good.cols], family=binomial(link="logit"))
fit3 <- glm(train.subset[, 95] ~ ., data=train.subset[, c(2:86)], family=binomial(link="logit"))
summary(fit3)
pred1 <- predict(fit3, newdata=validation.subset[, c(-1)], type="response")
pred1 <- predict(fit3, newdata=test[, c(-1)], type="response")
pred1 <- ifelse(pred1 < 0.5, 0, 1) # NOTE: Do NOT round off/convert for submission file!
table(pred1)
table(validation.subset[, 95])
MyRMSE(validation.subset[, 95], pred1)

y <- 33 # trainLabels
last.x <- 86
y.preds <- list()
rmse <- list()

# TIP: ?reshape to fix the submission format?
# In Matlab, the transformation corresponds to (y1-y4, 3 rows in example below):
# testLabelsPredicted = [0.9, 0.1, 0.0, 0.3; 0.03, 0.7, 0.2, 0.85; 0.19, 0.0, 1.0, 0.27];
# testLabelsSubmitted = reshape(testLabelsPredicted.',[],1);
testLabelsPredicted = as.data.frame(matrix(c(0.9, 0.1, 0.0, 0.3, 0.03, 0.7, 0.2, 0.85, 0.19, 0.0, 1.0, 0.27),
                             nrow=3, byrow=T))
testLabelsPredicted
reshape(testLabelsPredicted, direction="long")

# --------------------------------------------------------------------------------------------------------------
# GLM

for (counter in 1:y) {
  fit <- glm(train.subset[, last.x + counter] ~ ., data=train.subset[, good.cols], family=binomial(link="logit"))
  #fit <- glm(train.subset[, last.x + counter] ~ ., data=train.subset[, c(2:86)], family=binomial(link="logit"))
  #summary(fit)
  pred1 <- predict(fit, newdata=validation.subset[, c(-1)], type="response")
  pred1 <- predict(fit, newdata=test[, c(-1)], type="response")
  #pred1 <- ifelse(pred1 < 0.5, 0, 1) # NOTE: Do NOT round off/convert for submission file!
  #table(pred1)
  #table(validation.subset[, counter])
  rmse[[counter]] <- MyRMSE(validation.subset[, last.x + counter], pred1)
  y.preds[[counter]] <- pred1
}

rmse

submission <- data.frame(id_label=character(0), pred=numeric(0))

# Create the submission file format, one line for each y per id
for (counter in 1:length(test.ids)) {
  for (counter2 in 1:y) {
    id_label <- paste0(test.ids[counter], "_", names(train.subset)[last.x + counter2])
    prediction <- y.preds[[counter2]][counter]
    submission <- rbind(submission, data.frame(id_label=id_label, pred=prediction))
  }
}

n <- length(test.ids)
id_labels <- rep(" ", n)
predictions <- rep(0.0, n)
the.names <- names(train.subset)[last.x + 1:33]
pos <- 1
# Create the submission file format, one line for each y per id
for (counter in 1:length(test.ids)) {
  if (counter %% 1000 == 0)
    print(counter)
  
  for (counter2 in 1:y) {
    id_label <- paste0(test.ids[counter], "_", the.names[counter2])
    id_labels[pos] <- id_label
    predictions[pos] <- y.preds[[counter2]][counter]
    pos <- pos + 1
  }
}

submission <- data.frame(id_label=id_labels, pred=predictions)

rownames(submission) <- NULL
head(submission)

# Create the submission file
KaggleSubmission(submission, submissionsfolder, "GLM_binom")

# ---------------------------------------------------------------------------------------------------------------
# Random Forest
library(randomForest)
good.cols <- c("x131","x132","x66","x76","x21")
rf1 <- randomForest(y=trainLabels[1:5000,1], x=train[1:5000, good.cols], proximity=TRUE, keep.forest=TRUE, ntree=10)
prediction <- predict(rf1, newdata=test_out[, -1], type="response")
