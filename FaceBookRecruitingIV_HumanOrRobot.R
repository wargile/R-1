# Facebook Recruiting IV: Human or Robot?
# http://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/data
# Deadline: 08.06.2015

# The evaluation metric for this competition is AUC.

# Submission format: ?

# For the bidder dataset

# bidder_id - Unique identifier of a bidder.
# payment_account - Payment account associated with a bidder. These are obfuscated to protect privacy. 
# address - Mailing address of a bidder. These are obfuscated to protect privacy. 
# outcome - Label of a bidder indicating whether or not it is a robot. Value 1.0 indicates a robot, where value 0.0 indicates human. 

# For the bid dataset

# bid_id - unique id for this bid
# bidder_id - Unique identifier of a bidder (same as the bidder_id used in train.csv and test.csv)
# auction - Unique identifier of an auction
# merchandise - Product type
# device - Phone model of a visitor
# time - Time that the bid is made (transformed to protect privacy).
# country - The country that the IP belongs to
# ip - IP address of a bidder (obfuscated to protect privacy).
# url - url where the bidder was referred from (obfuscated to protect privacy). 

# ------------------------------------------------------------------------------------------------------------------

library(ROCR)
library(bit64)
library(data.table)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)

set.seed(1000)
SetStandardOptions()

# ------------------------------------------------------------------------------------------------------------------

datafolder <- "C:/coding/Kaggle/FacebookRecruitingIVHumanOrRobot/data/"
submissionfolder <- "C:/coding/Kaggle/FacebookRecruitingIVHumanOrRobot/Submissions/"

colClasses.train <- c(rep("character",3),"numeric")
colClasses.test <- c(rep("character",3))
colClasses.bids <- c(rep("character",3),"factor",rep("character",5))

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=T, colClasses=colClasses.train)
  test <- read.csv(paste0(datafolder, "test.csv"), header=T, colClasses=colClasses.test)
  bids <- fread(paste0(datafolder, "bids.csv"), header=T, colClasses=colClasses.bids)
  save(train, file=paste0(datafolder, "train.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
  save(bids, file=paste0(datafolder, "bids.rda"))
} else {
  if (file.exists(paste0(datafolder, "trainCleaned.rda")) == T) {
    load(paste0(datafolder, "trainCleaned.rda"))
    load(paste0(datafolder, "testCleaned.rda"))
    load(paste0(datafolder, "bidsCleaned.rda"))
  } else {
    load(paste0(datafolder, "train.rda"))
    load(paste0(datafolder, "test.rda"))
    load(paste0(datafolder, "bids.rda"))
    load(paste0(datafolder, "bids.train.rda"))
    load(paste0(datafolder, "bids.test.rda"))
  }
}

# ------------------------------------------------------------------------------------------------------------------------------

head(train, n=1)
dim(train)
str(train)
summary(train)
sapply(train, class)
names(train)
#CorrelationPlot(train[,])

str(bids)
sapply(bids, class)

train$outcome = as.factor(train$outcome)

bids <- as.data.frame(bids)
bids$merchandise <- as.factor(bids$merchandise)

length(table(bids$bidder_id))
length(table(train$bidder_id))
length(table(test$bidder_id))

# http://www.kaggle.com/c/facebook-recruiting-iv-human-or-bot/forums/t/13823/unique-identifier-of-auction-should-be-one-item-right/76339#post76339
# Get unique list of bidders and merchandise categories #
bids2 <- fread(paste0(datafolder, "bids.csv"), header=T, colClasses=colClasses.bids) # NOTE: fread neede for lines below

# Get unique list of auctions and merchandise categories #
merch.cnt.auction <- bids2[, length(unique(merchandise)), by=auction]
setnames(merch.cnt.auction, c("auction", "merchandise.cnt"))
table(merch.cnt.auction$merchandise.cnt)

bids.train <- merge(bids, train, by="bidder_id", all.y=T)
dim(bids.train)
head(bids.train)
save(bids.train, file=paste0(datafolder, "bids.train.rda"))

bids.test <- merge(bids, test, by="bidder_id", all.y=T)
dim(bids.test)
head(bids.test)
save(bids.test, file=paste0(datafolder, "bids.test.rda"))

# ------------------------------------------------------------------------------------------------------------------------------

# Get the baseline prediction accuracy
baseline.prediction <- table(train$outcome)[1] / nrow(train)
baseline.prediction

# Check for near zero variance in predictors
nearZeroVar(bids.train) # caret package

# ------------------------------------------------------------------------------------------------------------------------------

# Split train in train and validation subsets
# NOTE: also check createDataPartition in caret!
set.seed(1000)
split <- sample.split(bids.train$outcome, SplitRatio=0.7)
train.subset <- subset(bids.train, split==TRUE)
validation.subset <- subset(bids.train, split==FALSE)

# ------------------------------------------------------------------------------------------------------------------------------

train.subset$merchandise <- as.character(train.subset$merchandise)
validation.subset$merchandise <- as.character(validation.subset$merchandise)
train.subset$auction <- as.character(train.subset$auction)
validation.subset$auction <- as.character(validation.subset$auction)

# Do RF
bidRF <- randomForest(as.factor(outcome) ~ auction+merchandise+device+country,
                 data=train.subset[1:100000,], n.trees=25, na.action=na.omit)

# Do GBM
bidGBM <- gbm(as.factor(outcome) ~ auction+merchandise+device+country, distribution="bernoulli",
                      data=train.subset[1:100000,], n.trees=25)

# Do rpart
cp.value <- 0.05
bidCART <- rpart(as.factor(outcome) ~ merchandise+device+country,
                 data=train.subset[1:100000, ], method="class", cp=cp.value, na.action=na.omit)
summary(bidCART)
prp(bidCART, cex=.8, col="blue", main="Bids")

# Evaluate the performance of the model
predict <- predict(bidCART, newdata=validation.subset, type="class")
result <- table(validation.subset$outcome, predictCART)
result
# Compute accuracy
sum(diag(result)) / sum(result)

# ------------------------------------------------------------------------------------------------------------------------------

# Show ROCR colorized plot
par(mar=c(3,3,2,2))
predROCR = prediction(predict.rf[,2], train.subset$outcome)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on Corpus", lwd=3)
lines(c(0,1), c(0,1), col="gray", lty=2)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
# NOTE: At a cutoff of 0.6-0.8, we predict a good TP rate, while at the same time having a low FP rate.
par(mar=c(3,3,2,1))
# Compute AUC
performance(predROCR, "auc")@y.values
sn <- slotNames(predROCR)
sapply(sn, function(x) length(slot(predROCR, x)))

# ------------------------------------------------------------------------------------------------------------------------------

par(mfrow=c(2,2))
hist(prediction, col="wheat")
plot((prediction), col="blue", pch=21, bg="cyan", main="Prediction result")
plot(sort(prediction), type="o", col="blue", main="Sorted prediction curve")
par(mfrow=c(1,1))
# Create the submission file
options("scipen"=100, "digits"=8)
MySubmission <- data.frame(UniqueID=test$UniqueID, Probability1=prediction)
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "RF")
