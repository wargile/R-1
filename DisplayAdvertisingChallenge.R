# Kaggle Display Advertising Challenge
# Deadline: Tue 23 Sep 2014

# TIPS:
# Implement "feature trimming", which consists of:
# 1) introducing a random vector into the feature set,
# 2) calculating feature importance,
# 3) removing the features with importance below the "dummy feature".
# 4) http://www.kaggle.com/c/criteo-display-ad-challenge/forums/t/10429/congratulations-to-the-winners

library(data.table)
require(bit64)

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/data/"
codeFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/code/"
submissionsFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  # NOTE: Using fread() in data.table for faster read, also can set nrows=<N> to read a chunk
  train <- fread(paste0(dataFolder, "train_fixed.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- fread(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

dim(train)
dim(test)

# Save the test$Id col for use in submission
test.id <- test$Id
save(test.id, file=paste0(dataFolder, "testId.rda"))

# TODO - IMPORTANT: Since data are chronological, is it better to get chunks rather than random row selections??

# Testing using the smaller test set as training data...
# data <- read.csv("C:/coding/Kaggle/DisplayAdvertisingChallenge/data/train_out.csv", header=T, sep=",", stringsAsFactors=F)
# NOTE: Using fread() in data.table for faster read, also can set nrows=<N> to read a chunk
data <- fread("C:/coding/Kaggle/DisplayAdvertisingChallenge/data/train_out.csv", header=T, sep=",", stringsAsFactors=F)
dim(data)
str(data)
summary(data)
describe(data)

data2 <- data.frame(data)
test2 <- data.frame(test)

# Impute with mean/median??
data2[data2 == ""] <- "0" # TODO: Correct to assume that NA's can be replaced with zero here...? Or just skip NA rows?
test2[test2 == ""] <- "0" # TODO: Correct to assume that NA's can be replaced with zero here...? Or just skip NA rows?

# TODO: What is CTR? Moving average for time series??
GetCTR <- function(data) {
  CTR <- 0.0
  ctr_array <- numeric(0)
  decay <- 0.9999

  for (i in 1:length(data)) {
    CTR <- decay * CTR + (1.0 - decay) * data[i]
    ctr_array[i] <- CTR
  }
  
  return (ctr_array)
}

GetCMA <- function(data) {
  rows <- length(data) - 1
  ma <- rep(NA, rows)
  cma <- rep(NA, rows)
  
  for (counter in 3:rows) {
    ma[counter] <- mean(data[(counter - 2):(counter + 1)])
  }
  
  for (counter in 3:rows) {
    cma[counter] <- mean(ma[counter:(counter + 1)])
  }
  
  return (cma)
}

# Normalize values between min and max
Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  # Find the normalized (0-1 range) value of curval
  normalized <- (curval - minval) / (maxval - minval)
  normalized
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

FixData <- function(data, startcol) {
  # Convert all hex cols, add a "0x" prefix, and then use as.integer(hex_val)
  for (counter in startcol:length(names(data))) {
    new.colname <- paste0(names(data)[counter], ".new")
    if (substr(names(data)[counter], 1, 1) == "C") {
      data[, counter] <- paste0("0x", data[, counter])
      data[, new.colname] <- 0.0
      data[, new.colname] <- as.numeric(data[, counter])
    } else {
      NA.rows <- which(is.na(data[, counter]))
      data[NA.rows, counter] <- 0
      data[, new.colname] <- 0.0
      data[, new.colname] <- as.numeric(data[, counter])
    }
    data[, new.colname] <- Normalize(min(data[, new.colname]), max(data[, new.colname]), 0, 1, data[, new.colname])
  }
  
  return (data)
}

FixData2 <- function(data, startcol) {
  for (counter in startcol:length(names(data))) {
    data[, counter] <- Normalize(min(data[, counter]), max(data[, counter]), 0, 1, data[, counter])
  }
  
  return (data)
}

#data2 <- FixData(data2, 3)
train2 <- data.frame(train)
train3 <- FixData2(train2, 3)
names(train3)
dim(train3)
head(train3, n=1)

test2.chunk1 <- FixData(test2[1:500000, ], 2)
test2.chunk2 <- FixData(test2[500001:1000000, ], 2)
test2.chunk3 <- FixData(test2[1000001:1500000, ], 2)
test2.chunk4 <- FixData(test2[1500001:2000000, ], 2)
test2.chunk5 <- FixData(test2[2000001:2500000, ], 2)
test2.chunk6 <- FixData(test2[2500001:3000000, ], 2)
test2.chunk7 <- FixData(test2[3000001:3500000, ], 2)
test2.chunk8 <- FixData(test2[3500001:4000000, ], 2)
test2.chunk9 <- FixData(test2[4000001:4500000, ], 2)
test2.chunk10 <- FixData(test2[4500001:5000000, ], 2)
test2.chunk11 <- FixData(test2[5000001:5500000, ], 2)
test2.chunk12 <- FixData(test2[5500001:6000000, ], 2)
test2.chunk13 <- FixData(test2[6000001:6042135, ], 2)
# TODO: If predict can handle it, merge some of these chunks into larger units?

# Save the chunks and data2:
save(test2.chunk1, file=paste0(dataFolder, "test2.chunk1.rda"))
save(test2.chunk2, file=paste0(dataFolder, "test2.chunk2.rda"))
save(test2.chunk3, file=paste0(dataFolder, "test2.chunk3.rda"))
save(test2.chunk4, file=paste0(dataFolder, "test2.chunk4.rda"))
save(test2.chunk5, file=paste0(dataFolder, "test2.chunk5.rda"))
save(test2.chunk6, file=paste0(dataFolder, "test2.chunk6.rda"))
save(test2.chunk7, file=paste0(dataFolder, "test2.chunk7.rda"))
save(test2.chunk8, file=paste0(dataFolder, "test2.chunk8.rda"))
save(test2.chunk9, file=paste0(dataFolder, "test2.chunk9.rda"))
save(test2.chunk10, file=paste0(dataFolder, "test2.chunk10.rda"))
save(test2.chunk11, file=paste0(dataFolder, "test2.chunk11.rda"))
save(test2.chunk12, file=paste0(dataFolder, "test2.chunk12.rda"))
save(test2.chunk13, file=paste0(dataFolder, "test2.chunk13.rda"))
save(data2, file=paste0(dataFolder, "data2.rda"))
save(train3, file=paste0(dataFolder, "train_fixed.rda"))

# Load the chunks:
load(paste0(dataFolder, "test2.chunk1.rda"))
load(paste0(dataFolder, "test2.chunk2.rda"))
load(paste0(dataFolder, "test2.chunk3.rda"))
load(paste0(dataFolder, "test2.chunk4.rda"))
load(paste0(dataFolder, "test2.chunk5.rda"))
load(paste0(dataFolder, "test2.chunk6.rda"))
load(paste0(dataFolder, "test2.chunk7.rda"))
load(paste0(dataFolder, "test2.chunk8.rda"))
load(paste0(dataFolder, "test2.chunk9.rda"))
load(paste0(dataFolder, "test2.chunk10.rda"))
load(paste0(dataFolder, "test2.chunk11.rda"))
load(paste0(dataFolder, "test2.chunk12.rda"))
load(paste0(dataFolder, "test2.chunk13.rda"))
load(paste0(dataFolder, "data2.rda")) # The reduced train set
load(paste0(dataFolder, "train_fixed.rda")) # The reduced train set (train3 in memory)
load(paste0(dataFolder, "testId.rda")) # The test$Id column for sumbission file
load(paste0(dataFolder, "prediction.combined1.rda"))
load(paste0(dataFolder, "prediction.combined2.rda"))
load(paste0(dataFolder, "prediction.combined3.rda"))
load(paste0(dataFolder, "prediction.combined4.rda"))
load(paste0(dataFolder, "test.combined1.rda")) # The test chunks combined (1-3)
load(paste0(dataFolder, "test.combined2.rda")) # The test chunks combined (4-7)
load(paste0(dataFolder, "test.combined3.rda")) # The test chunks combined (8-10)
load(paste0(dataFolder, "test.combined4.rda")) # The test chunks combined (11-14)

# TODO: Find the length of unique values in all cols:
length(unique(data2$I9))

# TODO: Does the C-vars (the hashed categorical values) need to stay categorical?? I.e convert to factor??)

COR <- cor(data2[, c(2,42:80)])
size <- dim(data2[, c(2,42:80)])[2]
image(x=seq(size), y=seq(size), z=abs(COR), xlab="", ylab="",
      cex.axis=.6, cex.lab=.8, cex.main=1, main="Correlation Matrix", xaxt="n", yaxt="n")
axis(side=1, at=seq(1, size), labels=names(data2[, c(2,42:80)]), las=2, cex.axis=.6)
axis(side=2, at=seq(1, size), labels=names(data2[, c(2,42:80)]), las=2, cex.axis=.6)
text(expand.grid(x=seq(size), y=seq(size)), labels=round(c(COR), 2), cex=.6)
box() # Draw a box around the image

COR <- cor(train3[, c(3:41)])
size <- dim(train3[, c(3:41)])[2]
image(x=seq(size), y=seq(size), z=abs(COR), xlab="", ylab="",
      cex.axis=.6, cex.lab=.8, cex.main=1, main="Correlation Matrix", xaxt="n", yaxt="n")
axis(side=1, at=seq(1, size), labels=names(train3[, c(3:41)]), las=2, cex.axis=.6)
axis(side=2, at=seq(1, size), labels=names(train3[, c(3:41)]), las=2, cex.axis=.6)
text(expand.grid(x=seq(size), y=seq(size)), labels=round(c(COR), 2), cex=.6)
box() # Draw a box around the image

# Tip: Use corrplot from package corrplot
library(corrplot)
corrplot(COR, order="hclust")

# Get the variance. Remove predictors with zero variance (none here, though):
variance <- sapply(train3[, c(-1,-2)], var)
plot(variance, pch=16, col="blue", main="Variance", cex.lab=.8, cex.axis=.8)

plot(data$I13 ~ data$I4)
plot(data$I11 ~ data$I7)

# Skewed stuff! Do some log transforms to prevent bias?
# http://stats.stackexchange.com/questions/18480/interpretation-of-log-transformed-predictor
data <- train3
par(mfrow=c(1,2))
boxplot(data$I4, col="orange", main="I4")
boxplot(log(data$I4), col="cornflowerblue", main="I4")
boxplot(data$C5, col="orange", main="C5")
boxplot(log(data$C5), col="cornflowerblue", main="C5")
boxplot(data$C6, col="orange", main="I6")
boxplot(log(abs(data$C6)+0.1), col="cornflowerblue", main="I6")
hist(data$I4, col="orange", main="I4")
hist(log(data$I4), col="cornflowerblue", main="I4")
hist(data$I5, col="orange", main="I5")
hist(log(data$I5), col="cornflowerblue", main="I5")
hist(data$I6, col="orange", main="I6")
hist(log(data$I6), col="cornflowerblue", main="I6")
hist(data$I7, col="orange", main="I7")
hist(log(data$I7), col="cornflowerblue", main="I7")
par(mfrow=c(1,1))

barplot(c(length(data$I1[data$I1 <= mean(data$I1)]), length(data$I1)), col=c("green4","green2"), main="I1")
barplot(c(length(data$I1[data$I2 <= mean(data$I2)]), length(data$I2)), col=c("green4","green2"), main="I2")
barplot(c(length(data$I3[data$I3 <= mean(data$I3)]), length(data$I3)), col=c("green4","green2"), main="I3")
barplot(c(length(data$I3[data$C11 <= mean(data$C11)]), length(data$C11)), col=c("green4","green2"), main="C11")
barplot(c(length(data$I3[data$C14 <= mean(data$C14)]), length(data$C14)), col=c("green4","green2"), main="C14")

data.train <- data2[1:round(nrow(data2)/2), ]
data.test <- data2[(round(nrow(data2)/2)+1):nrow(data2), ]

# -----------------------------------------------------------------------------------------------------------------------------

# Finding the predictors with less than 10000 values:
predictors <- numeric(0)

for (counter in 3:length(names(data2))) {
  predictors[counter - 2] <- length(unique(data2[, counter]))
}
predictors.df <- data.frame(cols=names(data2)[3:length(names(data2))], length=predictors)
predictors.df <- predictors.df[which(grepl(".new", predictors.df$cols)), ]
predictors.df <- predictors.df[predictors.df$length < 10000, ]
cols <- c(as.character(predictors.df$cols))

fit0 <- glm(data2$Label ~ ., data=data2[, cols], family=binomial(link="logit"))
summary(fit9)

fit1 <- glm(data2$Label ~ ., data=data2[, c(42:80)], family=binomial(link="logit"))
summary(fit1)

fit2 <- glm(data2$Label ~ ., data=data2[, c("I6.new", "I11.new", "I7.new","I1.new","I13.new","C14.new","I3.new",
                                            "I5.new","C17.new", "C23.new")],
            family=binomial(link="logit"))
summary(fit2)

fit3 <- glm(train3$Label ~ ., data=train3[, c("I6","I11","I7","I1","I13","C14","I3",
                                            "C17","C23","I4","I12","C26", #"C25",
                                            "I8","I2","C9","C2","C16","C22")],
            family=binomial(link="logit"))
summary(fit3)

fit4 <- glm(Label ~ I3.new + I1.new + I6.new + C14.new + C17.new, data=data2[, -1],
            family=binomial(link="logit"))
# skip I7+I11 (bias is the problem?)
summary(fit4)

# I6.new   I6.new 24.1804921542352389
# I11.new I11.new 23.2946413219162878
# I7.new   I7.new 10.9106510976025000
# I1.new   I1.new 10.2585052995323949
# I13.new I13.new  7.5394481466735419
# C14.new C14.new  6.7166140044551756
# I3.new   I3.new  3.1953816918927154
# I5.new   I5.new  2.4564897557081378
# C17.new C17.new  2.0054414663608888
# C23.new C23.new  1.5846872792081941
# I4.new   I4.new  1.3778081178513615
# I12.new I12.new  1.1553783007286740
# C26.new C26.new  1.0455633096022989
# C20.new C20.new  0.7137528784301128
# C25.new C25.new  0.6355941328104230
# I9.new   I9.new  0.6156710232074881
# I8.new   I8.new  0.5563883632714262
# I2.new   I2.new  0.4317360018231731
# C9.new   C9.new  0.3770592471584626
# C2.new   C2.new  0.3429159039146022
# C16.new C16.new  0.2765662732932782
# C19.new C19.new  0.1535419856827282
# C22.new C22.new  0.1133663503765097
# C6.new   C6.new  0.0623058942643859

names(test.combined1)[41:79] <- names(train3[3:41])
test.combined11 <- test.combined1[, -c(2:40)]
result1 <- predict(fit3, newdata=test.combined11, type="response")
result2 <- predict(fit3, newdata=test.combined2, type="response")
result3 <- predict(fit3, newdata=test.combined3, type="response")
result4 <- predict(fit3, newdata=test.combined4, type="response")
result1[1:100]

pred.total <- c(result1, result2, result3, result4)
length(pred.total)
min(pred.total)
max(pred.total)

options(digits=15)
submission=data.frame(Id=test.id, Predicted=pred.total)
head(submission)
tail(submission)
dim(submission)
write.csv(submission, file=paste0(submissionsFolder, "LM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ------------------------------------------------------------------------------------------------------------------------------

require(gbm)

# GBM model settings, these can be varied 
GBM_NTREES = 500
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 4 
GBM_MINOBS = 50

# Build the GBM model 
GBM_model <- gbm.fit(x=data2[, c("I6.new", "I11.new", "I7.new","I1.new","I13.new","C14.new","I3.new","I5.new","C17.new","C23.new")],
                     y=data2$Label, distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=TRUE) 
GBM_model <- gbm.fit(x=data2[, c(42:80)],
                     y=data2$Label, distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=TRUE)
GBM_model <- gbm.fit(x=data2[, c(-1,-2)], y=data2$Label, distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=TRUE) 

save(GBM_model, file=paste0(dataFolder, "GBM_model.rda"))
load(paste0(dataFolder, "GBM_model.rda"))

# List variable importance 
summary(GBM_model, GBM_NTREES, main="GBM", cex.axis=.8, cex.lab=.8, las=1) 

# I6.new   I6.new 24.1804921542352389
# I11.new I11.new 23.2946413219162878
# I7.new   I7.new 10.9106510976025000
# I1.new   I1.new 10.2585052995323949
# I13.new I13.new  7.5394481466735419
# C14.new C14.new  6.7166140044551756
# I3.new   I3.new  3.1953816918927154
# I5.new   I5.new  2.4564897557081378
# C17.new C17.new  2.0054414663608888
# C23.new C23.new  1.5846872792081941
# I4.new   I4.new  1.3778081178513615
# I12.new I12.new  1.1553783007286740
# C26.new C26.new  1.0455633096022989
# C20.new C20.new  0.7137528784301128
# C25.new C25.new  0.6355941328104230
# I9.new   I9.new  0.6156710232074881
# I8.new   I8.new  0.5563883632714262
# I2.new   I2.new  0.4317360018231731
# C9.new   C9.new  0.3770592471584626
# C2.new   C2.new  0.3429159039146022
# C16.new C16.new  0.2765662732932782
# C19.new C19.new  0.1535419856827282
# C22.new C22.new  0.1133663503765097
# C6.new   C6.new  0.0623058942643859

# Predict for the leaderboard data 
prediction_GBM.combined1 <- predict.gbm(object=GBM_model,
    newdata=test.combined1[, c("I11.new", "I6.new", "I7.new","I13.new","I1.new","C14.new","I3.new","C17.new")],
    GBM_NTREES)
prediction_GBM.combined1.scaled <- plogis(prediction_GBM.combined1)

prediction_GBM.combined2 <- predict.gbm(object=GBM_model,
    newdata=test.combined2[, c("I11.new", "I6.new", "I7.new","I13.new","I1.new","C14.new","I3.new","C17.new")],
    GBM_NTREES)
prediction_GBM.combined2.scaled <- plogis(prediction_GBM.combined2)

prediction_GBM.combined3 <- predict.gbm(object=GBM_model,
    newdata=test.combined3[, c("I11.new", "I6.new", "I7.new","I13.new","I1.new","C14.new","I3.new","C17.new")],
    GBM_NTREES)
prediction_GBM.combined3.scaled <- plogis(prediction_GBM.combined3)

prediction_GBM.combined4 <- predict.gbm(object=GBM_model,
    newdata=test.combined4[, c("I11.new", "I6.new", "I7.new","I13.new","I1.new","C14.new","I3.new","C17.new")],
    GBM_NTREES)
prediction_GBM.combined4.scaled <- plogis(prediction_GBM.combined4)


# http://stackoverflow.com/questions/8410846/r-gbm-logistic-regression
# p <- plogis(prediction)

pred.total <- c(prediction_GBM.combined1.scaled, prediction_GBM.combined2.scaled,
                prediction_GBM.combined3.scaled, prediction_GBM.combined4.scaled)
length(pred.total)
min(pred.total)
max(pred.total)

options(digits=15)
submission=data.frame(Id=test.id, Predicted=pred.total)
head(submission)
tail(submission)
dim(submission)
write.csv(submission, file=paste0(submissionsFolder, "GBM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------------

package.install("LiblineaR")
library(LiblineaR)

# NOTE: Classification models usually perform better if each dimension of the data is first centered and scaled.
# http://rtutorialseries.blogspot.no/2012/03/r-tutorial-series-centering-variables.html

# http://www.inside-r.org/packages/cran/LiblineaR/docs/predict.LiblineaR

#model <- LiblineaR(data.train[, c(-1,-2)], data.train$Label, type=0, cost=1, epsilon=0.01, #type=0: L2-regularized
#          bias=T, wi=NULL, cross=0, verbose=F)
#model <- LiblineaR(data2[, c(42:80)], data2$Label, type=0, cost=1, epsilon=0.01, #type=0: L2-regularized
#                   bias=T, wi=NULL, cross=0, verbose=F)
model <- LiblineaR(train3[, c(3:41)], train3[, 2], type=0, cost=1, epsilon=0.01, #type=0: L2-regularized
                   bias=T, wi=NULL, cross=0, verbose=F)
summary(model)
#prediction1 <- predict(model, test2.chunk1, proba=T, decisionValues=T)

test.combined1 <- rbind(test2.chunk1, test2.chunk2, test2.chunk3) # Bigger test set, more accuracy?
save(test.combined1, file=paste0(dataFolder, "test.combined1.rda"))

test.combined2 <- rbind(test2.chunk4, test2.chunk5, test2.chunk6) # Bigger test set, more accuracy?
save(test.combined2, file=paste0(dataFolder, "test.combined2.rda"))

test.combined3 <- rbind(test2.chunk7, test2.chunk8, test2.chunk9) # Bigger test set, more accuracy?
save(test.combined3, file=paste0(dataFolder, "test.combined3.rda"))

test.combined4 <- rbind(test2.chunk10, test2.chunk11, test2.chunk12, test2.chunk13) # Bigger test set, more accuracy?
save(test.combined4, file=paste0(dataFolder, "test.combined4.rda"))

# Fix the names and remove duplicate cols in test.combined<n> sets:
names(test.combined1)[41:79] <- names(train3[3:41])
test.combined11 <- test.combined1[, -c(2:40)]
names(test.combined2)[41:79] <- names(train3[3:41])
test.combined12 <- test.combined2[, -c(2:40)]
names(test.combined3)[41:79] <- names(train3[3:41])
test.combined13 <- test.combined3[, -c(2:40)]
names(test.combined4)[41:79] <- names(train3[3:41])
test.combined14 <- test.combined4[, -c(2:40)]


prediction.combined1 <- predict(model, test.combined1[, c(41:79)], proba=T, decisionValues=T)
#prediction.combined1 <- predict(model, test.combined11[, -1], proba=T, decisionValues=T)
save(prediction.combined1, file=paste0(dataFolder, "prediction.combined1.rda"))
prediction.combined2 <- predict(model, test.combined2[, c(41:79)], proba=T, decisionValues=T)
save(prediction.combined2, file=paste0(dataFolder, "prediction.combined2.rda"))
prediction.combined3 <- predict(model, test.combined3[, c(41:79)], proba=T, decisionValues=T)
save(prediction.combined3, file=paste0(dataFolder, "prediction.combined3.rda"))
prediction.combined4 <- predict(model, test.combined4[, c(41:79)], proba=T, decisionValues=T)
save(prediction.combined4, file=paste0(dataFolder, "prediction.combined4.rda"))

prediction.combined1$predictions[1:25]
prediction.combined1$probabilities[,2][1:25]
min(prediction.combined1$probabilities[,2])
max(prediction.combined1$probabilities[,2])

# 0: prediction.combined4$probabilities[13,2], 1: prediction.combined4$probabilities[13,1]
pred.total <- c(prediction.combined1$probabilities[,2], prediction.combined2$probabilities[,2],
                prediction.combined3$probabilities[,2], prediction.combined4$probabilities[,2])
#pred.total <- c(prediction.combined1$predictions, prediction.combined2$predictions,
#                prediction.combined3$predictions, prediction.combined4$predictions)
length(pred.total)
min(pred.total) # 5.78547445387878e-05 # 1.12524464948027e-05
max(pred.total) # 0.999815955731271 # 0.998787792257428

#table(prediction1$predictions == prediction2$predictions)

# TODO: Use scale() function with centered?T instead of your own Normalize()?
# TODO: Check the data2[, c(42:80)] set with gbm to find variable importance

options(digits=15)
submission=data.frame(Id=test.id, Predicted=pred.total)
head(submission)
tail(submission)
dim(submission)
write.csv(submission, file=paste0(submissionsFolder, "LiblineaR_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
# LiblineaR gives best score: 0.54236

# To ZIP the file before upload: http://stat.ethz.ch/R-manual/R-devel/library/utils/html/zip.html
