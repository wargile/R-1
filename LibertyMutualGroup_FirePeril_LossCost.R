# Liberty Mutual Group - Fire Peril Loss Cost
# -------------------------------------------

# http://www.kaggle.com/c/liberty-mutual-fire-peril/data
# Deadline: Sept 2 2014

# TIPS:
# http://www.kaggle.com/c/liberty-mutual-fire-peril/forums/t/9837/dealing-with-na-values/51104#post51104
# http://www.kaggle.com/c/liberty-mutual-fire-peril/forums/t/10194/after-shock-let-s-talk-about-solutions
# https://github.com/wallinm1/kaggle-liberty-challenge/blob/master/train.py

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/Liberty MutualGroup_FirePeril_LossCost/data/"
codeFolder <- "C:/coding/Kaggle/Liberty MutualGroup_FirePeril_LossCost/code/"
submissionsFolder <- "C:/coding/Kaggle/Liberty MutualGroup_FirePeril_LossCost/submissions"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=T)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=T)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

dim(train)
dim(test)
describe(train) # slow on big datasets...
str(train)
summary(train)

head(train)
head(test)

CleanData <- function(data, start.col) {
  # Fix NA's (set to 0) in all numeric/integer cols:
  cols <- dim(data)[2]
  rows <- dim(data)[1]
  
  # Fix NA's
  # For other ways see:
  # http://www.kaggle.com/c/liberty-mutual-fire-peril/forums/t/9837/dealing-with-na-values/51104#post51104
  for (counter in start.col:cols) {
    if (sapply(data[counter], class) != "factor") {
      na.cols <- which(is.na(data[, counter]))
      data[na.cols, counter] <- 0.0
      #data[na.cols, counter] <- median(data[, counter], na.rm=T) # imputing with median worsens score!
    } else {
      #na.cols <- which(data[, counter] == "Z")
      #data[na.cols, counter] <- NA
      #data[, counter] = as.integer(data[, counter])
      #data[na.cols, counter] <- median(data[, counter], na.rm=T) # imputing with median worsens score!
    }
  }
  
  return(data)
}

train <- CleanData(train, 3)
test <- CleanData(test, 2)

save(train, file=paste0(dataFolder, "trainCleaned.rda"))
save(test, file=paste0(dataFolder, "testCleaned.rda"))

# -----------------------------------------------------------------------------------------------------------------
# NOTE: Start from here after loading file

load(paste0(dataFolder, "trainCleaned.rda"))
load(paste0(dataFolder, "testCleaned.rda"))

# TODO/TEST: Convert the var<n> nominal/ordinal variables: Replace Z so that we get A,B,C,D instead of A,B,C,Z?

par(cex.lab=.8)
par(cex.axis=.7)
par(cex.main=1)
par(cex=1)
par(mar=c(4.8,4.2,2.5,.8))

# IMPORTANT: Log-tranform the outcome variable
par(mfrow=c(1,2))
plot(train$target[train$target > 0], main="Target", col="red", ylab="Target")
plot(log(train$target[train$target > 0]), main="Log Target", col="blue", ylab="Log(target)")
par(mfrow=c(1,1))
par(mfrow=c(1,2))
hist(train$target[train$target > 0.1 & train$target < 17], main="Target", col="red", ylab="Target")
hist(log(train$target[train$target > 0.1 & train$target < 17]), main="Log Target", col="blue", ylab="Log(target)")
par(mfrow=c(1,1))
par(mfrow=c(1,2))
plot(sort(train$target[train$target > 0.1 & train$target < 17]), col="red", main="Target")
plot(sort(log(train$target[train$target > 0.1 & train$target < 17])), col="blue", main="Log(target)")
par(mfrow=c(1,1))
# Get a more random/representative subset to maximize variance in predictors
# train2 <- train[sample(1:nrow(train), nrow(train) / 3, replace=F), ]
result <- CreateTrainAndVerificationSets(train) 
train.subset <- result$train
verification.subset <- result$verification
dim(train.subset)
dim(verification.subset)

# TODO: Find out which cols have less than 10 unique values - convert to factor cols?
unique.lengths <- integer(0)
for (counter in 1:ncol(train))
  unique.lengths[counter] <- length(unique(train[, counter]))
which(unique.lengths <= 10)

# Show factor cols
names(train)[sapply(train, function(x) is.factor(x))]

# Correlation
CorrelationPlot(train[2:50])
CorrelationPlot(train[c(2,51:100)])
CorrelationPlot(train[c(2,101:150)])
CorrelationPlot(train[c(2,151:200)])
CorrelationPlot(train[c(2,201:250)])
CorrelationPlot(train[c(2,251:302)])

# Do some exploratory plots of factor predictors:
# Only var4 seems to have importance, any point in imputing those with a lot of NA's?
par(mfrow=c(1,2))
plot(train$var1, col="cornflowerblue", main="train$var1", cex.axis=.8, cex=.8, las=1) # LOTS of NA's (Z)! Try to impute?
plot(test$var1, col="orange", main="test$var1", cex.axis=.8, cex=.8, las=1)

median(as.integer(train$var1[which(train$var1 != 'Z')])) # Find median value that is not the NA/Z value

plot(train$var2, col="cornflowerblue", main="train$var2", cex.axis=.8, cex=.8, las=1) # LOTS of NA's (Z)! Try to impute?
plot(test$var2, col="orange", main="test$var2", cex.axis=.8, cex=.8, las=1)

plot(train$var3, col="cornflowerblue", main="train$var3", cex.axis=.8, cex=.8, las=1) # LOTS of NA's (Z)! Try to impute?
plot(test$var3, col="orange", main="test$var3", cex.axis=.8, cex=.8, las=1)

plot(train$var4, col="cornflowerblue", main="train$var4", cex.axis=.8, cex=.6, las=2) # Lots of category H1 
plot(test$var4, col="orange", main="test$var4", cex.axis=.8, cex=.6, las=2)
# TODO: Use Amelia to impute? TODO: Convert to integer after setting Z to NA first
# Try my own impute stuff (sloooooow.... actually, hopeless, take mean() of each category and compare instead):
# source("impute.R")
# na.val <- "Z"
# imputeData <- train[sample(1:nrow(train), 20, replace=T), ]
# which(imputeData$var4 == na.val)
# result <- impute(imputeData, 6, na.val)

plot(train$var4, col="cornflowerblue", main="train$var4", cex.axis=.8, cex=.6, las=2) # Lots of category H1
points(imputeData$var4, pch=16, col="blue")
plot(imputeData$var4, col="orange", main="imputeData$var4", cex.axis=.8, cex=.6, las=2)

plot(train$var5, col="cornflowerblue", main="train$var5", cex.axis=.8, cex=.6, las=1) # LOTS of NA's (Z)! Try to impute?
plot(test$var5, col="orange", main="test$var5", cex.axis=.8, cex=.6, las=1)

plot(train$var6, col="cornflowerblue", main="train$var6", cex.axis=.8, cex=.6, las=1) # LOTS of NA's (Z)! Try to impute?
plot(test$var6, col="orange", main="test$var6", cex.axis=.8, cex=.6, las=1)

plot(train$var7, col="cornflowerblue", main="train$var7", cex.axis=.8, cex=.6, las=1) # Very complete, few NA's
plot(test$var7, col="orange", main="test$var7", cex.axis=.8, cex=.6, las=1)

plot(train$var8, col="cornflowerblue", main="train$var8", cex.axis=.8, cex=.6, las=1) # Very complete, few NA's
plot(test$var8, col="orange", main="test$var8", cex.axis=.8, cex=.6, las=1)

plot(train$var9, col="cornflowerblue", main="train$var9", cex.axis=.8, cex=.6, las=1) # 1/3 NA's
plot(test$var9, col="orange", main="test$var9", cex.axis=.8, cex=.6, las=1)
par(mfrow=c(1,1))

boxplot(train$target ~ train$var1, col="blue")
boxplot(train$target ~ train$var4, col="blue", las=2, cex.axis=.7)
boxplot(train$target ~ train$var3, col="blue")
boxplot(log(train$target + 0.1) ~ train$var3, col="blue")

# Some histograms, notice skewness:
oldmar=par()$mar
par(mar=c(3,3,2,1))
par(mfrow=c(2,1))
hist(train$geodemVar1, col="orange", main="geoDemVar1", cex.axis=.8, cex=.6, cex.lab=.8, cex.main=1)
abline(v=mean(train$geodemVar1), col="red", lwd=1)
abline(v=median(train$geodemVar1), col="green", lwd=1)
hist(train$geodemVar9, col="wheat", main="geoDemVar9", cex.axis=.8, cex=.6, cex.lab=.8, cex.main=1)
abline(v=mean(train$geodemVar9), col="red", lwd=2)
abline(v=median(train$geodemVar9), col="green", lwd=1)
par(mfrow=c(1,1))
par(mar=oldmar)

# Various ways to check for outliers and correlation
plot(train$var15)
plot(test$var15)
quantile(train$var15)
quantile(test$var15)
hist(test$var16, col="cyan")
hist(train$weatherVar1, col="orange")
hist(log(train$weatherVar1), col="orange")
heatmap(matrix(as.numeric(test$var6[1:100]), nrow=10))
result <- cor(train[1:1000, -c(1:20)])
result[which(is.na(result))] <- 0
image(matrix(result, nrow=282, ncol=282))
plot(train$crimeVar1 ~ train$geodemVar1, col="blue")
cor(train$geodemVar1, train$crimeVar5)
MyCorrelationCoefficient(train$geodemVar3, train$crimeVar5)

# (Could have been a) nice cor heatmap:
COR <- cor(train[, c(-1,-2,-181)])
size <- dim(train)[2]
image(x=seq(size), y=seq(size), z=abs(COR), xlab="", ylab="",
      cex.axis=.6, cex.lab=.8, cex.main=1, main="Correlation Matrix", xaxt="n", yaxt="n")
axis(side=1, at=seq(1, size), labels=names(train), las=2, cex.axis=.6)
axis(side=2, at=seq(1, size), labels=names(train), las=2, cex.axis=.6)
text(expand.grid(x=seq(size), y=seq(size)), labels=round(c(COR), 2), cex=.7)
box() # Draw a box around the image

# ----------------------------------------------------------------------------------------------------------------
# Try GBM

# START Working on the train2 subset
library(gbm)
# TODO: Filter out cols that have no variation?
lengths <- sapply(train, function(x) length(unique(x)))
lengths <- which(lengths == 1)
GBM_NTREES = 50
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 3
GBM_MINOBS = 5

if (length(lengths) > 0) {
  the.train <- train[, c(-1,-20,-lengths)]# Skip id, (-var11 (weighted GINI index var)) and dummy cols and 0-variation cols
} else {
  the.train <- train[, c(-1,-20)] # Skip id, (-var 11 (weighted GINI index var)) and dummy cols
}

# TEST: Remove all the geo columns:
names(the.train)[sapply(the.train, function(x) is.factor(x))]
the.train <- the.train[,c(-(28:299))]

# TEST: Convert the factor cols to numeric:
the.train$var1 <- as.integer(the.train$var1)
the.train$var2 <- as.integer(the.train$var2)
the.train$var3 <- as.integer(the.train$var3)
the.train$var4 <- as.integer(the.train$var4)
the.train$var5 <- as.integer(the.train$var5)
the.train$var6 <- as.integer(the.train$var6)
the.train$var7 <- as.integer(the.train$var7)
the.train$var8 <- as.integer(the.train$var8)
the.train$var9 <- as.integer(the.train$var9)

#fit <- gbm.fit(x=the.train[,-1], y=the.train$target, distribution="gaussian", 
#  n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
#  n.minobsinnode=GBM_MINOBS, verbose=TRUE)

fit <- gbm.fit(x = the.train[, -1], y=the.train$target, distribution="gaussian", n.trees=GBM_NTREES) 

# List variable importance 
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

the.test <- test[, c(-1,-19)] # Get rid of id, (-var11) and dummy cols

# TEST: Remove all the geo columns:
the.test <- the.test[, c(-(27:301))]

result1 <- predict(fit, newdata=the.test[1:100000, ], type="response", n.trees=GBM_NTREES)
result2 <- predict(fit, newdata=the.test[100001:200000, ], type="response", n.trees=GBM_NTREES)
result3 <- predict(fit, newdata=the.test[200001:300000, ], type="response", n.trees=GBM_NTREES)
result4 <- predict(fit, newdata=the.test[300001:400000, ], type="response", n.trees=GBM_NTREES)
result5 <- predict(fit, newdata=the.test[400001:dim(test)[1], ], type="response", n.trees=GBM_NTREES)
total.result <- c(result1, result2, result3, result4,result5)

old.scipen=options()$scipen
options(scipen=999)
as.numeric(result1[1:100])
options(scipen = old.scipen)
#  END Working on the train2 subset

# Create submission file:
df <- data.frame(id=test$id, target=total.result)
#names(df) <- c("id", "target") # Not needed as long as correct var names set in df
head(df)
KaggleSubmission(df, submissionsFolder, "GBM", competitionName="FirePerilLossCost", rowNames=F)

# ----------------------------------------------------------------------------------------------------------------
# TODO: Try caret package

# ----------------------------------------------------------------------------------------------------------------
# Try LM
# More lm tips and methods to try: http://www.statmethods.net/stats/regression.html

####################################
## Best subset selection
####################################
train.cols <- c("target", "var4", "var15", "var13")
# TEST some more cols:
# train.cols <- c("target", "var4", "var15", "var13", "var8", "weatherVar160", "weatherVar147")
# train.cols <- c("target", "var4", "weatherVar137", "weatherVar102", "weatherVar103", "weatherVar118", "weatherVar189")
test.cols <- c("var4", "var15", "var13")
# TEST some more cols:
# test.cols <- c("var4", "var15", "var13", "var8", "weatherVar160", "weatherVar147")
# test.cols <- c("var4", "weatherVar137", "weatherVar102", "weatherVar103", "weatherVar118", "weatherVar189")

#library(leaps)
#l <- regsubsets(x=formula('train$target[1:100] ~ .'), data=train[1:100, 3:302], y=train[1:100, 2], really.big=T)
#plot(l$size,l$r2)
#l <- leaps(train[, train.cols], train[, 2], method='Cp') # Or method=r2
#plot(l$size,l$Cp)

# TEST: Is it more efficient for LM to convert var4 (and other ordinal/nominal factors)
# into numerical equivalents? RESULT: It is not!
# train$var4 <- as.numeric(train$var4)
# test$var4 <- as.numeric(test$var4)

# TEST: Impute the Z-values (NA) in var4 with median(var4):
# train$var4[train$var4 == "Z"] <- median(train$var4)
# test$var4[test$var4 == "Z"] <- median(test$var4)

# TEST: Get rid of outliers (Result: Worsens score!):
# train.testings <- train[which(train$target < 5), ]
# fit <- lm(target ~ var15 + var13 + var4, data=train.testings, na.action=na.exclude)


# Trying some anova comparisons:
fit0 <- lm(target ~ var11 + var15 + var13 + var4 + crimeVar4 + weatherVar1 + geodemVar1,
           data=train, na.action=na.exclude)
summary(fit0)
fit1 <- lm(target ~ var15 + var13, data=train, na.action=na.exclude)
summary(fit1)
fit2 <- lm(target ~ var15 + var13 + crimeVar3 + crimeVar4, data=train, na.action=na.exclude)
summary(fit2)
anova(fit1, fit2)

fit <- lm(target ~ ., data=train[, train.cols], na.action=na.exclude)
summary(fit)
anova(fit)
plot(coefficients(fit), pch=19, col="blue", main="fit$coeff", xaxt="n", cex=.8, cex.lab=.8,
     xlab="")
axis(1, las=2, at=1:length(coefficients(fit)), labels=names(coefficients(fit)), cex.axis=.7)
abline(v=1:length(coefficients(fit)), col="gray", lty=3)
confint(fit, level=0.95)
vcov(fit)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)
par(mfrow=c(1,1))

result <- predict(fit1, newdata=test[, test.cols], type="response")
old.scipen=options()$scipen
options(scipen=999)
as.numeric(result[1:100])
plot(as.numeric(result[1:100]), pch=19, col="blue", cex=.8, cex.axis=.8, cex.lab=.8, cex.main=1,
     main=paste0("LM fit (min=", round(min(result), 5), ", max=", round(max(result), 5), ")"),
     xlab="Elements", ylab="Fit")
lines(as.numeric(result[1:100]), col="gray")
abline(h=0, col="gray", lty=3)
options(scipen = old.scipen)

# TODO: Do CV cross-validation:
# package.install("DAAG")
library(DAAG)
cv.result <- cv.lm(df=train[, train.cols], fit, m=3) # 3 fold cross-validation
# http://www.unt.edu/rss/class/Jon/R_SC/Module9/CrossValidation/Cross_Validation_1.R
library(boot)
val.10.fold <- cv.glm(data=train[, train.cols], glmfit=fit, K=10)

# NOTE: Set negative results to 0, creates the tiniest improvement.
# NOTE: IMPORTANT: Do NOT do other type of rounding/cutoffs! Creates BAD scores!
result[which(result < 0)] <- 0
max(result)

# Create submission file:
df <- data.frame(id=test$id, target=result)
head(df)
KaggleSubmission(df, submissionsFolder, "LM", competitionName="FirePerilLossCost")
# LM gives the best score so far: 0.36296


# ----------------------------------------------------------------------------------------------------------------------
# Try RLM (Mass package)

fit2 <- rlm(target ~ ., data=train[, train.cols], na.action=na.exclude, psi=psi.hampel, init="lts")
fit2 <- rlm(target ~ ., data=train[, train.cols], na.action=na.exclude)
# See also ?lqs
summary(fit2)

result2 <- predict(fit2, newdata=test[, test.cols], type="response")
options(scipen=999)
as.numeric(result2[1:100])

# NOTE: Set negative results to 0, creates the tiniest improvement.
result2[which(result2 < 0)] <- 0

# Create submission file:
df <- data.frame(id=test$id, target=result2)
head(df)
KaggleSubmission(df, submissionsFolder, "RLM", competitionName="FirePerilLossCost", rowNames=F)

# ----------------------------------------------------------------------------------------------------------------------
# --- START LM NORMALIZED TEST (NOT THE WAY TO GO!!!) ---
# TEST: Set a cutover value, and if predval < CUTOVER_VAL set predval to 0 (train set has a lot of 0-values!)??
# TEST: Normalize (and also log10) the result to make the predicted values more in sync with train$target if above 0??
result.test <- result
result.test[which(result.test < 0.018)] <- 0.0 # Unsure - can have negative positive (< 0) trend too?
result.test.normalized <- Normalize(min(result.test[which(result.test > 0)]), max(result.test), 0, 15, result.test)
result.test.normalized[which(result.test.normalized < 0)] <- 0
plot(unique(result.test.normalized), col="blue")
result.test.log <- (log(result.test.normalized + 1))
result.test.normalized.log <- Normalize(min(result.test.log),
                                        max(result.test.log), 0, 15, result.test.log)
plot(unique(result.test.normalized), col="blue", main="Predicted values, normalized",
     cex=.8, cex.axis=.8, cex.lab=.8, cex.main=1)
plot(unique(result.test.normalized.log), col="green4", main="Predicted values, normalized+log",
     cex=.8, cex.axis=.8, cex.lab=.8, cex.main=1)
plot(unique(train$target), col="red", main="train$target",
     cex=.8, cex.axis=.8, cex.lab=.8, cex.main=1)

# Create TEST submission file:
df <- data.frame(id=test$id, target=result.test.normalized)
head(df)
KaggleSubmission(df, submissionsFolder, "LM_Normalized", competitionName="FirePerilLossCost", rowNames=F)
# --- END LM NORMALIZED TEST (NOT THE WAY TO GO!!!) ---


# -------------------------------------------------------------------------------------------------------------------
# Using Ridge regression
# http://mason.gmu.edu/~csutton/tactR789cr.txt
# http://cran.r-project.org/web/packages/parcor/parcor.pdf

require(MASS)

train.cols <- c("target", "var4", "var15", "var13")
test.cols <- c("var4", "var15", "var13")

# TEST: Convert all to numeric:
train.subset <- train[, train.cols]
train.subset$var4 <- as.integer(train.subset$var4)
test.subset <- test[, test.cols]
test.subset$var4 <- as.integer(test.subset$var4)

unique(is.finite(as.matrix(train.subset))) # Note: All vars must be numerical!
fit <- lm.ridge(target ~ var4 + var13 + var15, data=train.subset, lambda=seq(0,0.1,0.01), model=F, x=F)
fit$kHKB
fit$kLW
fit$GCV
plot(fit$GCV)
summary(fit)

plot(lm.ridge(target ~ var4 + var13 + var15, train.subset, lambda = seq(0,0.1,0.01)))
y.pred.ridge <- scale(test.subset, center=T, scale=fit$scales) %*% fit$coef[, which.min(fit$GCV)] + fit$ym
y.pred.ridge[1:100]
#summary((y.pred.ridge - test$target)^2)


# ----------------------------------------------------------------------------------------------------------------
# Try GBM with just a few cols

train.cols <- c("target", "var4", "var15", "var13")
test.cols <- c("var4", "var15", "var13")
train2 <- train[, train.cols]
test2 <- test[, test.cols]

GBM_NTREES = 500 # 1000
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 6
GBM_MINOBS = 50

fit <- gbm(target ~ ., data=train2, distribution="gaussian",
           n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
           n.minobsinnode=GBM_MINOBS, verbose=T, cv.folds=10)

best.cv.iter <- gbm.perf(fit, method="cv")

result <- predict(fit, newdata=test[, test.cols], type="response", best.cv.iter)
result[1:100]

# Show inportance plot
result <- summary(fit)
result
barplot(result$rel.inf[result$rel.inf > 0.0], col=c(2:20))
labs <- rownames(result)[which(result$rel.inf > 0.0)]
axis(1, at=1:length(labs), labels=labs)

# Create submission file:
df <- data.frame(id=test$id, target=total.result)
head(df)
KaggleSubmission(df, submissionsFolder, "GBM_ReducedCols", competitionName="FirePerilLossCost", rowNames=F)


# ----------------------------------------------------------------------------------------------------------------
# Do Random forest and find importance
library(randomForest)

par(mar=c(5,4.5,3,1))
Sys.time()
pos <- 1
result <- integer()

trainSubset <- sample(train[, train.cols], 10000, replace=T)
testSubset <- sample(train[, train.cols], 10000, replace=T)

for (counter in seq(1, 25, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(trainSubset$target ~ ., trainSubset, proximity=TRUE, keep.forest=TRUE, ntree=counter)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=testSubset, type="response")
  the.result <- (prediction == testSubset$target)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()

best.ntrees <- which(result == min(result))
# TODO: Then train with best ntrees value


# -----------------------------------------------------------------------------------------------------------
# Do rpart - not really usable here? Better for fewer outcomes/classification tasks?
library(rpart)

train.cols <- c("target", "var4", "var15", "var13")
test.cols <- c("var4", "var15", "var13")

fit <- rpart(target ~., data=train[, train.cols], method="anova")
printcp(fit)  # display cp table
plotcp(fit)  # plot cross-validation results
rsq.rpart(fit)
# plot approximate R-squared and relative error for different splits (2 plots).
# labels are only appropriate for the "anova" method.
prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

result <- predict(fit, newdata=test[, test.cols])
result[1:100]

# ------------------------------------------------------------------------------------------------------------
# Do caret
package.install("caret")
library(caret)

rows <- sample(nrow(train), 1000, replace=F)
trainSubset <- train[rows, train.cols]
rows <- sample(nrow(test), 1000, replace=F)
testSubset <- train[rows, train.cols]

trainSubset$var4 <- as.integer(trainSubset$var4)
testSubset$var4 <- as.integer(testSubset$var4)

ctrl <- trainControl(method="repeatedcv", number=10, repeats=10, savePred=T)
model <- train(y=trainSubset$target, x=trainSubset[-1], method="lm", trControl=ctrl)
model$pred

# -------------------------------------------------------------------------------------------------------------
# Try h2o...
# http://docs.0xdata.com/
package.install("h2o")
suppressMessages(library(h2o))
library(mlbench) # For testing on datasets in example referenced at http://blenditbayes.blogspot.co.uk/

#cols <- c(which(names(train) %in% names(influence)[influence > 15]), 53)

train.cols <- c("target", "var4", "var15", "var13")
test.cols <- c("var4", "var15", "var13")

localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, Xmx='2g')
dat_h2o <- as.h2o(localH2O, train[, train.cols], key='train')
dat_h2o.test <- as.h2o(localH2O, test[, test.cols], key='test')

model <- 
  h2o.deeplearning(x=2:4,   # column numbers for predictors
                   y=1,   # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   activation="TanhWithDropout", # or 'Tanh'
                   classification=F,
                   input_dropout_ratio=0.2, # % of inputs dropout
                   hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=T,
                   # l2, # TODO: How to set L1 or L2 regularization?
                   hidden=c(50, 50, 50), # three layers of 50 nodes
                   epochs=100) # max. no. of epochs

h2o_yhat_test <- h2o.predict(model, dat_h2o.test)
df_yhat_test <- as.data.frame(h2o_yhat_test)
table(df_yhat_test$predict)
# ERROR!

submission <- data.frame(EventID=eventId, RankOrder=1:length(eventId), Class=df_yhat_test$predict)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "h2o_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
