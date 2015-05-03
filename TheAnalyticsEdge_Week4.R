# The Analytics Edge week 4 - CART and Random Forests
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/cart-and-random-forests/
# ---------------------------------------------------------------------------------------------

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)


SetStandardOptions()
# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")
options(digits=7)

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 4/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) JUDGE, JUDGE, JURY AND CLASSIFIER - An Introduction to Trees:
# -------------------------------------------------------------
# Predicting of SCOTUS (US Supreme Court) outcome (Martin 2002). Data from 1994 to 2001.
# Rare dataset - longest period of time with the same set of justices in 180 years.
# Used Classification And Regression Trees (CART) rather than logistic regression, since latter results
# are more easily interpretable.

# "Minbucket" size in R: The number of points in each subset/split. Too small: Overfitting danger. Too large: Innacurate splits

# In each bucket/subset, compute the percentage of data in a subset of each type 
# Example: 10 affirm, 2 reverse -> 10/(10+2) = 0.87
# With a threshold of 0.5, we would pick Affirm (choosing most frequent outcome). But with threshold = 0.9, we would pick Reverse.

stevens <- read.csv(paste0(folder, "stevens.csv"))
# Data from: http://wusct.wustl.edu/data.php
str(stevens)
# NOTE: Docket: Unique id for each case.
pairs(stevens)
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio=0.7)
train <- subset(stevens, spl == T)
test <- subset(stevens, spl == F)
package.install("rpart.plot")
library(rpart.plot)
StevensTree <- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                     data=train, method="class", minbucket=25) # NOTE: minbucket param!
summary(StevensTree)
prp(StevensTree, cex=.7, col="blue")
PredictCart <- predict(StevensTree, newdata=test, type="class")
result <- table(test$Reverse, PredictCart)
result
score <- (result[1] + result[4]) / sum(result) # Diagonal of correct outcomes (predicted 0, affirm)
score
PredictROC <- predict(StevensTree, newdata=test)
PredictROC # Prob of outcome 0 and outcome 1. Note row numbers: Each test set observation is classified into a
# subset, or bucket, of our cart tree. These numbers give the percentage of training set data in that subset
# with outcome 0, and the percentage of training set data in that subset with outcome 1.
pred <- prediction(PredictROC[,2], test$Reverse) # PredictROC[,2] = outcome 1 column
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# Random Forest, how does it work:
data <- c(1,2,3,4,5) # Our total dataset
first.cart.tree <- sample(data, 5, replace=T)
second.cart.tree <- sample(data, 5, replace=T) # etc., this is the "ntree" parameter, the total "forest"
# "nodesize" param is the mininimum number of observations in each subset. Smaller nodesize = longer time
# ntree param should not be too small, as bagging procedure may miss observations. But more trees take longer
# to build.
train$Reverse <- as.factor(train$Reverse) # NOTE: to specify for rf that we want classification, not regression
test$Reverse <- as.factor(test$Reverse)
StevensForest <- randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                              data=train, nodesize=25, ntree=200) # nodesize=minbucket size for CART
PredictForest <- predict(StevensForest, newdata=test)
result <- table(test$Reverse, PredictForest)
result
score <- (result[1] + result[4]) / sum(result) # Diagonal of correct outcomes (predicted 0, affirm)
score

# Cross-validation:
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
# http://cran.r-project.org/web/packages/randomForest/randomForest.pdf

# K-fold cross validation: Split the training set into <k> pieces. Then predict fold <1:k>, one at a time,
# with the rest of the folds. Last, average the accuracy over all the folds to determine which parameter values
# we want to use for the final model. Called "cp" (Complexity Paramater), works like UC and adj.R^2.
# Measures trade-off between model complexity and accuracy on training set.
# A smaller cp value leads to a bigger tree, so might overfit.
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=train,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
StevensTreeCV <- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                       data=train, method="class", cp=0.18)
PredictCV <- predict(StevensTreeCV, newdata=test, type="class")
result <- table(test$Reverse, PredictCV)
score <- (result[1] + result[4]) / sum(result) # Diagonal of correct outcomes (predicted 0, affirm)
score
prp(StevensTreeCV, cex=.7, col="blue")
summary(StevensTreeCV)


# 2) KEEPING AN EYE ON HEALTHCARE COSTS: THE D2HAWKEYE STOREY
# -----------------------------------------------------------
# Data mining company located in Waltham, Massachusets
# what data is avaiable? Medical claims, Eligibility info from employees, Demographic info
# D2Hawkeye's Claims Data: "Observation" Period 2001-2003, "Results" period 2003-2004

# Using "penalty error" for classification: Classifying a high-risk patient wrong is worse than classifying
# a low-risk patient wrong. Find most important factors to split on first, then "move down" importance tree.

# Split priority: Cost->Risk factors->Chronic Illness

# Using Claims data:
# Data from: http://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html
Claims <- read.csv(paste0(folder, "ClaimsData.csv"))
str(Claims)
# A 1% random sample of Medicare beneficiaries limited to those still alive at the end of 2008
# Independent vars from 2008. We will be predicting cost for 2009.
# Vars: Age at the end of 2008 + binary vars for various diseases.
# Reimbursment2008: Total value of all Medicare reimursment for this particular patient in 2008.
# Reimbursment2009: Total value of all Medicare reimursment for this particular patient in 2008.
# Bucket2008: The cost bucket the patient fell into in 2008. Thresholds determined by D2Hawkeye.
# Bucket2009: The cost bucket the patient fell into in 2009.
CorrelationPlot(Claims)
barplot(table(Claims$bucket2009) / nrow(Claims), main="Costbucket2009")
set.seed(88)
spl <- sample.split(Claims$bucket2009, SplitRatio=0.6)
ClaimsTrain <- subset(Claims, spl == T)
ClaimsTest <- subset(Claims, spl == F)
# Creating a smart baseline model: Predict whether the cost buckets for 2009 will be the same as for 2008:
result <- table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
result
accuracy <- sum(diag(result)) / nrow(ClaimsTest)
accuracy
# Create a "penalty matrix": The penalty matrix is constructed value-wise so that it "penalizes"
# wrong predictions on the high-risk groups (3,4,5) more than the low risk groups (1,2) because it is
# more important for the CART model to get correct predictions for the high risk groups, as the cost
# for treatment for these groups is exponentially higher than the low risk groups. The PenaltyMatrix
# given below can be put into the rpart model as a parameter (see below), and then the rpart can adjust
# its predictions/splits accordingly. Overall accuracy can go down, but the important risk groups will
# still be predicted better. 
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=T, nrow=5)
PenaltyMatrix
# Multiply the baseline model outcome with the penalty matrix:
as.matrix(result) * PenaltyMatrix
sum(as.matrix(result) * PenaltyMatrix) / nrow(ClaimsTest) # ~0.74: This is the Penalty Error
# Predicting healthcare costs:
library(rpart)
library(rpart.plot)
ClaimsTree1 <- rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes+heart.failure+
                      ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,
                    data=ClaimsTrain, method="class",cp=0.00005)
prp(ClaimsTree1, cex=.8, col="blue")
PredictTest <- predict(ClaimsTree1, newdata=ClaimsTest, type="class")
result <- table(ClaimsTest$bucket2009, PredictTest)
result
accuracy <- sum(diag(result)) / nrow(ClaimsTest)
accuracy
# Penalty error:
result <- as.matrix(result) * PenaltyMatrix
penalty.error <- sum(result) / nrow(ClaimsTest)
penalty.error # Penalty error also went up.
# rpart has a parameter called "loss". This is the penalty matrix we want to use.
ClaimsTree2 <- rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes+heart.failure+
                      ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,
                    data=ClaimsTrain, method="class",cp=0.00005,
                    parms=list(loss=PenaltyMatrix))
# Now it might choose different splits to minimize the penalty error.
PredictTest <- predict(ClaimsTree2, newdata=ClaimsTest, type="class")
result <- table(ClaimsTest$bucket2009, PredictTest)
result
accuracy <- sum(diag(result)) / nrow(ClaimsTest)
accuracy # Accuracy went down.
# Penalty error:
result <- as.matrix(result) * PenaltyMatrix
penalty.error <- sum(result) / nrow(ClaimsTest)
penalty.error # But penalty error also went down.


# 3) LOCATION, LOCATION, LOCATION! REGRESSION TREES FOR HOUSING DATA
# ------------------------------------------------------------------
# David Harrison of Harvard and Daniel Rubinfeldof U. of Michigan. 
# "Hedonic Housing Prices and the Demand for Clean Air" (late 70's)
boston <- read.csv(paste0(folder, "boston.csv"))
str(boston) # 506 sensus tracts
summary(boston)
pairs(boston, col="blue")
with(boston, plot(MEDV ~ CRIM, col="blue", pch=16, main="MEDV ~ Crime"))
model <- lm(MEDV ~ CRIM, data=boston)
model <- lm(MEDV ~ poly(CRIM,2), data=boston)
summary(model)
abline(model, col="red")
# Create a nice Boston map...
plot(boston$LAT ~ boston$LON, col=alpha(boston$CHAS+3, .5), main="Boston", pch=19) # Blue: Charles River
points(boston$LAT[boston$TRACT == 3531] ~ boston$LON[boston$TRACT == 3531], col="red", pch=19) # MIT Location
summary(boston$NOX)
# Add polution levels (NOX) > .55:
points(boston$LAT[boston$NOX >= .55] ~ boston$LON[boston$NOX >= .55], col=alpha("brown", .3), pch=19)
# Create a Boston plot with housing prices...
plot(boston$LAT ~ boston$LON, col=alpha(boston$CHAS+3, .5), main="Boston", pch=19) # Blue: Charles River
summary(boston$MEDV)
points(boston$LAT[boston$MEDV >= 21.2] ~ boston$LON[boston$MEDV >= 21.2], col=alpha("red", .3), pch=19)
latlonlm <- lm(MEDV ~ LAT+LON, data=boston)
summary(latlonlm)
# As we go to the East (LON) house prices decrease:
plot(boston$MEDV ~ boston$LON)
abline(latlonlm$coeff[1], latlonlm$coeff[3], col="red") # Hmmm....
plot(boston$LAT ~ boston$LON, col=alpha(boston$CHAS+3, .5), main="Boston", pch=19) # Blue: Charles River
points(boston$LAT[boston$MEDV >= 21.2] ~ boston$LON[boston$MEDV >= 21.2], col=alpha("red", .3), pch=19)
points(boston$LAT[latlonlm$fitted.values >= 21.2] ~ boston$LON[latlonlm$fitted.values >= 21.2], col="blue", pch="$")
# Here we see LAT matters instead! So this lm model is not very good. Let's see how regression trees do:
latlontree <- rpart(MEDV ~ LAT+LON, data=boston)
prp(latlontree, cex=.7, col="blue") # A regression tree splits on continuous values, not categories
plot(boston$LAT ~ boston$LON, col=alpha("blue", .3), pch=19)
# Above median prices:
points(boston$LAT[boston$MEDV >= 21.2] ~ boston$LON[boston$MEDV >= 21.2], col=alpha("red", .3), pch=19)
fittedvalues <- predict(latlontree) # Predict on training data
points(boston$LAT[fittedvalues >= 21.2] ~ boston$LON[fittedvalues >= 21.2], col="green4", pch="$")
latlontree <- rpart(MEDV ~ LAT+LON, data=boston, minbucket=50)
plot(latlontree, cex=.7, col="blue") # A regression tree splits on continuous values, not categories
# NOTE: Right side of the tree corresponds to the left side of the map, and vice vera
text(latlontree, col="blue", cex=.7)
plot(boston$LAT ~ boston$LON, col=alpha(boston$CHAS+3, .5), pch=19)
points(boston$LAT[boston$MEDV >= 21.2] ~ boston$LON[boston$MEDV >= 21.2], col=alpha("red", .3), pch=19)
# Show the "bucket":
abline(v=-71.07, col="red") # This is the first split in the tree ("LON <= -71.07), see plot above
abline(h=42.21, col="red") # This is the last split in the tree ("LAT < 42.21), see plot above
abline(h=42.17, col="red") # This is the next to last split in the tree, see plot above
# The middle right triangle is a low value area. Notice how the tree has managed to single that out, unlinear.
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio <- 0.7)
train <- subset(boston, split == T)
test <- subset(boston, split == F)
linreg <- lm(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
summary(linreg)
CorrelationPlot(boston)
linreg.pred <- predict(linreg, newdata=test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2)
linreg.sse
tree <- rpart(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train)
summary(tree)
prp(tree, cex=.8, col="blue")
tree.pred <- predict(tree, newdata=test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse # Trees is not as good on this problem as linear regression
# Note that we are predicted the Median/average house price in a particular bucket.
# The CP parameter: Complexity Parameter:
# We want to find the tree that minimizes:
# SUM(leaves) of <RSS at each leaf> + (lambda * Splits) lamda = penalty value, penalizing too many splits
lambda <- seq(0.001, 1, by=0.001)
tree.vals <- rnorm(100) # our y
RSS.of.tree.with.no.splits <- sum((tree.vals  - mean(tree.vals))^2) # Tree with no splits, just take the mean
cp <- lambda / RSS.of.tree.with.no.splits
cp
plot(cp, col="blue", type="l", main="cp")
# Using Cross validation:
tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp=(0:10)*0.001) # NOTE: This is approx the same range we got (0 to ~0.010) above
tr <- train(MEDV ~ LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data=train, method="rpart",
            trControl=tr.control, tuneGrid=cp.grid)
summary(tr)
options(digits=6) # NOTE: If model outputs, etc. rounds too much, set this!
tr # NOTE: It chooses the cp value with the smallest RMSE
best.tree <- tr$finalModel
prp(best.tree, cex=.7, col="blue")
best.tree.pred <- predict(best.tree, newdata=test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
best.tree.sse # Best tree SSE value! But not better than linear regression SSE
# Conclusion: For this particular regression problem, regular Linear Regression (lm) is better than Regression Trees.

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (Judge, Jury and clasifier):
# ----------------------------------------------
# 1) How much data do you think Andrew Martin should use to build his model?
# Answer: Information from all cases with the same set of justices as those he is trying to predict.
# Data from cases where the justices were different might just add noise to our problem

# 2) Answer: 1) Splits: 3 2) See below
# If X is greater than or equal to 60 and less than 85, and Y is less than 20.
# If X is less than 60, and Y is any value

# 3) Suppose you have a subset of 20 observations, where 14 have outcome A and 6 have outcome B.
# What proportion of observations have outcome A? Answer: 1) 0.7, 2) predict A,A.B (thresholds 0.25, 0.5 and 0.75)
A <- 14; B <- 6
A / (A + B)

# 4) 1) AUC is: 0.6927  2) Splits: 16 3) 1 split (see from summary(StevensTree2/3) below)
PredictROC <- predict(StevensTree, newdata=test)
pred <- prediction(PredictROC[,2], test$Reverse) # PredictROC[,2] = outcome 1 column
as.numeric(performance(pred, "auc")@y.values)
StevensTree2 <- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                      data=train, method="class", minbucket=5) # NOTE: minbucket param!
prp(StevensTree2, cex=.7, col="blue")
summary(StevensTree2)
# CP nsplit rel error xerror    xstd
# 1 0.21667      0    1.0000 1.0000 0.05505
# 2 0.05278      1    0.7833 0.8556 0.05389
# 3 0.02407      3    0.6778 0.9278 0.05460
# 4 0.01667      6    0.6056 0.9000 0.05436
# 5 0.01389      9    0.5500 0.9111 0.05446
# 6 0.01111     13    0.4833 0.9111 0.05446
# 7 0.01000     16    0.4500 0.9111 0.05446
StevensTree3 <- rpart(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                      data=train, method="class", minbucket=100) # NOTE: minbucket param!
prp(StevensTree3, cex=.7, col="blue")
summary(StevensTree3)
# CP nsplit rel error xerror    xstd
# 1 0.2167      0    1.0000 1.0000 0.05505
# 2 0.0100      1    0.7833 0.8222 0.05349

# 5) 1) Accuracy with set.seed(100): 0.6882  2) with set.seed(200): 0.7059
set.seed(100)
StevensForest2 <- randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                               data=train, nodesize=25, ntree=200) # nodesize=minbucket size for CART
PredictForest2<- predict(StevensForest2, newdata=test)
result <- table(test$Reverse, PredictForest2)
score <- (result[1] + result[4]) / sum(result) # Diagonal of correct outcomes (predicted 0, affirm)
score
set.seed(200)
StevensForest3 <- randomForest(Reverse ~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,
                               data=train, nodesize=25, ntree=200) # nodesize=minbucket size for CART
PredictForest3<- predict(StevensForest3, newdata=test)
result <- table(test$Reverse, PredictForest3)
score <- (result[1] + result[4]) / sum(result) # Diagonal of correct outcomes (predicted 0, affirm)
score

# 6) Splits of StevensTreeCV: 1
prp(StevensTreeCV, cex=.7, col="blue")
summary(StevensTreeCV)

# Quick questions 2 (D2Hawkeye):
# ------------------------------
# 1) In what ways do you think an analytics approach to predicting healthcare cost will improve upon the
# previous approach of human judgment? Select all that apply.
# Answer: All

# 2) A common problem in analytics is that you have some data available, but it's not the ideal dataset.
# This is the case for this problem, where we only have claims data. Which of the following pieces of
# information would we ideally like to have in our dataset, but are not included in claims data?
# Answer: We know from the claims info the drugs that were prescribed,
# but would like to know blood test results and physical exam results.

# 3) Additional variables: 
# Interactions between illnesses 
# Interactions between diagnosis and age 
# Noncompliance to treatment 
# Illness severity
# Variables to capture chronic conditions

# 4) What is the worst mistake we can make, according to the penalty error matrix?
# Answer: We predict 1 (very low cost), but the actual outcome is 5 (very high cost).
# What are the "best" types of mistakes we can make, according to the penalty error matrix?
# Answer: Mistakes where we predict one cost bucket HIGHER than the actual outcome.

# 5) What were the most important factors in the CART trees to predict cost?
# Answer: Cost ranges from the previous year

# 6) 1) What is the average age of patients in the training set, ClaimsTrain?
# 2) What proportion of people in the training set (ClaimsTrain) had at least one diagnosis code for diabetes?
# Answer: 1) 72.65 years  2) 0.3807
mean(ClaimsTrain$age)
table(ClaimsTrain$diabetes)[2] / nrow(ClaimsTrain)

# 7) baseline method of predicting the most frequent outcome for all observations.
# This new baseline method would predict cost bucket 1 for everyone.
# 1) What would the accuracy of this baseline method be on the test set? Answer: 0.6713
# 2) What would the penalty error of this baseline method be on the test set? 1.044
result <- table(ClaimsTest$bucket2009)
# NOTE: The baseline method simply picks the most frequent outcome for the dependent variable, which is 1.
result
accuracy <- result[1] / nrow(ClaimsTest)
accuracy
# Create a penalty matrix:
PenaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=T, nrow=5)
PenaltyMatrix
# Multiply the baseline model outcome with the penalty matrix (the COLUMN for the prediction value, here it is col 1):
sum(result * PenaltyMatrix[,1]) / nrow(ClaimsTest)

# 8) The first CART model, without the loss matrix, predicted bucket 1 for 78.6% of the observations in the test set.
# Did the second CART model, with the loss matrix, predict bucket 1 for more or fewer of the observations, and why?
# Answer: (2nd model predicted fewer observatons for bucket 1.) According to the penalty matrix, some of the worst types
# of errors are to predict bucket 1 when the actual cost bucket is higher. Therefore, the model with the penalty matrix
# predicted bucket 1 less frequently. 


# -----------------------------------------------------------------------------------------------------------------
# HOMEWORK UNIT 3:

# HOMEWORK 1: UNDERSTANDING WHY PEOPLE VOTE
# -----------------------------------------
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/3372864201764d6d9f63931920e5152e/ab08d73980f046479d3bcd105a55b0c2/

# 1) PROBLEM 1.1 - EXPLORATION AND LOGISTIC REGRESSION. Answer: 0.3159
# What proportion of people in this dataset voted in this election?
folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 4/Assignment/"
gerber <- read.csv(paste0(folder, "gerber.csv"))
str(gerber)
pairs(gerber, col="blue")
summary(gerber)
describe(gerber)
CorrelationPlot(gerber)
result <- table(gerber$voting)
proportion.voted <- result[2] / (result[1] + result[2])
proportion.voted

# 2) PROBLEM 1.2 - EXPLORATION AND LOGISTIC REGRESSION. Answer: Neighbors
# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
table(gerber$civicduty)
temp <- subset(gerber, civicduty == 1)
result <- table(temp$voting)
proportion.voted.civicduty <- result[2] / (result[1] + result[2])
proportion.voted.civicduty
temp <- subset(gerber, hawthorne == 1)
result <- table(temp$voting)
proportion.voted.hawthorne <- result[2] / (result[1] + result[2])
proportion.voted.hawthorne
temp <- subset(gerber, self == 1)
result <- table(temp$voting)
proportion.voted.self <- result[2] / (result[1] + result[2])
proportion.voted.self
temp <- subset(gerber, neighbors == 1)
result <- table(temp$voting)
proportion.voted.neighbors <- result[2] / (result[1] + result[2])
proportion.voted.neighbors

# 3) PROBLEM 1.3 - EXPLORATION AND LOGISTIC REGRESSION. Answer: Significant coeffs: All
model1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(model1)

# PROBLEM 1.4 - EXPLORATION AND LOGISTIC REGRESSION. 1) Accuracy at o.3 threshold = 0.542
# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
p1 <- predict(model1, type="response")
cm <- table(gerber$voting, p1 > 0.3) # create confusion matrix with threshold
# Accuracy: True predictions / all predictions
accuracy.0_3 <- (cm[1] + cm[4]) / sum(cm)
accuracy.0_3

# PROBLEM 1.5 - EXPLORATION AND LOGISTIC REGRESSION. Answer: 1) Accuracy at 0.5 threshold = 0.6841
cm <- table(gerber$voting, p1 > 0.5) # create confusion matrix with threshold
# Accuracy: True predictions / all predictions
accuracy.0_5 <- sum(diag(cm)) / sum(cm) # NOTE: Can use this technique also on a one-column!
accuracy.0_5

# PROBLEM 1.6 - EXPLORATION AND LOGISTIC REGRESSION. Answer: See below
# Answer: Even though all of the variables are significant, this is a weak predictive model.
result <- table(gerber$voting)
proportion.not.voted <- result[1] / (result[1] + result[2])
proportion.not.voted # Baseeline model same result (0.6841) as for acc > 0.5
p1 <- predict(model1, type="response")
pred <- prediction(p1, gerber$voting) # PredictROC[,2] = outcome 1 column
as.numeric(performance(pred, "auc")@y.values) # 0.5308, weaker than baseline model

# PROBLEM 2.1 - TREES. Answer: No variables are used (the tree is only a root node
# - none of the variables make a big enough effect to be split on.
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
summary(CARTmodel)

# PROBLEM 2.2 - TREES. Answer: Neighbor is the first split, civic duty is the last.
# What do you observe about the order of the splits?
CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2, cex=.7)
plot(CARTmodel2)
text(CARTmodel2, col="blue", cex=.7)
summary(CARTmodel2)

# PROBLEM 2.3 - TREES. Answer: 0.31 of civivduty = 1 voted (see right branch at bottom)

# PROBLEM 2.4 - TREES. Answer: WRONG HERE! Its men in both groups!
# 1) In the control group, which gender is more likely to vote?
# 2) In the "Civic Duty" group, which gender is more likely to vote?
CARTmodel3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3, cex=.7, digits=6)
plot(CARTmodel3)
text(CARTmodel3, col="blue", cex=.7)

# PROBLEM 3.1 - INTERACTION TERMS. Answer: abs(0.296638-0.34) = 0.04336
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, cex=.8, digits=6)
summary(CARTmodel4)
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, cex=.8, digits=6)

# PROBLEM 3.2 - INTERACTION TERMS. Answer: They are affected about the same
# (change in probability within 0.001 of each other).
# Determine who is affected more by NOT being in the control group (being in any of the four treatment groups)
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
summary(CARTmodel5)
prp(CARTmodel5, cex=.8, digits=6)
women <- abs(0.290456-0.334176)
men <- abs(0.302795-0.345818)
round(abs(women - men), 3)

# PROBLEM 3.3 - INTERACTION TERMS. Answer: Coefficient is negative, reflecting that women are less likely to vote
# Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex"
LogModelSex <- glm(voting ~ control + sex, data=gerber, family=binomial)
summary(LogModelSex)
LogModelSex <- glm(voting ~ control + as.factor(sex), data=gerber, family=binomial)
summary(LogModelSex) # NOTE: sex: 1 for female. Since coeff is negative, females are less likely to vote

# PROBLEM 3.4 - INTERACTION TERMS. Answer: 0.08055 (ERROR!) 0.02509 (ERROR!) TODO...
Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
# Two women, two men + not.control, control, not.control, control
# Or: ((Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control))
result1 <- predict(LogModelSex, newdata=Possibilities, type="response")
table(result1)
CARTmodelSex <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
summary(CARTmodelSex)
prp(CARTmodelSex, cex=.7, digits=5)
result2 <- predict(CARTmodelSex, newdata=Possibilities)
# Woman + Control group = last value in prediction: 
glm.woman.and.control <- result[4] # 0.2908065 with 7 digits
cart.woman.and.control <- result2[4]
abs(glm.woman.and.control - cart.woman.and.control)

# PROBLEM 3.5 - INTERACTION TERMS. Answer: See below
# Answer: If a person is a woman and in the control group, the chance that she voted goes down. 
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
# NOTE: Syntax to COMBINE two (or more?) predictors!
summary(LogModel2)

# PROBLEM 3.6 - INTERACTION TERMS. Answer: TODO...
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
p1 <- predict(LogModel2, newdata=Possibilities, type="response")
p2 <- predict(CARTmodel5, newdata=Possibilities)
sum(abs(p1[4] - p2[4]))

# PROBLEM 3.7 - INTERACTION TERMS. Answer: No
# Should we always include all possible interaction terms of the independent variables when building
# a logistic regression model?


# HOMEWORK 2: LETTER RECOGNITION
# ------------------------------
# Data from: http://archive.ics.uci.edu/ml/datasets/Letter+Recognition
letters <- read.csv(paste0(folder, "letters_ABPR.csv"))
str(letters)

# PROBLEM 1.1 - PREDICTING B OR NOT B. Answer: Baseline prediction on test set "notB" (most frequenct outcome): 0.7542
library(caTools)
letters$isB = as.factor(letters$letter == "B")
table(letters$isB)
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio=0.5)
train <- subset(letters, spl == T)
test <- subset(letters, spl == F)
# Create a baseline model: For Logistic Regression, we just measure the distribution of the outcome variable.
table(test$isB)
not.B <- as.integer(table(test$isB)[1])
all.rows <- nrow(test)
baseline <- not.B / all.rows
baseline

# PROBLEM 1.2 - PREDICTING B OR NOT B. Answer: Accuracy on test set with CART and treshold 0.5: 0.9358
CARTb = rpart(isB ~ . -letter, data=train, method="class")
summary(CARTb)
prp(CARTb, cex=.7, col="blue", main="CARTb tree")
result <- predict(CARTb, newdata=test, type="class")
table(result)
result <- table(as.integer(test$isB)-1, as.integer(result)-1 > 0.5) # Accuracy with treshold 0.5
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 1.3 - PREDICTING B OR NOT B. Answer: Accuracy on test set with RandomForest and treshold 0.5: 0.9878
set.seed(1000)
rfB = randomForest(isB ~ . -letter, data=train)
summary(rfB)
varImpPlot(rfB)
result <- predict(rfB, newdata=test, type="class")
table(result)
result <- table(as.integer(test$isB)-1, as.integer(result)-1 > 0.5) # Accuracy with treshold 0.5
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 2.1 - PREDICTING THE LETTERS A, B, P, R. Answer: Baseline accuracy on test set (predicting "P"): 0.2574
letters$letter = as.factor(letters$letter)
set.seed(2000)
spl <- sample.split(letters$letter, SplitRatio=0.5)
train <- subset(letters, spl == T)
test <- subset(letters, spl == F)
table(test$letter)
# Baseline model on test set: Predict the ratio of the most frequenct outcome (letter P):
table(test$letter)["P"] / nrow(test)

# PROBLEM 2.2 - PREDICTING THE LETTERS A, B, P, R. Accuracy on test set with rpart: 0.8787
CARTb = rpart(letter ~ . -isB, data=train, method="class")
prp(CARTb, cex=.8, col="blue")
p <- predict(CARTb, newdata=test, type="class")
result <- table(test$letter, p)
result
ConfusionMatrix(table(test$letter, p), c("A","B","P","R"))
accuracy <- sum(diag(result)) / sum(result) # sum(result) = nrow(test)
accuracy

# PROBLEM 2.3 - PREDICTING THE LETTERS A, B, P, R. Answer: Accuracy on test set with randomForest: 0.9801
set.seed(1000)
rfB = randomForest(letter ~ . -isB, data=train)
p <- predict(rfB, newdata=test, type="class")
result <- table(test$letter, p)
result
ConfusionMatrix(table(test$letter, p), c("A","B","P","R"))
accuracy <- sum(diag(result)) / sum(result) # sum(result) = nrow(test)
accuracy


# HOMEWORK 3: PREDICTING EARNINGS FROM CENSUS DATA
# ------------------------------------------------

# Data from: http://archive.ics.uci.edu/ml/datasets/Adult
census <- read.csv(paste0(folder, "census.csv"))
str(census)
CorrelationPlot(census)
hist(census$hoursperweek, col="wheat", main="Hours per week")
boxplot(census$hoursperweek ~ census$sex, data=census, col="wheat",
        main="Weekly working hours by sex")
par(mar=c(7,3,2,1))
boxplot(census$hoursperweek ~ census$occupation, data=census, las=2, col="wheat",
        main="Weekly working hours by occupation")
boxplot(census$hoursperweek ~ census$maritalstatus, data=census, las=2, col="wheat",
        main="Weekly working hours by marital status")
boxplot(census$hoursperweek ~ census$race, data=census, las=2, col="wheat",
        main="Weekly working hours by race")
par(mar=c(3,3,2,1))

# PROBLEM 1.1 - A LOGISTIC REGRESSION MODEL. Answer: See below.
# Answer: age workclass education maritalstatus occupation relationship sex capitalgain capitalloss hoursperweek
# Which variables are significant, or have factors that are significant?
# (Use 0.1 as your significance threshold, so variables with a period or dot in the stars column
# should be counted too
set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio=0.6)
train <- subset(census, spl == T)
test <- subset(census, spl == F)
model1 <- glm(over50k ~., data=train, family=binomial)
summary(model1)

# PROBLEM 1.2 - A LOGISTIC REGRESSION MODEL. Answer: Accuracy on test set with threshold 0.5: 0.8552
p <- predict(model1, newdata=test, type="response")
result <- table(test$over50k, p > 0.5)
result
ConfusionMatrix(result, c("No", "Yes"))
accuracy <- sum(diag(result)) / sum(result) # sum(result) = nrow(test)
accuracy

# PROBLEM 1.3 - A LOGISTIC REGRESSION MODEL. Answer: 0.7594
table(test$over50k)[1] / nrow(test) # NOTE: Always predict the most frequenct outcome!

# PROBLEM 1.4 - A LOGISTIC REGRESSION MODEL. Answer: ROC AUC: 0.9062
PredictROC <- predict(model1, newdata=test)
pred <- prediction(PredictROC, test$over50k)
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 2.1 - A CART MODEL. Answer: There are 4 splits in the tree (see also summary)
cart1 <- rpart(over50k ~ ., data=train, method="class")
summary(cart1)
prp(cart1, cex=.7, col="blue")

# PROBLEM 2.2 - A CART MODEL- Answer: relationship
# Which variable does the tree split on at the first level (the very first split of the tree)?

# PROBLEM 2.3 - A CART MODEL- Answer: capitalg, education
# Which variables does the tree split on at the second level (immediately after the first split of the tree)?

# PROBLEM 2.4 - A CART MODEL- Answer: Accuracy = 0.8474
# One way, with probablilities for over (1) or under (0) 50k:
p <- predict(cart1, newdata=test, type="prob")
result <- table(test$over50k, p[,2] > 0.5) # p[,2] = >50K
result
# Another way, with class directly:
p <- predict(cart1, newdata=test, type="class")
result <- table(test$over50k, p)
result
ConfusionMatrix(result, c("<=50K", ">50K"))
accuracy <- sum(diag(result)) / sum(result) # sum(result) = nrow(test)
accuracy

# PROBLEM 2.5 - A CART MODEL- Answer: See below.
# Answer: The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf
# of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values. (ROC AUC = 0.847)
PredictROC <- predict(cart1, newdata=test, type="prob")
result <- table(test$over50k, PredictRoc[,2] > 0.5) # p[,2] = >50K
result
pred <- prediction(PredictROC[,2], test$over50k)
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue", type="o") # NOTE: More jagged than glm pred. Marks the threshold at the bucket/leaf of the CART tree.
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 2.6 - A CART MODEL- Answer: AUC = 0.847 (see calulation in 2.5)

# PROBLEM 3.1 - A RANDOM FOREST MODEL. Answer: rf accuracy = 0.8345 (a little worse than CART because of downsampling train set)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
rf1 <- randomForest(over50k ~., data=trainSmall)
varImpPlot(rf1, col="blue", pch=19)
p <- predict(rf1, newdata=test) # prob default
result <- table(test$over50k, p) # threshold 0.5
result
ConfusionMatrix(result, c("<=50K", ">50K"))
accuracy <- sum(diag(result)) / sum(result) # sum(result) = nrow(test)
accuracy

# PROBLEM 3.2 - A RANDOM FOREST MODEL. Answer: Most inportant term: Age
vu <- varUsed(rf1, count=TRUE)
vusorted <- sort(vu, decreasing=FALSE, index.return=TRUE)
# NOTE: This measures the times a variable was selected for splitting in a rf tree
dotchart(vusorted$x, names(rf1$forest$xlevels[vusorted$ix]), col="blue", pch=19)

# PROBLEM 3.3 - A RANDOM FOREST MODEL. Answer: Most inportant var: Occupation
# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is.
# In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way
# to measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is
# selected for splitting in all of the trees in the forest.
varImpPlot(rf1, col="blue", pch=19)

# PROBLEM 4.1 - SELECTING CP BY CROSS-VALIDATION. Answer: Best cp = 0.002
# NOTE: Use whole train set:
set.seed(2)
numFolds <- trainControl(method="cv", number=10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
result <- train(over50k ~ ., data=train,
      method="rpart", trControl=numFolds, tuneGrid=cartGrid) # Get cp param at end
summary(result) # Pick cv with lowest RMSE
best.cp <- 0.002 # Summary: cp = 0.002

# PROBLEM 4.2 - SELECTING CP BY CROSS-VALIDATION. Answer: Score for new CV model: 0.8612
census.cv <- rpart(over50k ~ ., data=train, method="class", cp=best.cp)
PredictCV <- predict(census.cv, newdata=test, type="class")
result <- table(test$over50k, PredictCV)
score <- sum(diag(result)) / sum(result) # Diagonal of correct outcomes / total rows
score # Improved: 0.8612, but less interpretable model now (more splits):
prp(census.cv, cex=.7, col="blue")
summary(census.cv)

# PROBLEM 4.3 - SELECTING CP BY CROSS-VALIDATION. Answer: Splits in new CV model: 18
prp(census.cv, cex=.7, col="blue")
summary(census.cv)


# 4) STATE DATA REVISITED (OPTIONAL)
# ----------------------------------
data(state)
statedata <- data.frame(state.x77) # NOTE: Same as stateDataSimple.csv in week 4 folder

# This dataset has 50 observations (one for each US state) and the following 8 variables:
# Population - the population estimate of the state in 1975
# Income - per capita income in 1974
# Illiteracy - illiteracy rates in 1970, as a percent of the population
# Life.Exp - the life expectancy in years of residents of the state in 1970
# Murder - the murder and non-negligent manslaughter rate per 100,000 population in 1976 
# HS.Grad - percent of high-school graduates in 1970
# Frost - the mean number of days with minimum temperature below freezing from 1931-1960 in the capital or a large city of the state
# Area - the land area (in square miles) of the state
str(statedata)
pairs(statedata, col="blue", main="statedata")
# We will try to build a model for life expectancy using regression trees,
# and employ cross-validation to improve our tree's performance.
CorrelationPlot(statedata)

# PROBLEM 1.1 - LINEAR REGRESSION MODELS. Answer: adjR2 = 0.692
fit <- lm(Life.Exp ~ ., data=statedata)
summary(fit)

# PROBLEM 1.2 - LINEAR REGRESSION MODELS. Answer: SSE = 23.2971
pred <- predict(fit)
SSE <- sum((pred - statedata$Life.Exp)^2) 
SSE # 23.2971
# or use:
sum(fit$residuals^2)

# PROBLEM 1.3 - LINEAR REGRESSION MODELS. Answer: AdjR2 = 0.7126
fit2 <- lm(Life.Exp ~ Population+Murder+Frost+HS.Grad, data=statedata)
summary(fit2)

# PROBLEM 1.4 - LINEAR REGRESSION MODELS. Answer: SSE = 23.30804
pred2 <- predict(fit2)
SSE2 <- sum((pred2 - statedata$Life.Exp)^2) 
SSE2 # 23.30804

# PROBLEM 1.5 - LINEAR REGRESSION MODELS. Answer: See below.
# Answer: Trying different combinations of variables in linear regression is like trying different numbers of
# splits in a tree - this controls the complexity of the model.

# PROBLEM 2.1 - CART MODELS Answer: Only variable in tree is "Murder"
fit3 <- rpart(Life.Exp ~ ., data=statedata)
summary(fit3)
prp(fit3, cex=.7, col="blue")

# PROBLEM 2.2 - CART MODELS Answer: SSE = 28.99848
pred3 <- predict(fit3)
SSE3 <- sum((pred3 - statedata$Life.Exp)^2) 
SSE3 # 28.99848

# PROBLEM 2.3 - CART MODELS Answer: Murder, HS.Grad and Area are shown in the CART tree
fit4 <- rpart(Life.Exp ~ ., data=statedata, minbucket=5)
summary(fit4)
prp(fit4, cex=.7, col="blue")

# PROBLEM 2.4 - CART MODELS Answer: The default minbucket size must be larger, since 5 is limiting the number of trees
# being built. A larger minbucket split into more trees. See rpart.control (minsplit = 20, minbucket = round(minsplit/3))
# Do you think the default minbucket parameter is smaller or larger than 5 based on the tree that was built?

# PROBLEM 2.5 - CART MODELS Answer: SSE = 23.64283
pred4 <- predict(fit4)
SSE4 <- sum((pred4 - statedata$Life.Exp)^2) 
SSE4 # 23.64283

# PROBLEM 2.6 - CART MODELS Answer: SSE = 9.312442
fit5 <- rpart(Life.Exp ~ Area, data=statedata, minbucket=1) # NOTE: One observation in each bucket? No, see note below.
summary(fit5)
prp(fit5, cex=.7, col="blue")
pred5 <- predict(fit5)
SSE5 <- sum((pred5 - statedata$Life.Exp)^2) 
SSE5 # 9.312442
# Note that the SSE is not zero here - we still make some mistakes. This is because there are other parameters
# in rpart that are also trying to prevent the tree from overfitting by setting default values. So our tree
# doesn't necessarily have one observation in each bucket - by setting minbucket=1 we are just ALLOWING the tree
# to have one observation in each bucket.

# PROBLEM 2.7 - CART MODELS Answer: See below.
# This is the lowest error we have seen so far. What would be the best interpretation of this result?
# Answer: We can build almost perfect models given the right parameters, even if they violate our intuition of
# what a good model should be.

# PROBLEM 3.1 - CROSS-VALIDATION. Answer: cp = 0.12
# Use caret to get optimal cp value:
set.seed(111)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Life.Exp ~ ., data=statedata,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
# Remember that the train function tells you to pick the largest value of cp with the lowest error
# when there are ties, and explains this at the bottom of the output.

# PROBLEM 3.2 - CROSS-VALIDATION. Answer: See below.
# Answer: Qe predict the life expectancy to be 70 if the murder rate is greater than or equal to 6.6 and is less than 11.
fit6 <- rpart(Life.Exp ~ ., data=statedata, cp=0.12) # NOTE: One observation in each bucket? No, see note below.
summary(fit6)
prp(fit6, cex=.7, col="blue")
pred6 <- predict(fit6)

# PROBLEM 3.3 - CROSS-VALIDATION. Answer: SSE = 32.86549
SSE6 <- sum((pred6 - statedata$Life.Exp)^2) 
SSE6 # 32.86549

# PROBLEM 3.4 - CROSS-VALIDATION. Answer: The model we just made with the "best" cp
# Given what you have learned about cross-validation, which of the three models would you expect to be better if we did
# use it for prediction on a test set?

# PROBLEM 3.5 - CROSS-VALIDATION. Answer: Tree has 4 splits.
set.seed(111)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Life.Exp ~ Area, data=statedata,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
fit7 <- rpart(Life.Exp ~ Area, data=statedata, cp=0.02) # NOTE: One observation in each bucket? No, see note below.
prp(fit7, cex=.7, col="blue")

# PROBLEM 3.6 - CROSS-VALIDATION. Answer: Observations in this leaf correspond to states with area greater than
# or equal to 9579 and area less than 51000 (51e+3)

# PROBLEM 3.7 - CROSS-VALIDATION. Answer: The Area variable is not as predictive as Murder rate.
pred7 <- predict(fit7)
SSE7 <- sum((pred7 - statedata$Life.Exp)^2) 
SSE7 # 44.26817
