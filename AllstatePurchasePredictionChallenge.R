# Allstate Purchase Prediction Challenge
# --------------------------------------
# Predict a purchased policy based on transaction history

# As a customer shops an insurance policy, he/she will receive a number of quotes with different coverage
# options before purchasing a plan. This is represented in this challenge as a series of rows that include
# a customer ID, information about the customer, information about the quoted policy, and the cost.
# Your task is to predict the purchased coverage options using a limited subset of the total interaction
# history. If the eventual purchase can be predicted sooner in the shopping window, the quoting process is
# shortened and the issuer is less likely to lose the customer's business.
# Using a customer's shopping history, can you predict what policy they will end up choosing?

# http://www.kaggle.com/c/allstate-purchase-prediction-challenge/data

# Solutions:
# http://www.kaggle.com/c/allstate-purchase-prediction-challenge/forums/t/8218/solution-sharing/44911#post44911
# https://github.com/mrcanard/AllStateKaggle/tree/winningentry


# TIPS:
# Could decision tree work here? Predict different outcomes for A-G? Compare to
# Decision Tree for the Samsung Activity Classification (Jeff Leek) 

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/AllstatePurchasePredictionChallenge/data/"
codeFolder <- "C:/coding/Kaggle/AllstatePurchasePredictionChallenge/code/"
submissionsFolder <- "C:/coding/Kaggle/AllstatePurchasePredictionChallenge/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",")
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",")
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

names(train)
head(train)
head(train[,c(1,3,4,5,18,19,20:25)], n=9)
train[1:9,c(1:5,18:25)]
head(test)
dim(train)
dim(test)
sapply(train, class)

# Do some plotting:
plot(risk_factor ~ state, data=train, col="powderblue", cex.axis=.7, las=2,
     main="Risk factor by State")
plot(car_age ~ state, data=train, col="cornflowerblue", cex.axis=.7, las=2,
     main="Car age by State")

# Look at missing values:
package.install("Amelia")
library(Amelia)
par(mfrow=c(1,2))
missmap(train[1:500, ], main = "Missingness Map Train", col = c("wheat", "cornflowerblue"))
missmap(test[1:500, ], main = "Missingness Map Test", col = c("wheat", "powderblue"))
par(mfrow=c(1,1))
# NA's in: risk_factor, location_previous, C_previous -- impute these?

sum(!complete.cases(train))
sum(!complete.cases(test))

library(e1071)
impute(train, what="median")
impute(test, what="median")

sum(!complete.cases(train))
sum(!complete.cases(test))

planCol <- 18 # Save for later use/adjustment


# ------------------------------------------------------------------------------------------------------------------------
# TEST: Could we find all the records where a purchase was made (record_type = 1), and get the record before that,
# find the corresponding policy combination in test set, and then use the purchase record in test set?
line.purchase <- which(train$record_type == 1)
line.before.purchase <- (which(train$record_type == 1) - 1)
line.purchase[1:20]
line.before.purchase[1:20]
# ------------------------------------------------------------------------------------------------------------------------

# Create a new col with merged purchase options? How to use purchase options as outcome?
train$planMerged <- as.integer(paste0(train[, planCol]+1, train[, planCol + 1]+1, train[, planCol + 2]+1,
                                        train[, planCol + 3]+1, train[, planCol + 4]+1,
                                        train[, planCol + 5]+1, train[, planCol + 6]+1))
test$planMerged <- as.integer(paste0(test[, planCol]+1, test[, planCol + 1]+1, test[, planCol + 2]+1,
                                       test[, planCol + 3]+1, test[, planCol + 4]+1,
                                       test[, planCol + 5]+1, test[, planCol + 6]+1))

# IMPORTANT:
# Product Options: Each product has 7 customizable options selected by customers, each with 2, 3, or 4 ordinal values possible:
# A=0,1,2
# B=0,1
# C=1,2,3,4
# D=1,2,3
# E=0,1
# F=0,1,2,3
# G=1,2,3,4

# Another way/tip:
# do.call(paste0, as.list(sapply(1:6, FUN=function(x) { stuff <- paste0(stuff, x) })))

# Do a quick test. TODO: How to predict a categorical/nominal outcome variable with this many combinations?
# http://stats.stackexchange.com/questions/82660/performance-metric-for-categorical-outcome-prediction
# http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
# options(contrasts = c("contr.treatment", "contr.poly"))
package.install("nnet")
library(nnet) # For multinom, etc.

trainSub <- train[1:1000, ]

# http://www.ats.ucla.edu/stat/r/dae/ologit.htm

fit1 <- glm(record_type ~ shopping_pt+A+B+C+D+E+F+G, data=train[1:5000,], family=binomial(link="logit"))
fit2 <- glm(record_type ~ shopping_pt+planMerged, data=train[1:5000,], family=binomial(link="logit"))
pred1 <- predict(fit1, newdata=test, type="response")
pred2 <- predict(fit2, newdata=test, type="response")

fit <- glm(planMerged ~ shopping_pt + record_type + group_size + homeowner + car_age + as.integer(car_value) +
             risk_factor, data=train[1:5000,])
summary(fit)
pred <- predict(fit, newdata=test, type="response")

fit <- multinom(as.factor(A) ~ group_size + homeowner + car_age + as.integer(car_value) +
             risk_factor, data=trainSub)
summary(fit)
pred <- predict(fit, newdata=test, type="class")
pred <- predict(fit, newdata=test, type="probs")

fitA <- glm(A ~ group_size + homeowner + car_age + as.integer(car_value) + risk_factor,
                  data=trainSub, family = gaussian(link = "identity"))
summary(fitA)
pred <- predict(fitA, newdata=test, type="response")

fitA <- lm(A ~ homeowner + car_age, data=trainSub)
summary(fitA)
# NOTE: Test need to be fixed too!
pred <- predict(fitA, newdata=test, type="response")

# Get unique customerID for test set pred and submission:
test1 <- test[ !duplicated(test$customer_ID, fromLast=TRUE ), ]

library(MASS)
fitA <- polr(as.factor(A) ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train)
predA <- predict(fitA, newdata=test1, type="class")
unique(predA)
fitB <- glm(B ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train, family=binomial(link="logit"))
predB <- predict(fitB, newdata=test1, type="response")
predB <- ifelse(predB > 0.5, 1, 0) 
unique(predB)
fitC <- polr(as.factor(C) ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train)
predC <- predict(fitC, newdata=test1, type="class")
unique(predC)
fitD <- polr(as.factor(D) ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train)
predD <- predict(fitD, newdata=test1, type="class")
unique(predD)
fitE <- glm(E ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train, family=binomial(link="logit"))
predE <- predict(fitE, newdata=test1, type="response")
predE <- ifelse(predE > 0.5, 1, 0) 
unique(predE)
fitF <- polr(as.factor(F) ~ group_size + homeowner + car_age + shopping_pt + married_couple, data=train)
predF <- predict(fitF, newdata=test1, type="class")
unique(predF)
fitG <- polr(as.factor(G) ~ group_size + homeowner + car_age, data=train)
predG <- predict(fitG, newdata=test1, type="class")
unique(predG)
# method = c("logistic", "probit", "cloglog", "cauchit")

# Use clogit?

# ------------------------------------------------------------------------------------------------------
# Submission

# Get the most recent quoted option for each customer:
# http://stackoverflow.com/questions/15042646/r-dataframe-how-to-extract-unique-values
# ddply can also be used?
sub <- test[ !duplicated(test$customer_ID, fromLast=TRUE ), ]
sub$plan <- paste0(sub[, planCol], sub[, planCol + 1], sub[, planCol + 2], sub[, planCol + 3],
                   sub[, planCol + 4], sub[, planCol + 5], sub[, planCol + 6])
head(sub)
write.csv(x=sub[, c(1, ncol(sub))], file=paste0(submissionsFolder, "submission_lastQuoted.csv"),
          quote=FALSE, row.names=FALSE)

# ------------------------------------------------------------------------------------------------------
# Submission 2 (polr and lm)
sub <- test[ !duplicated(test$customer_ID, fromLast=TRUE ), ]
plan <- paste0(predA, predB, predC, predD, predE, predF, predG)
x <- data.frame(customer_ID=sub$customer_ID, plan=plan)
head(x)
write.csv(x, paste0(submissionsFolder, "submission_lastQuoted.csv"),
          quote=FALSE, row.names=FALSE)
