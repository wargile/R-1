# UCI ML Adult data set
# http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# UCI ML Leaf data set
# https://archive.ics.uci.edu/ml/datasets/Leaf

# Columns:
# age: continuous.
# workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
# fnlwgt: continuous.
# education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
# education-num: continuous.
# marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
# occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
# relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
# race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
# sex: Female, Male.
# capital-gain: continuous.
# capital-loss: continuous.
# hours-per-week: continuous.
# native-country: categorical. Country names
  
set.seed(1000)
source("tools.R")
SetStandardOptions()

# col.classes <- c("integer","factor",rep("numeric", 30))

# NOTE: " ," separator has been converted to "," in CSV file after download
colClasses <- c("numeric","factor","numeric","factor","numeric",rep("factor",5),rep("numeric",3),"factor","factor")
train <- read.csv("c:/coding/R/testdata/adult.csv", header=F, sep=",", colClasses=colClasses)
load("c:/coding/R/testdata/adultCleaned.rda")
str(train)
names(train) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship",
                  "race","sex","capital.gain","capital.loss","hours.per.week","native.country","wage")
head(train)
dim(train)
summary(train)
sapply(train, class)

# Severe outliers in captital.gain...
plot(sort(train$capital.gain), col="blue", main="Capital gain")
barplot(table(train$education.num), cex.axis=.8, cex.names=.8, main="education.num")
barplot(table(train$relationship), cex.axis=.8, cex.names=.7, main="relationship")

y <- train$wage # Save the outcome variable

table(is.na(train$workclass))
sapply(train, function(x) table(is.na(x)))
table(train$workclass)
table(train$V7)
train$workclass <- as.character(train$workclass)
train$workclass[train$workclass == "?"] <- "Unknown"
train$workclass <- as.factor(train$workclass)
table(train$occupation)
train$occupation <- as.character(train$occupation)
train$occupation[train$occupation == "?"] <- "Unknown"
train$occupation <- as.factor(train$occupation)
# TODO: Do the same in native.country!

train$wage <- as.character(train$wage)
train$wage[train$wage == "<=50K"] <- 0
train$wage[train$wage == ">50K"] <- 1
train$wage <- as.factor(train$wage)

#numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))
#names(train[, numeric.cols])[which((var(train[, numeric.cols]) > 0.2) == T)]
#cols <- c("V5","V8","V11","V13")

save(train, file="c:/coding/R/testdata/adultCleaned.rda")

names(train)
CorrelationPlot(train)

par(mar=c(5,5,3,1))
plot(sort(var(train))) # TODO: Only numeric quantitative cols
VariancePlot(train)

# TODO: Check missing values or other anomalies!

# Split in train and validation sets
library(caTools)
set.seed(1000)
split <- sample.split(train$wage, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)

# Get baseline accuracy
table(train.subset$wage)[1] / nrow(train.subset) # Baseline accuracy: 0.7592

# ------------------------------------------------------------------------------------------------------------------
# Model selection

# Remove outcome variable + country
# Do GLM
names(train.subset)
fit.glm <- glm(as.factor(train.subset$wage) ~ ., data=train.subset[,-14], family=binomial(link="logit"))
fit.glm <- glm(train.subset$wage ~ age + hours.per.week + race + sex + relationship + workclass +
                 education.num, data=train.subset, family=binomial(link="logit"))
summary(fit.glm)
pred.glm <- predict(fit.glm, newdata=validation.subset, type="response")
pred.glm <- ifelse(pred.glm < 0.5, 0, 1)
result <- table(validation.subset$wage, pred.glm)
result
ConfusionMatrix(result, labels=c("<=50K",">50K"))
sum(diag(result)) / sum(result)
rmse <- MyRMSE(as.integer(pred.glm), as.integer(validation.subset$wage)-1)
rmse

# ------------------------------------------------------------------------------------------------------------------

# Train a random forest
library(caret)
numFolds <- trainControl(method="cv", number=10)
train.caret <- train(as.factor(wage) ~ ., data=train.subset[,-14], method="rf", trControl=numFolds, metric="Accuracy")
predict.caret <- predict(train.caret, newdata=validation.subset, type="class")
result <- table(validation.subset$wage, predict.caret)
result
# Compute accuracy
sum(diag(result)) / sum(result)
rmse <- MyRMSE(as.integer(predict.caret)-1, as.integer(validation.subset$wage)-1)
rmse

# ------------------------------------------------------------------------------------------------------------------

# Do GBM with Bernoulli distribution (binary outcome)
library(gbm)

fit.gbm <- gbm(train.subset$wage ~ .
           ,data=train.subset
           ,var.monotone=NULL
           ,distribution="bernoulli"
           ,n.trees=100
           ,shrinkage=0.1
           ,interaction.depth=3
           ,bag.fraction=0.5
           ,train.fraction=1
           ,n.minobsinnode=10
           ,cv.folds=10
           ,keep.data=FALSE # TODO: Needed for summary graph??
           ,verbose=TRUE)

summary(fit.gbm, main="GBM variable importance", cex.axis=.8, cex.lab=.8, cex.main=1, cex.names=.7, las=1)
best.iter <- gbm.perf(fit.gbm, method="cv") # Find the best iteration number
pred.gbm <- predict(fit.gbm, validation, best.iter, type="response")
pred.gbm[1:50]
pred.gbm[pred.gbm < 0] <- 0 # NOTE: Must remove those below zero!

# ------------------------------------------------------------------------------------------------------------------

# Do a CART tree
library(rpart)
library(rpart.plot)

# Train to find optimal cp parameter
library(caret)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(as.factor(wage) ~ ., data=train.subset[,-14],
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.01
adultCART <- rpart(as.factor(wage) ~ ., data=train.subset[,-14], method="class", cp=cp.value)
adultCART <- rpart(as.factor(wage) ~ age + hours.per.week + race + sex + relationship + workclass +
                     education.num, data=train.subset, method="class", cp=cp.value)

summary(adultCART)
prp(adultCART, cex=.8, col="blue", main="Wage")
# Evaluate the performance of the model
predictCART <- predict(adultCART, newdata=validation.subset, type="class")
result <- table(validation.subset$wage, predictCART)
result
# Compute accuracy
sum(diag(result)) / sum(result)
rmse <- MyRMSE(as.integer(predictCART)-1, as.integer(validation.subset$wage)-1)
rmse

# ------------------------------------------------------------------------------------------------------------------

# TODO: Test cv.glm in package boot for 10-fold CV and LOOCV.
# http://stackoverflow.com/questions/16781551/cost-function-in-cv-glm-of-boot-library-in-r
library(boot)

train.diag <- glm.diag(fit)
glm.diag.plots(fit, train.diag)

cost <- function(y, pred) mean(abs(y-pred) > 0.5)
cv.glm(data=train.subset, glmfit=fit, cost(as.integer(train.subset$wage)-1, fit$fitted), K=10)

rmse <- DoCV(train.subset$wage, train.subset, 10)
