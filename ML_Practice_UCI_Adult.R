# UCI ML Adult data set
# http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# UCI ML Leaf data set
# https://archive.ics.uci.edu/ml/datasets/Leaf

set.seed(19621607)
source("tools.R")

# col.classes <- c("integer","factor",rep("numeric", 30))

# NOTE: " ," separator has been converted to "," in CSV file after download
train <- read.csv("c:/coding/R/testdata/adult.csv", header=F, sep=",")

head(train)
dim(train)
summary(train)
sapply(train, class)

# Severe outliers in V11...
plot(sort(train$V11), col="blue", main="V11")
barplot(table(train$V5), cex.axis=.8, cex.names=.8, main="V5")
barplot(table(train$V8), cex.axis=.8, cex.names=.7, main="V8")

y <- train$V15 # Save the outcome variable

na.cols <- c("V2","V7")
table(train$V2)
table(train$V7)
train$V2 <- as.character(train$V2)
train$V7 <- as.character(train$V7)
train$V2[train$V2 == "?"] <- "Unknown"
train$V7[train$V7 == "?"] <- "Unknown"
train$V2 <- as.factor(train$V2)
train$V7 <- as.factor(train$V7)

train$V15 <- as.character(train$V15)
train$V15[train$V15 == "<=50K"] <- 0
train$V15[train$V15 == ">50K"] <- 1
train$V15 <- as.factor(train$V15)

numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))
names(train[, numeric.cols])[which((var(train[, numeric.cols]) > 0.2) == T)]
cols <- c("V5","V8","V11","V13")

CorrelationPlot(train[, numeric.cols])
CorrelationPlot(train)

par(mar=c(5,5,3,1))
plot(sort(var(train[, numeric.cols])))
VariancePlot(train[, numeric.cols])

# TODO: Check missing values or other anomalies!

result <- CreateTrainAndValidationSets(train)
train.subset <- result$train
validation.subset <- result$validation

# ------------------------------------------------------------------------------------------------------------------
# Model selection

# Remove outcome variable + country
# Do GLM
fit.glm <- glm(train.subset$V15 ~ ., data=train.subset[, c(-14,-15)], family=binomial(link="logit"))
fit.glm <- glm(train.subset$V15 ~ ., data=train.subset[, cols], family=binomial(link="logit"))
summary(fit.glm)
pred.glm <- predict(fit.glm, newdata=validation.subset, type="response")
pred.glm <- ifelse(pred.glm < 0.5, 0, 1)
table(validation.subset$V15, pred.glm)
ConfusionMatrix(table(validation.subset$V15, pred.glm), labels=c("<=50K",">50K"))
rmse <- MyRMSE(as.integer(pred.glm), as.integer(validation.subset$V15)-1)
rmse

# Do GBM with Bernoulli distribution (binary outcome)
library(gbm)

fit.gbm <- gbm(train.subset$V15 ~ .
           ,data=train.subset[, cols]
           ,var.monotone=NULL
           ,distribution="bernoulli"
           ,n.trees=100
           ,shrinkage=0.1
           ,interaction.depth=3
           ,bag.fraction=0.5
           ,train.fraction=1
           ,n.minobsinnode=10
           ,cv.folds=10
           ,keep.data=TRUE
           ,verbose=TRUE)

summary(fit.gbm, main="GBM variable importance", cex.axis=.8, cex.lab=.8, cex.main=1, cex.names=.7, las=1)
best.iter <- gbm.perf(fit.gbm, method="cv") # Find the best iteration number
pred.gbm <- predict(fit.gbm, validation, best.iter, type="response")
pred.gbm[1:50]
pred.gbm[pred.gbm < 0] <- 0 # NOTE: Must remove those below zero!


# ------------------------------------------------------------------------------------------------------------------
# TODO: Test cv.glm in package boot for 10-fold CV and LOOCV.
# http://stackoverflow.com/questions/16781551/cost-function-in-cv-glm-of-boot-library-in-r
library(boot)

train.diag <- glm.diag(fit)
glm.diag.plots(fit, train.diag)

cost <- function(y, pred) mean(abs(y-pred) > 0.5)
cv.glm(data=train.subset, glmfit=fit, cost(as.integer(train.subset$V2)-1, fit$fitted), K=10)

# Call with a slightly larger train set:
rmse <- DoCV(train$V15, train[, c(-14,-15)], 10)
