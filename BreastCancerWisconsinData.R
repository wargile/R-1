# Machine Learning, Breast Cancer Wisconsin Data
# http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/

set.seed(19621607)
source("tools.R")

col.classes <- c("integer","factor",rep("numeric", 30))

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
train <- read.csv(url, header=F, sep=",", colClasses=col.classes)

head(train)
dim(train)
summary(train)
sapply(train, class)

train$V2[train$V2 == "M"] <- 1
train$V2[train$V2 == "B"] <- 0
names(train[,3:32])[which((var(train[,3:32]) > 0.2) == T)]
#cols <- c("V3","V4","V5","V6","V13","V15","V16","V23","V24","V25","V26","V28","V29")
cols <- c("V25","V27","V31")

CorrelationPlot(train)
par(mar=c(5,5,3,1))
plot(sort(var(train[, c(-1,-2)])))
VariancePlot(train[, c(-1,-2)])

# V25 has huge variance compared to V27, V30. Normalize? No improvement in prediction.
#var(train$V25)
#train$V25 <- Normalize(min(train$V25), max(train$V25), 0, 1, train$V25)
#var(train$V25)

# TODO: Check missing values or other anomalies!

result <- CreateTrainAndValidationSets(train)
train.subset <- result$train
validation.subset <- result$validation

fit <- glm(train.subset$V2 ~ ., data=train.subset[, cols], family=binomial(link="logit"))
summary(fit)
pred <- predict(fit, newdata=validation.subset, type="response")
pred <- ifelse(pred < 0.5, 0, 1)
table(validation.subset$V2, pred)
ConfusionMatrix(table(validation.subset$V2, pred), labels=c("B","M"))
rmse <- MyRMSE(as.integer(pred), as.integer(validation.subset$V2)-1)
rmse

# TODO: Test cv.glm in package boot for 10-fold CV and LOOCV.
# http://stackoverflow.com/questions/16781551/cost-function-in-cv-glm-of-boot-library-in-r
library(boot)

train.diag <- glm.diag(fit)
glm.diag.plots(fit, train.diag)

cost <- function(y, pred) mean(abs(y-pred) > 0.5)
cv.glm(data=train.subset, glmfit=fit, cost(as.integer(train.subset$V2)-1, fit$fitted), K=10)

# Call with a slightly larger train set:
rmse <- DoCV(train$V2, train[, cols], 10)
