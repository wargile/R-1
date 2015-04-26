# UCI ML Repository, WineQuality datasets
# ---------------------------------------

set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)

datafolder <- "C:/coding/R/Coursera/DataAnalysisAndStatisticalInference/Project/Data/" # TODO: Read from same folder
red.wine <- read.csv(paste0(datafolder, "winequality-red.csv"), header=T, sep=";")
white.wine <- read.csv(paste0(datafolder, "winequality-white.csv"), header=T, sep=";")

dim(red.wine)
str(red.wine)
summary(red.wine)

head(red.wine, n=1)
pairs(red.wine, col="blue")
CorrelationPlot(red.wine)
class(red.wine)

plot(fixed.acidity ~ density, data=red.wine, pch=21, bg="cyan", col="blue")

par(mfrow=c(2,1))
plot(quality ~ density, data=red.wine, pch=21, bg="cyan", col="blue", main="quality ~ density")
plot(quality ~ alcohol, data=red.wine, pch=21, bg="cyan", col="blue", main="quality ~ alcohol")
plot(quality ~ sulphates, data=red.wine, pch=21, bg="cyan", col="blue", main="quality ~ sulphates")
plot(chlorides ~ residual.sugar, data=red.wine, pch=21, bg="cyan", col="blue", main="chlorides ~ residual.sugar")
plot(chlorides ~ log(residual.sugar), data=red.wine, pch=21, bg="cyan", col="blue", main="chlorides ~ log(residual.sugar)")
par(mfrow=c(1,1))

# Check for outliers:
boxplot(red.wine$total.sulfur.dioxide, col="wheat") # Severe outliers!
boxplot(red.wine$chlorides, col="wheat") # Severe outliers!
boxplot(red.wine$pH, col="wheat") # Severe outliers!
boxplot(red.wine$volatile.acidity, col="wheat") # Severe outliers!
boxplot(red.wine$alcohol, col="wheat") # Severe outliers!
boxplot(red.wine$residual.sugar, col="orange") # Severe outliers!

red.wine[which.max(red.wine$total.sulfur.dioxide),]
with(red.wine, cor(fixed.acidity, volatile.acidity))

result <- CreateTrainAndValidationSets(red.wine)
red.wine.train <- result[[1]]
red.wine.validate <- result[[2]]

# -----------------------------------------------------------------------------------------------------------------------
# Create models and predict:

model1 <- lm(quality ~ volatile.acidity + alcohol + sulphates + pH + total.sulfur.dioxide, data=red.wine.train)
summary(model1)
model2 <- randomForest(quality ~ volatile.acidity + alcohol + sulphates + pH + total.sulfur.dioxide, data=red.wine.train,
                       ntree=500)
model2 <- randomForest(quality ~ ., data=red.wine.train, ntree=1000)
summary(model2)
varImpPlot(model2)
model3 <- gbm(quality ~ volatile.acidity + alcohol + sulphates + pH + total.sulfur.dioxide, data=red.wine.train,
                       n.trees=500, distribution="multinomial", cv.folds=5)
summary(model3, las=2, cex.lab=.7)
# TODO: Try trees!
library(tree)
# Do a Classification tree:
model4 <- tree(as.factor(quality) ~ volatile.acidity + alcohol + sulphates + pH + total.sulfur.dioxide, data=red.wine.train)
plot(model4)
text(model4, cex=.7, col="blue")
summary(model4)
# Number of terminal nodes:  13 
# Residual mean deviance:  0.402 = 477 / 1190 
partition.tree(model4, label="quality", add=T) # ??
legend(1.75, 4.5, legend=unique(red.wine.train$quality), col=unique(as.numeric(red.wine.train$quality)), pch=19)

plot(cv.tree(model4, FUN=prune.tree, method="misclass"))
plot(cv.tree(model4), cex.lab=.7, cex.axis=.7)
pruneTree <- prune.tree(model4, best=10)

pred <- predict(model1, newdata=red.wine.validate, type="response")
pred <- predict(model2, newdata=red.wine.validate, type="response")
pred <- predict(model3, newdata=red.wine.validate, type="response", n.trees=500)
# For GBM:
pred <- sapply(1:nrow(pred), function(x) which.max(pred[x,1:5,]))
pred <- pred + 2

pred <- predict(model4, newdata=red.wine.validate, type="class") # NOTE: y must be factor for "class" type to work!
pred <- as.numeric(pred) + 2

# -------------------------------------------------------------------------------------------------------------------
# Evaluate:

SSE <- sum((red.wine.validate$quality - as.numeric(pred))^2) 
SSE

RMSE <- sqrt(SSE / nrow(red.wine.validate))
RMSE
# Same as:
MyRMSE(red.wine.validate$quality, as.numeric(pred))

SST <- sum((red.wine.validate$quality - mean(red.wine.train$quality))^2)
SST

R2 <- 1 - SSE/SST
R2

barplot(table(round(as.numeric(pred)) == red.wine.validate$quality), col=c("Red","Green"))

plot(jitter(red.wine.validate$quality[1:200]), type="p", col="darkcyan")
points(pred[1:200], col="violet")
  
# ------------------------------------------------------------------------------------------------------------------------------

n <- seq(-pi,pi,.02)^3
y <- (n * abs(rnorm(length(n))))
x <- seq(1, length(y))
plot(y, col="blue")
model1 <- lm(y ~ x)
summary(model1)
lines(model1$fitted, col="cyan", lwd=2)
model2 <- lm(y ~ poly(x, 2))
summary(model2)
lines(model2$fitted, col="red", lwd=2)
model3 <- lm(y ~ poly(x, 3))
summary(model3)
lines(model3$fitted, col="green3", lwd=2)
model4 <- lm(y ~ poly(x, 4))
summary(model4)
lines(model4$fitted, col="violet", lwd=2)
