# DataScienceLondon_SciKitLearn - R try

# https://github.com/dmcgarry/Data_Science_London_scilearn
# http://www.kaggle.com/c/data-science-london-scikit-learn/visualization
# TIPS: https://github.com/dmcgarry/Data_Science_London_scilearn

set.seed(16071962)

# Create the submisson file with the function name as part of the filename
CreateSubmission <- function(functionName, solution) {
  submissionfolder <- "C:/coding/Kaggle/DataScienceLondon_SciKitLearn/R/Submissions/"
  write.csv(data.frame(id=1:length(solution), solution),
            file=paste0(submissionfolder, "Submission_", functionName, "_",
                        format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F)
  
}

Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  # Find the normalized (0-1 range) value of curval
  normalized <- (curval - minval) / (maxval - minval) # This gives normalized as 'normalized2' var below
  normalized
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

# Get the data:
folder <- "C:/coding/Kaggle/DataScienceLondon_SciKitLearn/R/"
datafolder <- "C:/coding/Kaggle/DataScienceLondon_SciKitLearn/R/Data/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=F)
  trainLabels <- read.csv(paste0(datafolder, "trainLabels.csv"), header=F)
  test <- read.csv(paste0(datafolder, "test.csv"), header=F)
  save(train, file=paste0(datafolder, "train.rda"))
  save(trainLabels, file=paste0(datafolder, "trainLabels.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  load(paste0(datafolder, "train.rda"))
  load(paste0(datafolder, "trainLabels.rda"))
  load(paste0(datafolder, "test.rda"))
}

head(train, n=1)
head(trainLabels, n=10)
dim(train)
dim(trainLabels)
dim(test)

names(trainLabels) <- "Label"
test <- cbind(0, test[1:ncol(test)])
names(test)[1] <- "Label"
head(test, n=1)
# Bind Label col to train set
train <- cbind(trainLabels, train)

cors <- numeric(ncol(train) - 1)
#cors <- numeric(0)

for (counter in 2:ncol(train)) {
  cors[counter - 1] <- cor(trainLabels[, 1], train[, counter])
}

plot(cors, pch=19, col="blue", main="Correlations", cex.lab=.8, xaxt="n", ylab="Value", cex.main=1)
axis(side=1, at=1:(length(cors)), labels=names(train)[-1], cex.axis=.6, las=2)
abline(v=which(cors == max(cors)), col="cyan", lty=2)
# Find the col name:
names(train)[which(cors == max(cors)) + 1]

# Check for mean and variance:
plot(apply(train, 2, var), type="l", col="blue", main="Var", cex.lab=.8, xaxt="n", ylab="Value", cex.main=1)
axis(side=1, at=1:dim(train)[2], labels=names(train), cex.axis=.6, las=2)
grid()
plot(apply(test, 2, var), type="l", col="blue", main="Var", cex.lab=.8, xaxt="n", ylab="Value", cex.main=1)
axis(side=1, at=1:dim(train)[2], labels=names(train), cex.axis=.6, las=2)
grid()
plot(apply(train, 2, mean), type="l", col="blue", main="Mean", cex.lab=.8, xaxt="n", ylab="Value", cex.main=1)
axis(side=1, at=1:dim(train)[2], labels=names(train), cex.axis=.6, las=2)
grid()
plot(apply(test, 2, mean), type="l", col="blue", main="Mean", cex.lab=.8, xaxt="n", ylab="Value", cex.main=1)
axis(side=1, at=1:dim(train)[2], labels=names(train), cex.axis=.6, las=2)
grid()

# Looks like rows with Label = 0 has a lower average/min/max value
mean(colMeans(train[train$Label == 0, ]))
mean(colMeans(train[train$Label == 1, ]))
min(colMeans(train[train$Label == 0, ]))
min(colMeans(train[train$Label == 1, ]))
max(colMeans(train[train$Label == 0, ]))
max(colMeans(train[train$Label == 1, ]))

# Have a look at some rows as an image (TODO: Values must be normalised to RGB range)
pal.1 <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias=1, n.steps.between=c(10, 1, 10))
par(mfrow=c(3, 3))
oldmar <- par()$mar
par(mar=c(4, 1, 1, 1))

for (counter in 1:9) {
  tempdata <- as.numeric(train[sample(1:nrow(train), 1), ])
  y <- as.integer(tempdata[1])
  tempdata <- tempdata[-1]
  tempdata <- Normalize(min(tempdata), max(tempdata), 0, 255, tempdata)
  z <- matrix(data=tempdata, nrow=5, ncol=8, byrow=T)
  image(t(z)[,nrow(z):1], xlab=paste0("Label:", y)) #col=pal.1(28*28))
}

par(mar=oldmar)
par(mfrow=c(1, 1))

# Create an image/heatmap from the train data (scale vars), to see if a pattern emerges:
# http://33sticks.com/build-a-simple-heatmap-using-r/
# http://www.r-bloggers.com/heat-maps-using-r/
# http://stackoverflow.com/questions/5294955/how-to-scale-down-a-range-of-numbers-with-a-known-min-and-max-value
trainOrdered <- train[order(train$Label), ]
trainScaled <- scale(trainOrdered[, -1])
trainMatrix <- matrix(trainScaled, nrow=nrow(trainOrdered), ncol=40, byrow=T)
row.names(trainMatrix) <- trainOrdered$Label
color = rev(heat.colors(256))
trainHeatmap <- heatmap(trainMatrix, Rowv=NA, Colv=NA, col=color, scale="column",
                        margins=c(3,3), labRow=NULL, verbose=F)

hc.rows <- hclust(dist(trainScaled))
plot(hc.rows)
hc.cols <- hclust(dist(t(trainScaled)))
# Draw heatmap for first cluster
heatmap(trainScaled[cutree(hc.rows, k=2) == 1, ], Colv=as.dendrogram(hc.cols), scale='none')
# Draw heatmap for second cluster
heatmap(trainScaled[cutree(hc.rows, k=2) == 2, ], Colv=as.dendrogram(hc.cols), scale='none')


# Try a glm with just one IV:
lm1 <- glm(as.factor(Label) ~ V15 + V13 + V19 + V37, data=train, family=binomial(link="logit"))
summary(lm1)
test.pred <- predict.glm(lm1, newdata=test, type="response")
solution <- ifelse(test.pred < 0.5, 0, 1)

# Create the submisson file:
CreateSubmission("GLM", solution)


# Try a random forest:
library(randomForest)

percent.train <- nrow(train) / nrow(test) * 100
train.rows <- floor((nrow(train) * percent.train) / 100)
getrows <- sample(nrow(train), size=train.rows, replace=F)
train.limited <- train[getrows, ]
test.limited <- train[-getrows, ]
Labels <- as.factor(train.limited[,1])

result <- integer()
pos <- 1

# Get best tree number:
for (counter in seq(1, 70, 1)) {
  Sys.time()
  rf1_1 <- randomForest(as.factor(Label) ~ ., data=train.limited, ntree=counter)
  Sys.time()
  pred.rf <- predict(rf1_1, newdata=test.limited, type="response")
  prediction <- levels(Labels)[pred.rf]
  prediction
  
  the.result <- (as.integer(prediction) == test.limited$Label)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(test.limited)))
  pos <- pos + 1
}

plot(result, pch=19, col="blue", main="Random Forest Error Rate")
lines(result, col="steelblue3")

forestTrain1 <- randomForest(as.factor(Label) ~ ., train, proximity=T, keep.forest=T, ntree=70) #norm.votes=F)
forestTrain2 <- randomForest(as.factor(Label) ~ ., train, proximity=T, keep.forest=T, ntree=70)
forestTrain3 <- randomForest(as.factor(Label) ~ ., train, proximity=T, keep.forest=T, ntree=70)
forestTrain4 <- randomForest(as.factor(Label) ~ ., train, proximity=T, keep.forest=T, ntree=70)
forestTrain5 <- randomForest(as.factor(Label) ~ ., train, proximity=T, keep.forest=T, ntree=70)
forestCombined <- combine(forestTrain1, forestTrain2, forestTrain3, forestTrain4, forestTrain5)
prediction.combined <- predict(forestCombined, test, type="class")
solution <- ifelse(prediction.combined == 0, 0, 1)

# Last entry was: ntree=70, 5 rf's combined. Score: 0.87370
# Create the submisson file:
CreateSubmission("RF", solution)

oldmar <- par$par
par(mar=c(3,3,3,3))
var.importance <- forestCombined$importance[forestCombined$importance > 10]
names <- rownames(forestCombined$importance)[forestCombined$importance > 10]
df <- data.frame(names, var.importance)
plot(df$var.importance, pch=16, cex=1.4, col="blue", xaxt="n",
     main="Var.Importance MeanDecreaseGini")
symbols(x=1:nrow(df), y=df$var.importance, xlab="Variable", ylab="Importance",
        circles=df$var.importance, inches=1/4, ann=F, bg="steelblue3", fg=NULL, xaxt="n")
axis(side=1, at=1:nrow(df),
     cex.axis=.6, labels=df$names, col="black", col.axis="steelblue4")
par(mar=oldmar)

# -----------------------------------------------------------------------------------------------------------------------
# PCA:
# TIPS: http://www.kaggle.com/c/digit-recognizer/forums/t/4038/pca-application-to-digit-recognizer
# C:\coding\R\Coursera\StatisticalLearning\R\ch10.html
# http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
# http://nbviewer.ipython.org/gist/luanjunyi/6632d4c0f92bc30750f4
# http://www.ic.unicamp.br/~wainer/cursos/1s2013/ml/Lecture18_PCA.pdf

summary(pc.cr <- princomp(train[, -1], scores=TRUE))
print(pc.cr)
loadings(pc.cr)
plot(pc.cr, cex=.7, cex.lab=.8, cex.axis=.8, las=2) # Shows a screeplot

pca.out <- prcomp(train[, -1], scale=T, center=T, retx=T)
pca.out
plot(pca.out, xaxt="n")
plot(pca.out$sdev, pch=21, col="blue", bg="cyan", xaxt="n", type="b", cex.axis=.8)
axis(side=1, at=1:length(pca.out$sdev), cex.axis=.6, las=2)
biplot(pca.out, scale=0, cex=.6, cex.lab=.8, cex.axis=.8, col=c("blue", "red"))
print(pca.out$sdev)
plot(pca.out, type="l", col="blue", bg="cyan", pch=21, cex.lab=.8, cex.axis=.8)
summary(pca.out)
result <- predict(pca.out, newdata=test[, -1]) # Returns 36000 rows, 4 times as much as 9000...
length(result[,1]) # 9000
result <- ifelse(plogis(rowMeans(result)) < 0.5, 0, 1)
CreateSubmission("PCA", result) # Probably not correct......

require(caret)
trans <- preProcess(train, method=c("BoxCox", "center", "scale", "pca"))
PC <- predict(trans, newdata=test)

# TODO: PCA on caret package (http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/)

# -----------------------------------------------------------------------------------------------------------------------
# KNN:
# TODO: Test prediction quality with various k=<n> on train/test subsets from train set!
# TODO: Create an error rate function to check against the train/test subsets from train set!
library(FNN)
# NOTE: Also knn1(train, test, cl) for 1-nearest neighbour classification

results <- knn(train, test, train[, 1], k=10, prob=F, use.all=T) # k=20, same score. Try k=2?
results.cv <- knn.cv(train, train[, 1], k=10, l=0, prob=F, use.all=T)
# attributes(.Last.value)
# Get the object's attributes
attributes(results)$prob # Use prob=T above to return this attribute
# Or use:
attr(results, "prob")
solution <- as.numeric(levels(results)[results])


# Create the submisson file:
# Last entry was: k=10. Score: 0.88823. k=30 gave same score.
CreateSubmission("KNN", solution)


# GMM:
# Generalized Method of Moments and Generalized Empirical Likelihood
package.install("gmm")
library(gmm)
gmm(as.factor(train$Label) ~ train$V15 + train$V13 + train$V19 + train$V37, x=train)
# Not correct...


# SVM:
# http://stackoverflow.com/questions/1753299/help-using-predict-for-kernlabs-svm-in-r
library(kernlab)
library(e1071)

# TODO: Datasets must be scaled/normalized before use??

# Try svm from e1071 lib:
# https://github.com/JetGoodson/LitteraLector/blob/master/rageAgainstTheSupportVectorMachine.R
#responseVector <- as.vector(as.matrix(train[,1]))
#model <- svm(x=train[,-1], y=responseVector, scale=FALSE,
#             type="C-classification", kernel="polynomial", degree=poly,
#             coef0=bestCoef, gamma=bestGamma, cost=bestCost, cross=10, probability=TRUE)
#rm(responseVector)

# kernel='rbf' for nonlinear SVM
rbf <- rbfdot(sigma=0.1)
cols <- c("V5","V7","V13","V15","V19","V23","V24","V29","V30","V33","V37")

#ksvm1 <- ksvm(Label ~ ., data=train, type="C-bsvc", kernel=rbf, C=10, prob.model=TRUE)
ksvm1 <-  ksvm(train$Label ~., data=scale(train[, cols], center=T, scale=T), kernel="rbfdot", kpar=list(sigma=0.015),
              C=70, cross=4, prob.model=TRUE)
fitted(ksvm1)
#result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="probabilities")
result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="response")
#solution <- ifelse(result[,1] > result[,2], 0, 1)
result <- scale(result, center=T, scale=T)
result <- plogis(result)
solution <- ifelse(result < 0.5, 0, 1)

# Create the submisson file:
CreateSubmission("KSVM", solution)
# This creates best entry (Submission_KSVM_20140801_1102): 0.91170


# http://www.r-bloggers.com/learning-kernels-svm/
rbf <- rbfdot(sigma=0.1)
svm1 <- svm(Label ~ ., data=train, type="C-classification", gamma=.1, kernel=rbf, C=10, prob.model=TRUE)
fitted(svm1)
result <- predict(svm1, newdata=test, type="probabilities")
solution <- as.integer(result)-1 # TODO: Wrong?? plogis transform here?
solution

# Create the submisson file:
CreateSubmission("SVM", solution)

######################################################################################################

# Create test/train subsets from train data:
rows <- sample(nrow(train), size=nrow(train) / 2, replace=F)
train.limited <- train[rows, ]
test.limited <- train[-rows, ]

ErrorRate <- function(y, solution) {
  return (1 - (length(y[y != solution]) / length(y)))
}

maxrun <- 10
solution <- numeric()
errorRate <- numeric(maxrun)
errorRates <- matrix(nrow=maxrun, ncol=nrow(test.limited))

for (counter in 1:maxrun) {
  results <- knn(train.limited, test.limited, train.limited[, 1], k=counter,
                 prob=F, use.all=T) 
  results.cv <- knn.cv(train.limited, train.limited[, 1], k=counter, l=0, prob=F, use.all=T)
  solution <- as.numeric(levels(results)[results])
  errorRate[counter] <- ErrorRate(test.limited[, 1], solution)
  errorRates[counter, ] <- solution
}

# Looks like k=3 and k=10 is most consistent in good scores
errorData <- data.frame(x=1:maxrun, y=errorRate)
ggplot(data=errorData, aes(x=x, y=errorRate)) + geom_line(col="green4") +
  geom_point(col="blue", aes(size=5)) + ggtitle("Score")

# Try a run of k=10, and do a majority vote:
solution <- numeric()
errorRate <- numeric(maxrun)
errorRates <- matrix(nrow=maxrun, ncol=nrow(test.limited))

for (counter in 1:maxrun) {
  k.num <- ifelse(counter %% 2 == 0, 10, 10)
  results <- knn(train.limited, test.limited, train.limited[, 1], k=k.num,
                 prob=F, use.all=T) 
  results.cv <- knn.cv(train.limited, train.limited[, 1], k=k.num, l=0, prob=F, use.all=T)
  solution <- as.numeric(levels(results)[results])
  errorRate[counter] <- ErrorRate(test.limited[, 1], solution)
  errorRates[counter, ] <- solution
}

#finalVote <- ifelse((colMeans(errorRates) * maxrun) < (maxrun / 2), 0, 1)
finalVote <- ifelse(colSums(errorRates) > (nrow(errorRates) / 2), 1, 0)

hist(colMeans(errorRates) * maxrun, col="orange")
problemCols <- which((colMeans(errorRates) * maxrun) < maxrun & (colMeans(errorRates) * maxrun) > 0)

total <- nrow(train.limited)
correct <- (finalVote == test.limited[, 1])
correct <- length(correct[correct == TRUE])
wrong <- total - correct
barplot(c(correct/total*100, wrong/total*100), ylim=c(0, 100), xlab="Correct / Wrong",
        ylab="Percent", col=c("Green", "Red"), main="Score")

# Try again without the problem cols
train.limited2 <- train.limited[-problemCols]
test.limited2 <- test.limited[-problemCols]

# Try a run of k=10, and do a majority vote:
solution <- numeric()
errorRate <- numeric(maxrun)
errorRates <- matrix(nrow=maxrun, ncol=nrow(test.limited2))

for (counter in 1:maxrun) {
  k.num <- ifelse(counter %% 2 == 0, 10, 10)
  results <- knn(train.limited2, test.limited2, train.limited2[, 1], k=k.num,
                 prob=F, use.all=T) 
  results.cv <- knn.cv(train.limited2, train.limited2[, 1], k=k.num, l=0, prob=F, use.all=T)
  solution <- as.numeric(levels(results)[results])
  errorRate[counter] <- ErrorRate(test.limited2[, 1], solution)
  errorRates[counter, ] <- solution
}

finalVote <- ifelse((colMeans(errorRates) * maxrun) < (maxrun / 2), 0, 1)
hist(colMeans(errorRates) * maxrun, col="orange")
total <- nrow(train.limited2)
correct <- (finalVote == test.limited2[, 1])
correct <- length(correct[correct == TRUE])
wrong <- total - correct
barplot(c(correct/total*100, wrong/total*100), ylim=c(0, 100), xlab="Correct / Wrong",
        ylab="Percent", col=c("Green", "Red"), main="Score")

errorData <- data.frame(x=1:maxrun, y=errorRate)
ggplot(data=errorData, aes(x=x, y=errorRate)) + geom_line(col="green4") +
  geom_point(col="blue", aes(size=5)) + ggtitle("Score")
