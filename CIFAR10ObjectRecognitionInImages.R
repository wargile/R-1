CIFAR-10 - Object Recognition in Images
=======================================

## Object recognition in images

# http://www.kaggle.com/c/cifar-10/data
# TIPS: 
# http://www.kaggle.com/c/cifar-10/forums/t/6318/what-data-prep-you-all-are-doing
# http://www.kaggle.com/c/cifar-10/forums/t/7210/using-r
# http://ufldl.stanford.edu/wiki/index.php/UFLDL_Tutorial (PCA and Whitening)

# Using Global Contrast Normalization and ZCA whitening for preprocessing.
# My implementation  can be found at https://github.com/nagadomi/kaggle-cifar10-torch7

## Get the data

```{r Get data}
set.seed(16071962)
source("tools.R")
# http://cran.r-project.org/web/packages/png/png.pdf
package.install("png")
library(png)

# memory.limit()
folder <- "C:/coding/Kaggle/CIFAR10ObjectRecognitionInImages/"
datafolder <- "C:/coding/Kaggle/CIFAR10ObjectRecognitionInImages/Data/"
submissionfolder <- "C:/coding/Kaggle/CIFAR10ObjectRecognitionInImages/Submissions/"

trainLabels <- read.csv(paste0(datafolder, "trainLabels.csv"), header=T)
head(trainLabels, n=2)

pixels <- 32
png <- readPNG(paste0(datafolder, "train/train/2.png"))
numfiles <- dim(trainLabels)[1]
numfiles <- 100 # TEST

# Read in all the png files in training set, and cbind to trainLabels label col:
library(plyr)
names <- c(rep(paste0("pixel", 1:(pixels * pixels))), "label")
files <- data.frame()
for (counter in 1:numfiles) {
  filename <- paste0(datafolder, "train/train/", counter, ".png")
  png <- readPNG(filename)
  p <- list(png[1:1024])
  df <- ldply(p)
  files.temp <- cbind(df, trainLabels$label[counter])
  files <- rbind(files, files.temp)
  print(paste0("Iteration ", counter, "..."))
  flush.console()
}
colnames(files) <- names
head(files)

# Save the files object to .rda
save(files, file=paste0(datafolder, "files.rda"))

z <- matrix(data=as.numeric(files[2,-(pixels*pixels+1)]), nrow=pixels, ncol=pixels, byrow=F)

# Have a look at some images:
op <- par()
pal.1 <- colorRampPalette(c("blue", "cyan", "yellow", "red"), bias=1)
par(mar=c(3, 6, 3, 6))

z <- matrix(data=as.numeric(png), nrow=pixels, ncol=pixels, byrow=F)
image(t(z)[,nrow(z):1], col=pal.1(pixels*pixels))

# Show the image as-is:
plot(1)
rasterImage(png, 0.8, 0.8, 1.2, 1.2)

# Show image with color info another way:
# http://www.r-bloggers.com/r-k-means-clustering-on-an-image/
# Obtain the dimension
imgDm <- dim(png)
# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each=imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(png[, , 1]),
  G = as.vector(png[, , 2]),
  B = as.vector(png[, , 3])
)
# Create a custom styling function for ggplot:
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 1,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 1),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 18,
      color="steelblue4",
      face = "bold",
      vjust = 1.5)
  )
}
# Display the image:
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image") +
  xlab("x") +
  ylab("y") + plotTheme()


par <- op

# Test our Binarize function (tools.R):
pal.2 <- colorRampPalette(c("black", "white"), bias=1)
image(Binarize(t(z), max(z)/2)[,nrow(z):1], col=pal.2(pixels*pixels))

# Test our Normalize function (tools.R):
pal.2 <- colorRampPalette(c("black", "white"), bias=1)
image(Normalize(min(z), max(z), 0, 1, t(z))[,nrow(z):1], col=pal.2(pixels*pixels))

# Image processing: Get some example data:
destFile="C:/coding/R/TestData/face.rda"
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda", destfile=destFile)
load(destFile)
image(t(faceData)[, nrow(faceData):1])

# Get a subset of cols to centralize image, and reduce size:
# Do start 20% in and to 80% (rest seems to be empty/0 values)
pixels <- ncol(train) - 1
pixel.start <- 20
pixel.start <- floor(pixels * (pixel.start / 100))
pixel.end <- pixels - pixel.start
my.image.data <- as.integer(train[4, -1])

my.subset <- seq(1, length(my.image.data), 4)
my.image.data <- my.image.data[my.subset]
z <- matrix(data=my.image.data, nrow=14, ncol=14, byrow=T)
image(t(z)[, nrow(z):1])

z <- matrix(data=as.integer(train[4, -1]), nrow=pixels, ncol=pixels, byrow=F)
image(t(z)[, nrow(z):1])
#svd1 <- svd(x=scale(z))
svd1 <- svd(x=z)
plot(svd1$d^2/sum(svd1$d^2), pch=19, col="blue", xlab="Singular vector", ylab="Variance explained")

oldmar=par$mar
par(mar=c(1, 1, 1, 1))
par(mfrow=c(2, 3))
for (counter in seq(5, 25, 5)) {
  approx <- svd1$u[, 1:counter] %*% diag(svd1$d[1:counter])%*% t(svd1$v[, 1:counter]) 
  image(t(approx)[, nrow(approx):1])
}
par(mfrow=c(1, 1))
par(mar=oldmar)

# Check for completeness
unique(complete.cases(train),)

for (counter in 2:nrow(train)) {
  cor.df$ColName[counter - 1] <- names(train)[counter]
  cor.df$Coeff[counter - 1] <- cor(train[,1], train[,counter])
}

head(cor.df)
```

## Create a sample from the training set and partition into train and validation subsets with correct train/test row ratio

```{r Do Train/Test Paritition}
# TODO...
```

## Do various tests and predictions from the subset

```{r Do Train/Test Paritition Predictions}
# Do clustering
# Tip: Clustering good for image recognition!
# http://www.statmethods.net/advstats/cluster.html
data.train.scaled <- scale(data.train)
d <- dist(data.train.scaled, method="euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters
# Model Based Clustering
library(mclust)
fit <- Mclust(data.train.scaled)
plot(fit) # plot results 
summary(fit) # display the best model
```

## Do KNN

```{r Do KNN}
# https://www.kaggle.com/wiki/KNearestNeighbors

# makes the KNN submission
package.install("FNN")
library(FNN)

labels <- train[,1]
train1 <- train[,-1]

results <- (0:9)[knn(train1, test, labels, k=10)]
results <- knn(train1, test, labels, k=10)

submission.knn = data.frame(ImageId=1:nrow(test), Label=results)
head(submission.knn)

write.csv(submission.knn, file=paste0(submissionfolder,
                                      "knn_pred1_benchmark_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)
```

# Do Random Forest

```{r Do Random Forest}
# https://www.kaggle.com/wiki/RandomForests

# Makes the random forest submission

library(randomForest)
library(medley)
library(kernlab)

labels <- as.factor(train[,1])
train2 <- train[,-1]

rf1 <- randomForest(labels ~ ., data=train2, ntree=100)
rf1.pred <- predict(rf1, test, type="class")
prediction1 <- levels(labels)[rf1.pred]
plot(rf1$importance, pch=19, col="blue", xaxt="n", xlab="Variable", ylab="Importance",
     main="rf1 Var.Importance MeanDecreaseGini")
significance <- sort(rf1$importance, decreasing=T, index.return=T)
significance$ix[1:10]
rf1$importance[significance$ix[1:10],]
plot(rf1$importance[significance$ix[1:20],], pch=19, col="blue")
rmse(as.integer(rf1.pred), test)

# TEST: Do just a few columns:
Sys.time()
trainLimited <- train[,-1]
trainLimited <- trainLimited[,c(382,407,410,414,489)]
names(trainLimited)
rfLimited1 <- randomForest(labels ~ ., data=trainLimited, ntree=800)
rfLimited2 <- randomForest(labels ~ ., data=trainLimited, ntree=800)
rfLimited3 <- randomForest(labels ~ ., data=trainLimited, ntree=800)
rfLimitedCombined <- combine(rfLimited1, rfLimited2, rfLimited3)
rfLimited.pred <- predict(rfLimitedCombined, test, type="response")
predictionLimited <- levels(labels)[rfLimited.pred]
rmse(as.integer(rfLimited.pred), test)
Sys.time()

# Trying a linear regression on randomForest variable importance
lm1 <- glm(as.numeric(labels) ~ pixel409 + pixel381 + pixel406 + pixel413 + pixel488,
           data=train, gaussian(link = "identity"))
summary(lm1)
lm1.pred <- predict(lm1, test, type="response") # TODO: How to interpret result??
rmse(lm1.pred, test)

submission.lm1 = data.frame(ImageId=1:nrow(test), Label=lm1.pred)
head(submission.lm1)

# TODO: Create a subset of the training data, and split into train and test subsets,
# with same row ratio as original sets (see Titanic.Rmd for code)

Sys.time()
rf1_1 <- randomForest(labels ~ ., data=train2, ntree=10, nodesize=5000)
Sys.time()
rf1_2 <- randomForest(labels ~ ., data=train2, ntree=1000)
Sys.time()
rf1_3 <- randomForest(labels ~ ., data=train2, ntree=1000)
Sys.time()
rfCombined <- combine(rf1_1, rf1_2, rf1_3)

rfCombined.pred <- predict(rfCombined, test, type="class")
predictionCombined <- levels(labels)[rfCombined.pred]
rfCombined$importance
plot(rfCombined$importance, pch=19, col="blue", xaxt="n", xlab="Variable", ylab="Importance",
     main="Combined Var.Importance MeanDecreaseGini")
axis(side=1, at=1:nrow(rfCombined$importance),
     cex.axis=.8, labels=rownames(rfCombined$importance), col="black", col.axis="dark violet")
rmse(as.integer(rfCombined.pred), test)


#submission.rf = data.frame(ImageId=1:nrow(test), Label=as.integer(prediction1))
#head(submission.rf)

submission.rfCombined = data.frame(ImageId=1:nrow(test), Label=as.integer(predictionCombined))
head(submission.rfCombined)

submission.rfLimited = data.frame(ImageId=1:nrow(test), Label=as.integer(predictionLimited))
head(submission.rfLimited)

#write.csv(submission.rf,
#          file=paste0(submissionfolder, "randomForest_rf1_benchmark_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)
write.csv(submission.rfCombined,
          file=paste0(submissionfolder, "randomForest_rfCombined_benchmark_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)
# rfCombined gives the best score: 0.96714 
write.csv(submission.rfLimited,
          file=paste0(submissionfolder, "randomForest_rfLimited_benchmark_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)
```

## Do Neural Network:

```{r Do NN}
# http://beckmw.wordpress.com/tag/neural-network/

library(nnet)
# NOTE: Too many weights if using formula = label ~ . (all IV's)
model1 <- nnet(as.factor(label) ~ pixel409 + pixel381 + pixel406 + pixel413 + pixel488, data=train, size=10, linout=F)
form.in <- as.formula('as.factor(label) ~ pixel409 + pixel381 + pixel406 + pixel413 + pixel488')
form.in <- as.formula('as.factor(label) ~ .')
model1 <- nnet(form.in, data=train, size=5, linout=T, MaxNWts=600)
model1 <- nnet(as.factor(label) ~ ., data=train, size=2, linout=T, MaxNWts=1600)
summary(model1)
package.install("devtools")
library(devtools)
plot.nnet(model1)
nnet1.pred <- predict(model1, test, type="class")
submission.nnet1 = data.frame(ImageId=1:nrow(test), Label=as.integer(nnet1.pred))
head(submission.nnet1, n=25)
write.csv(submission.nnet1,
          file=paste0(submissionfolder, "NNET_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)

library(neuralnet)
form.in <- as.formula('as.factor(label) ~ pixel409 + pixel381 + pixel406 + pixel413 + pixel488')
form.in <- as.formula('as.factor(label) ~ .')
model2 <- neuralnet(form.in, data=train, hidden=2) # 10
nnet2.pred <- predict(model2, test, type="class")
submission.nnet2 = data.frame(ImageId=1:nrow(test), Label=as.integer(nnet2.pred))
head(submission.nnet2)
write.csv(submission.nnet2,
          file=paste0(submissionfolder, "NeuralNet_", format(Sys.time(), "%Y%m%d"), ".csv"), row.names=F)
# http://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf

library(RSNNS)
model3 <- mlp(train$pixel409 + train$pixel381, as.factor(label), size=10, linOut=T)
nnet3.pred <- predict(model3, test, type="class")

# monmlp example:
# Sample data
package.install("monmlp")
library(monmlp)
n <- 1000
k <- 7
x <- matrix(rnorm(k*n), nrow=n)
w <- rnorm(k)
y <- ifelse(logistic(x %*% w) + rnorm(n, sd=0.2) > 1, 0, 1)

# Fit the model and compute the predictions
r <- monmlp.fit(x, y, hidden1=3, n.ensemble=15, monotone=1, bag=TRUE)
z <- monmlp.predict(x=x, weights=r)

# Compute the AUC
library(ROCR)
plot(performance(prediction(z, y), "tpr", "fpr"), col="blue")
performance(prediction(z, y), "auc")@y.values[[1]]

```

## Do PCA to find important IV's

```{r Do PCA}
# TIPS: http://www.kaggle.com/c/digit-recognizer/forums/t/4038/pca-application-to-digit-recognizer

summary(pc.cr <- princomp(train, scores=TRUE))
print(pc.cr)
loadings(pc.cr)
plot(pc.cr) # Shows a screeplot
#biplot(pc.cr)
```

## Do average of pixel values for the same number

```{r Do Average Pixel Values}
# TIPS: http://www.kaggle.com/c/digit-recognizer/prospector#70

# Create smaller subsets:
reduced.train <- nrow(train) / 100
reduced.test <- nrow(test) / 100
rows.train <- sample(nrow(train), size=reduced.train, replace=F)
rows.test <- sample(nrow(test), size=reduced.test, replace=F)

data.train <- train[rows.train,]
data.test <- train[rows.test,]

averageDigitFinalTrain <- numeric(10)
sliceDigitFinalTrain <- numeric(10)
minDigitFinalTrain <- numeric(10)
maxDigitFinalTrain <- numeric(10)

# Do start 20% in and to 80% (rest seems to be empty/0 values)
pixels <- ncol(train) - 1
pixel.start <- 20
pixel.start <- floor(pixels * (pixel.start / 100))
pixel.end <- pixels - pixel.start

for (counter in 0:9) {
  trainDigit <- numeric()
  
  # TODO: Need to get the pixel vector into a 28x28 matrix, and THEN take a limited col range or one col only!
  z <- matrix(data=colMeans(data.train[data.train$label == counter, -1]), nrow=28, ncol=28, byrow=T)
  z <- t(z)
  
  trainDigitSlice <- z[, 14] # NOTE: One col in z

  averageDigitFinalTrain[counter + 1] <- mean(colMeans(z))
  sliceDigitFinalTrain[counter + 1] <- mean(trainDigitSlice)
  #minDigitFinalTrain[counter + 1] <- min(rowMeans(trainDigit))
  #maxDigitFinalTrain[counter + 1] <- max(rowMeans(trainDigit))
}

averageDigitFinalTrain
minDigitFinalTrain
maxDigitFinalTrain

digitTest <- numeric(nrow(data.test))

#for (counter in 1:nrow(test)) {
for (counter in 1:10) {
  start <- 1000
  
  for (digitCounter in 1:10) {
    error <- sqrt((averageDigitFinalTrain[digitCounter] - mean(as.numeric(data.test[counter, c(pixel.start:pixel.end)])))^2)
    
    if (error < start) {
      start <- error
      number <- (digitCounter - 1)
    }
  }
  
  digitTest[counter] <- number
}

```

## More Random Forest

```{r Do Random Forest}

# Do random forest
library(randomForest)
library(medley)
library(kernlab)

labels <- as.factor(data.train[,1])
train2 <- data.train[,-1]
pos <- 1
result <- integer()

for (counter in seq(1, 200, 10)) {
	Sys.time()
	rf1_1 <- randomForest(labels ~ ., data=train2, ntree=counter)
	Sys.time()
	pred.rf <- predict(rf1_1, newdata=data.test, type="response")
	prediction <- levels(labels)[pred.rf]
	prediction

	the.result <- (as.integer(prediction) == data.test$label)
	result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(data.test)))
	pos <- pos + 1
}

plot(result, pch=19, col="blue", main="Random Forest Error Rate")
lines(result, col="steelblue3")

# Confusion matrix
# NOTE: Verification on x-axis, Prediction on y-axis
table(data.test$label, as.integer(prediction))
# Problem numbers in comparison: 5-8, 4-9, 3-5 
rmse(as.integer(prediction), data.test)


# Do KNN
library(FNN)

results <- (0:9)[knn(data.train, data.test, labels, k=10, prob=T)]
results <- knn(data.train, data.test, labels, k=10)
attributes(.Last.value)
```

