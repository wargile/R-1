# National Data Science Bowl
# http://www.kaggle.com/c/datasciencebowl/forums/t/11242/getting-started-a-tutorial
# Deadline: 16 Mar 2015

# TIPS/CODE:
# https://github.com/JeffHebert/Plankton_Classification/blob/master/plankton_starter_code_hebert.R#L97

# WINNING SOLUTION:
# http://benanne.github.io/2015/03/17/plankton.html
# https://www.kaggle.com/c/datasciencebowl/forums/t/12844/blog-post-describing-the-winning-solution/66643#post66643

# TIP: Scale all images to same size/aspect ratio:
# https://trac.ffmpeg.org/wiki/Scaling%20%28resizing%29%20with%20ffmpeg
# ffmpeg -i input.jpg -vf scale=320:240 output_320x240.jpg

set.seed(16071962)
source("tools.R")

# Set some standard graphical params for plot
oldpar <- par()
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)
par(mar=c(3,3,2,1))

#library(ff) # For huge files
#library(ffbase)

datafolder <- "C:/coding/Kaggle/NationalDataScienceBowl/Data/"
submissionsfolder <- "C:/coding/Kaggle/NationalDataScienceBowl/submissions/"

# TODO: Get individual JPG files in various folders!

package.install("jpeg")
library(jpeg)

# read a sample file
# img <- readJPEG(system.file("img", paste0(datafolder, "train/amphipods/4661.jpg"), package="jpeg"))
img <- readJPEG(paste0(datafolder, "train/ephyra/45707.jpg"))
img <- readJPEG(paste0(datafolder, "train/echinoderm_larva_pluteus_typeC/5692.jpg"))
img <- readJPEG(paste0(datafolder, "train/pteropod_butterfly/55077.jpg"))

# read it also in native format
# img.n <- readJPEG(system.file("img", paste0(datafolder, "train/amphipods/4661.jpg"), package="jpeg"), TRUE)

# files <- readFiles()... 

DegreesToRadians <- function(d) return(d * (pi / 180))
RadiansToDegrees <- function(r) return((r * 180) / pi)

# http://en.wikipedia.org/wiki/Rotation_matrix
theta <- DegreesToRadians(90) # find radians for degrees to use in rotation matrix
rotation.matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2, byrow=T) # rotation matrix for any angle

# if your R supports it, we'll plot it
if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
  #plot(1:2, type='n')
  #plot(1:1, type='n')
  #rasterImage(img, 1.2, 1.27, 1.8, 1.73)
  #rasterImage(img, 1.5, 1.5, 1.9, 1.8)
  par(mfrow=c(2,2))
  image((img), asp=1)
  image((Binarize(img, threshold=.9)), asp=1)
  img2 <- as.matrix(t(img)[,nrow(img):1]) # NOTE: Rotate the image 90 degrees
  image(img2, asp=1)
  image((Binarize(img2, threshold=.9)), asp=1)
  par(mfrow=c(1,1))
}

# Idea: Binarize the images, then get the ratio of black versus white pixels in the image
img3 <- Binarize(img2, threshold=.9)
image(img3, asp=1)
result <- as.numeric(table(img3))
bw.ratio <- result[1] / result[2]
bw.ratio

# Smaller image version
interval <- 4
img4 <- img3[seq(nrow(img3), 1, by=-interval), ]
img4 <- img4[, seq(ncol(img4), 1, by=-interval)]
image(t(img4)[,nrow(img4):1], asp=1) #col=pal.1(28*28))

# Multinomial tips:
# http://www.ats.ucla.edu/stat/r/dae/mlogit.htm

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=T, sep=",")
  test <- read.csv(paste0(datafolder, "test.csv"), header=T, sep=",")
  save(train, file=paste0(datafolder, "train.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  load(paste0(datafolder, "train.rda"))
  load(paste0(datafolder, "test.rda"))
}

dim(train)
head(train, n=5)
sapply(train, class)
summary(train)
# numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))

dim(test)
head(test, n=1)
sapply(test, class)
summary(test)

# Split training data in training and validation subsets
result <- CreateTrainAndValidationSets(train,
                                       percent.train=75, random.selection=F)
train.subset <- result$train
validation.subset <- result$validation

# Predict and get RMSE on validation set
# hour2 + device_make + device_conn_type + device_os + site_category + device_geo_country + C1 + C2 + C24

# -------------------------------------------------------------------------------------------------------------------
# Try GLM
fit <- lm(hand ~ ., data=train.subset)
summary(fit)
prediction <- predict(fit, newdata=validation.subset, type="response")
prediction[1:10]
rmse <- MyRMSE(as.numeric(validation.subset$click), as.numeric(prediction))


# Try randomForest
library(randomForest)
result <- integer()
Sys.time()
for (counter in seq(1, 25, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(hand ~ ., data=train.subset,
                               proximity=TRUE, keep.forest=TRUE, ntree=counter,
                               nodesize=5)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=validation.subset, type="response")
  the.result <- (prediction == validation.subset$hand)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(validation.subset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()
best.ntrees <- which(result == min(result))


# Try GBM
library(gbm)
# TODO: Filter out cols that have no variation?
# lengths <- sapply(train, function(x) length(unique(x)))
# lengths <- which(lengths == 1)
GBM_NTREES = 50
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 3
GBM_MINOBS = 5
fit <- gbm(hand ~ ., data=train.subset,
           distribution="gaussian", n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE,
           interaction.depth=GBM_DEPTH, n.minobsinnode=GBM_MINOBS) 
prediction <- predict(fit, newdata=validation.subset, type="response", n.trees=GBM_NTREES)
prediction[1:10]
rmse <- MyRMSE(as.numeric(validation.subset$hand), as.numeric(prediction))

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


# Try H2o
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
# 2.8.4.4 is latest release
install.packages("h2o", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-noether/4/index.html", getOption("repos"))))
library(h2o)
localH2O = h2o.init()

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)

cols <- c("hour2","device_make","device_conn_type","device_os","C1","C24")
train.subset <- train.subset[, cols]
validation.subset <- validation.subset[, cols]

train_h20 <- as.h2o(localH2O, train.subset, key='train')
test_h20 <- as.h2o(localH2O, validation.subset, key='test')

## Split the dataset into 80:20 for training and validation
train_split <- h2o.splitFrame(train_h20, ratios=0.8, shuffle=F)

# TODO: Implement model
model <- h2o.deeplearning(x = cols,
                          y = 2,
                          data = train_split[[1]],
                          validation = train_split[[2]],
                          activation = "Rectifier",
                          hidden = c(50, 50, 50),
                          epochs = 100,
                          classification = FALSE,
                          balance_classes = FALSE)


# Try SVM
# SVM:
# http://stackoverflow.com/questions/1753299/help-using-predict-for-kernlabs-svm-in-r
library(kernlab)
library(e1071)

# kernel='rbf' for nonlinear SVM
rbf <- rbfdot(sigma=0.1)

#ksvm1 <- ksvm(Label ~ ., data=train, type="C-bsvc", kernel=rbf, C=10, prob.model=TRUE)
cols <- c("hour2","device_make","device_conn_type","device_os","C1","C24")
ksvm1 <-  ksvm(train.subset$click ~., data=scale(train.subset[, cols], center=T, scale=T),
               kernel="rbfdot", kpar=list(sigma=0.015),
               C=70, cross=4, prob.model=TRUE)
ksvm1 <-  ksvm(train.subset$click ~., data=train.subset[, cols],
               kernel="rbfdot", kpar=list(sigma=0.015),
               C=70, cross=4, prob.model=TRUE)
fitted(ksvm1)
#result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="probabilities")
#result <- predict(ksvm1, newdata=scale(test[, cols], center=T, scale=T), type="response")
result <- predict(ksvm1, newdata=validation.subset[, cols], type="response")
#solution <- ifelse(result[,1] > result[,2], 0, 1)
result <- scale(result, center=T, scale=T)
result <- plogis(result)
solution <- ifelse(result < 0.5, 0, 1)


# -------------------------------------------------------------------------------------------------------------------

op <- par()
par(mfrow=c(1,1))
par(mar=c(2.5,4,2,1))
n <- 50 # If plotting subset
y.min <- ifelse(min(validation.subset$y) < min(prediction),
                min(validation.subset$y), min(prediction))
y.max <- ifelse(max(validation.subset$y) > max(prediction),
                max(validation.subset$y), max(prediction))
plot(validation.subset$y, type="o", col="blue", xlab="x", ylab="y/pred",
     main=paste0("y/pred (RMSE: ", round(rmse, 3), ")"), ylim=c(y.min, y.max))
points(prediction, col="red", type="o")
legend("topleft", legend=c("y","pred"), col=c("blue","red"), lwd=2)
par <- op


# Create the submission file
KaggleSubmission(submission, submissionsfolder, "GLM_binom")
