# Kaggle Africa Soil Property Prediction Challenge

# TIPS:
# http://afsiskaggle.qed.ai/
# https://github.com/hiransuvrat/machinelearning
# http://www.kaggle.com/c/afsis-soil-properties/forums/t/10176/simple-linear-regression-starting-kit
# http://cran.r-project.org/web/packages/glmnet/index.html
# https://www.kaggle.com/c/afsis-soil-properties/forums/t/10136/beat-zeros-benchmark

# http://www.r-bloggers.com/h2o-domino-kaggle-quick-start-guide-and-rugsmaps2/
# http://blog.dominoup.com/using-r-h2o-and-domino-for-a-kaggle-competition/

# http://www.kaggle.com/c/afsis-soil-properties/forums/t/10676/solutions-sharing

# "We suggest you to remove spectra CO2 bands which are in the region m2379.76 to m2352.76"

library(caret)

set.seed(16071962)
source("tools.R")

#package.install("BayesTree")
#library(BayesTree)

# memory.limit()
folder <- "C:/coding/Kaggle/AfricaSoilPropertyPredictionChallenge/"
datafolder <- "C:/coding/Kaggle/AfricaSoilPropertyPredictionChallenge/Data/"
submissionsfolder <- "C:/coding/Kaggle/AfricaSoilPropertyPredictionChallenge/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=T)
  test <- read.csv(paste0(datafolder, "test.csv"), header=T)
  save(train, file=paste0(datafolder, "train.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  load(paste0(datafolder, "train.rda"))
  load(paste0(datafolder, "test.rda"))
}

head(train, n=1)

train$Depth <- with(train, ifelse((Depth == 'Subsoil'), 0 , 1))
test$Depth <- with(test, ifelse((Depth == 'Subsoil'), 0 , 1))

Xtrain <- train[, 2:3595]
Ytrain <- train[, 3596:3600]
Xtest <- test[, 2:3595]
IDtest <- test[, 1]
# Delete highly correlated (>0.95) features.
tooHigh <- findCorrelation(cor(rbind(Xtrain, Xtest)), .95)

Xtrainfiltered <- Xtrain[, -tooHigh]
Xtestfiltered <- Xtest[, -tooHigh]
train <- Xtrainfiltered
test <- Xtestfiltered

result <- CreateTrainAndValidationSets(cbind(Xtrain, Ytrain))
train.subset <- result$train
validation.subset <- result$validation
head(train.subset)

result <- CorrelationPlot(train.subset)

# TODO - NOTE: A location proxy might be to concatenate CTI & ELEV
# NOTE: The whole dataset seems to be PAIRS of measurements! Split/Predict on topsoil versus subsoil??
seq(1, nrow(train), by=2)
seq(2, nrow(train), by=2)

# CO2_bands <- 2656:2670
names(trainingdata)[2656:2670]

# Check for linear dependence (variables that can be expressed as a combination of two or more other variables): 
library(caret)
findLinearCombos(train[, c(-1, -3595)]) # remove id and Depth col

# Check variance:
variance <- numeric()
for (counter in 2:3595) {
  variance <- c(variance, var(train[, counter]))
}
# Or:
variance <- sapply(train[, 2:3595], var)

plot(sort(variance), type="l", col="blue", main="Variance", cex.axis=.8, cex.lab=.8)
limit <- 0.11
cols <- which(variance > limit)
plot(variance[cols], type="o", col="blue", main=paste0("Variance > ", limit), cex.axis=.8, cex.lab=.8,
     xaxt="n", xlab="", ylab="Var", pch=21, bg="cyan", cex.main=1)
axis(1, at=1:length(names(train[cols])), labels=names(train[cols]), las=2, cex.axis=.7)

# Find where the measurements differ most:
plot(train$m6715[1:50], col="blue", type="l")
lines(train$m7494.11[1:50], col="red")
# etc....
# Do cor array to find the same?
cors <- numeric()
for (counter in 2:3594) {
  if (sapply(train[1, counter], class) == "numeric" & sapply(train[1, counter + 1], class) == "numeric")
    cors <- c(cors, cor(train[, counter], train[, counter + 1]))
  else
    cors <- c(cors, NA)
}
cors[1:100]
plot(cors[3578:3594], type="o", col="blue", xaxt="n", xlab="", ylab="Cor", main="Cor")
axis(side=1, at=1:(3594-3578+1), labels=names(train[1,3578:3594]), las=2)
# ----------------------------------------------------------------------------------------------------------------------

y <- c("Ca", "P", "pH", "SOC", "Sand")
y.n <- 5
cols <- list()
skipRows <- list()
cols.Ca <- c(-28,-29,-30,-31,-32)
cols[[1]] <- cols.Ca 
skipRows[[1]] <- which(Ytrain$Ca > 1)
cols.P <- c(-28,-29,-30,-31,-32)
cols[[2]] <- cols.P
skipRows[[2]] <- which(Ytrain$P > 1)
cols.pH <- c(-28,-29,-30,-31,-32)
cols[[3]] <- cols.pH 
skipRows[[3]] <- which(Ytrain$pH > 2.2)
cols.SOC <- c(-28,-29,-30,-31,-32)
cols[[4]] <- cols.SOC 
skipRows[[4]] <- integer(0)
cols.Sand <- c(-3,-14,-19,-26,-28,-29,-30,-31,-32)
cols[[5]] <- cols.Sand
skipRows[[5]] <- integer(0)
train.subset2 <- train.subset[, -tooHigh]
train.subset2 <- train.subset2[, cols.Ca]
names(train.subset2)

fit <- lm(train.subset[, y[y.n]] ~ ., data=train.subset2)
summary(fit)
pred <- predict(fit, newdata=validation.subset, type="response")
#pred[1:100]

rmse <- MyRMSE(validation.subset[, y[y.n]], pred)

op <- par()
par(mfrow=c(1,1))
par(mar=c(2.5,4,2,1))
n <- 50
plot(validation.subset[1:n, y[y.n]], type="o", col="blue", xlab="", ylab=y[y.n], cex.lab=.8, cex.axis=.8, cex.main=.8,
     main=paste0("y/pred (RMSE: ", round(rmse, 3), ")"))
points(pred[1:n], col="red", type="o")
legend("topright", legend=c("y","pred"), col=c("blue","red"), lwd=2, cex=.7)
par <- op

# ---------------------------------------------------------------------------------------------------------------------
# Give it a try...

# START: Section below can not be used with LM!
trainingdata <- train
testdata <- test

MIR_measurements <- trainingdata[, 2:2655]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_train <- cbind(trainingdata[, 3580:3594], MIR_DER[,-1])
MIR_measurements <- trainingdata[, 2671:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_train <- cbind(X_train, MIR_DER[, -1])

MIR_measurements <- testdata[, 2:2655]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_test <- cbind(testdata[, 3580:3595], MIR_DER[,-1])
MIR_measurements <- testdata[, 2671:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_test <- cbind(X_test, MIR_DER[, -1])
# END: Section above can not be used with LM!

# First derivative:
# https://kaggle2.blob.core.windows.net/forum-message-attachments/52888/1497/FirstDerivGraph.R?
#   sv=2012-02-12&se=2014-09-07T04%3A09%3A08Z&sr=b&sp=r&sig=lc1rLk7aO2FaHXSoYkc3sE7j55svLhe2VSJa69%2FKhaU%3D
# http://afsiskaggle.qed.ai/

use.cols <- c(800,900,1000,1500,2000,2500,3000,3580:3594)
y <- c("Ca", "P", "pH", "SOC", "Sand")
df <- data.frame(PIDN=IDtest, Ca=NA, P=NA, pH=NA, SOC=NA, Sand=NA)

for (counter in 1:length(y)) {
  #if (length(skipRows[[counter]]) > 0)
  #  fit <- lm(Ytrain[-skipRows[[counter]], y[counter]] ~ .,
  #            data=train[-skipRows[[counter]], cols[[counter]]])
  #else
  fit <- lm(Ytrain[, y[counter]] ~ ., data=train[, cols[[counter]]])
  summary(fit)
  pred <- predict(fit, test, type="response")
  df[, counter + 1] <- pred
}

head(df)
submission <- df

write.csv(submission, file=paste0(submissionsfolder, "LM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
# This (with Depth nor included) gives best score so far: 0.57855

# ----------------------------------------------------------------------------------------------------------
# TODO: Try GLMNET
# http://blog.revolutionanalytics.com/2013/05/hastie-glmnet.html
# http://www.stanford.edu/~hastie/lectures.htm
# http://www.youtube.com/watch?v=BU2gjoLPfDc#t=38
package.install("glmnet")
library(glmnet)

df <- data.frame(PIDN=IDtest, Ca=NA, P=NA, pH=NA, SOC=NA, Sand=NA)

for (counter in 1:length(y)) {
  fit <- glmnet(x=train[, cols[[counter]]], y=Ytrain[, y[counter]], family="gaussian")
  pred <- predict(fit, test, type="response")
}
#df[, counter+1] <- pred
#head(df)
#submission <- df

# -----------------------------------------------------------------------------------------------------------
# Trying H2O
# http://blog.dominoup.com/using-r-h2o-and-domino-for-a-kaggle-competition/

# Uninstall first:
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# h2o install. Get the zip, unpack the .tar.gz under the R subdir, and do:
# http://docs.0xdata.com/Ruser/Rinstall.html
#install.packages("c:/temp/h2o_2.6.1.5.tar.gz", repos=NULL, type="source")
library(h2o)
#demo(h2o.glm)

#localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, Xmx='2g')
localH2O <- h2o.init()

## Import Data to H2O Cluster
#train_hex <- h2o.importFile(localH2O, "./data/train.zip")  
#test_hex <- h2o.importFile(localH2O, "./data/test.zip")
train_hex <- as.h2o(localH2O, train, key='train')
test_hex <- as.h2o(localH2O, test, key='test')

## Split the dataset into 80:20 for training and validation
train_hex_split <- h2o.splitFrame(train_hex, ratios = 0.8, shuffle = TRUE)

## One Variable at at Time
ls_label <- c("Ca", "P", "pH", "SOC", "Sand")

for (n_label in 1:5) {
  
  ## Display
  cat("\n\nNow training a DNN model for", ls_label[n_label], "...\n")
  
  ## Train a 50-node, three-hidden-layer Deep Neural Networks for 100 epochs
  model <- h2o.deeplearning(x = 2:3595,
                            y = (3595 + n_label),
                            data = train_hex_split[[1]],
                            validation = train_hex_split[[2]],
                            activation = "Rectifier",
                            hidden = c(50, 50, 50),
                            epochs = 100,
                            classification = FALSE,
                            balance_classes = FALSE)
  
  ## Print the Model Summary
  print(model)
  
  ## Use the model for prediction and store the results in submission template
  raw_sub[, (n_label + 1)] <- as.matrix(h2o.predict(model, test_hex))
}

write.csv(raw_sub, file = paste0(submissionsFolder, "Kaggle_H20_Submission.csv"), row.names = FALSE)
