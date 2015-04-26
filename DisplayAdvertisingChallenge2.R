# Kaggle Display Advertising Challenge
# Deadline: Tue 23 Sep 2014

# TIPS:
# Implement "feature trimming", which consists of:
# 1) introducing a random vector into the feature set,
# 2) calculating feature importance,
# 3) removing the features with importance below the "dummy feature".
# 4) http://www.kaggle.com/c/criteo-display-ad-challenge/forums/t/10429/congratulations-to-the-winners

library(data.table)
require(bit64)

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/data/"
codeFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/code/"
submissionsFolder <- "C:/coding/Kaggle/DisplayAdvertisingChallenge/submissions/"

#train <- fread(paste0(dataFolder, "train_fixed.csv"), header=T, sep=",", stringsAsFactors=F)
train <- read.csv(paste0(dataFolder, "train_fixed.csv"), header=T, sep=",", stringsAsFactors=F)
#test <- fread(paste0(dataFolder, "test_fixed.csv"), header=T, sep=",", stringsAsFactors=F)
test <- read.csv(paste0(dataFolder, "test_fixed.csv"), header=T, sep=",", stringsAsFactors=F)
#train <- data.frame(train)
#test <- data.frame(test)
dim(test)
dim(train)
head(train, n=1)
head(test, n=1)
# Save the test$Id col for use in submission
test.id <- test$Id

library(LiblineaR)

# NOTE: Classification models usually perform better if each dimension of the data is first centered and scaled.
# http://rtutorialseries.blogspot.no/2012/03/r-tutorial-series-centering-variables.html

# http://www.inside-r.org/packages/cran/LiblineaR/docs/predict.LiblineaR

# I6.new   I6.new 24.1804921542352389
# I11.new I11.new 23.2946413219162878
# I7.new   I7.new 10.9106510976025000
# I1.new   I1.new 10.2585052995323949
# I13.new I13.new  7.5394481466735419
# C14.new C14.new  6.7166140044551756
# I3.new   I3.new  3.1953816918927154
# I5.new   I5.new  2.4564897557081378
# C17.new C17.new  2.0054414663608888
# C23.new C23.new  1.5846872792081941
# I4.new   I4.new  1.3778081178513615
# I12.new I12.new  1.1553783007286740
# C26.new C26.new  1.0455633096022989
# C20.new C20.new  0.7137528784301128
# C25.new C25.new  0.6355941328104230
# I9.new   I9.new  0.6156710232074881
# I8.new   I8.new  0.5563883632714262

# Normalize values between min and max
Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  # Find the normalized (0-1 range) value of curval
  normalized <- (curval - minval) / (maxval - minval)
  normalized
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

scale.it <- function(data, start.col) {
  for (counter in start.col:ncol(data)) {
    #data[, counter] <- scale(data[, counter], center=T, scale=T)
    data[, counter] <- Normalize(min(data[, counter]), max(data[, counter]), 0, 1, data[, counter])
  }
  return (data)
}
train.scaled <- scale.it(train, 3)
test.scaled <- scale.it(test, 2)

model <- LiblineaR(train.scaled[, c(-1,-2)], train.scaled$Label, type=0, cost=1, epsilon=0.01, #type=0: L2-regularized
                   bias=T, wi=NULL, cross=0, verbose=F)
model <- LiblineaR(train.scaled[, c("I6", "I11", "I7", "I1", "I13", "C14", "I3", "I5", "C17", "C23", "I4", "I12",
                             "C26", "C20", "C25", "I9", "I8")],
                   train.scaled$Label, type=0, cost=1, epsilon=0.01, #type=0: L2-regularized
                   bias=T, wi=NULL, cross=0, verbose=F)

summary(model)
prediction <- predict(model, test.scaled, proba=T, decisionValues=T)
#prediction$predictions[1:100]
#prediction$label[1:100] ## ??
prediction$probabilities[,2][1:100]
min(prediction$probabilities[,2])
max(prediction$probabilities[,2])

options(digits=15)
submission=data.frame(Id=test.id, Predicted=prediction)
head(submission)
tail(submission)
dim(submission)
write.csv(submission, file=paste0(submissionsFolder, "LiblineaR_fixed_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
