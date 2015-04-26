# Kaggle How Much Did It Rain
# Ends: 15 May 2015
# http://www.kaggle.com/c/how-much-did-it-rain
# http://www.kaggle.com/c/how-much-did-it-rain/data
# Understanding Polarimetric radar measurements:
# https://www.kaggle.com/c/how-much-did-it-rain/details/understanding-polarimetric-radar-measurements

set.seed(16071962)
source("tools.R")

competition <- "HowMuchDidItRain"
datafolder <- paste0("C:/coding/Kaggle/", competition, "/Data/")
submissionsfolder <- paste0("C:/coding/Kaggle/", competition, "/Submissions/")

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- fread(paste0(datafolder, "train_2013.csv"), header=T, sep=",")
  test <- fread(paste0(datafolder, "test_2014.csv"), header=T, sep=",")
  
  train <- as.data.frame(train)
  test <- as.data.frame(test)
  test.ids <- test$Id
  
  save(train, file=paste0(datafolder, "train_2013.rda"))
  save(test, file=paste0(datafolder, "test_2014.rda"))
  save(test.ids, file=paste0(datafolder, "testIds.rda"))
} else {
  load(paste0(datafolder, "train_2013.rda"))
  load(paste0(datafolder, "test_2014.rda"))
  load(paste0(datafolder, "testIds.rda"))
}

head(train, n=1)
dim(train)
summary(train)
sapply(train, class)
numeric.cols <- which(sapply(train, class) %in% c("numeric","integer"))

#plot(colSums(trainLabels[,c(-1)]))
dim(test)
head(test, n=1)
sapply(test, class)

# TODO: Since there are several values (space-separated) in each column, do we need to split this and take the mean/median
#       and create new columns, and then train our model on those values?

# Create training and validation subsets from train set
result <- CreateTrainAndValidationSets(train)
train.subset <- result$train
validation.subset <- result$validation

# Create the submission file
#KaggleSubmission(submission, submissionsfolder, "GLM")
