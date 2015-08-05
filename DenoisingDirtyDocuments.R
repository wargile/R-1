# Denoising Dirty Documents
# https://www.kaggle.com/c/ ...
# Deadline: ... 

# The evaluation metric for this competition is...

# TIPS/CODE:

# TODO:

set.seed(1000)

source("tools.R")
library(caret)
library(randomForest)
library(gbm)
package.install("readr")
library(readr)
library(caTools)
library(wordcloud)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(Metrics)
library(SnowballC)
library(tm)
library(png)
# etc.
SetStandardOptions()

# -------------------------------------------------------------------------------------------------------------------------------

trainfolder <- "C:/coding/Kaggle/DenoisingDirtyDocuments/data/train/"
testfolder <- "C:/coding/Kaggle/DenoisingDirtyDocuments/data/test/"
traincleanedfolder <- "C:/coding/Kaggle/DenoisingDirtyDocuments/data/train_cleaned/"
submissionfolder <- "C:/coding/Kaggle/DenoisingDirtyDocuments/submissions/"

# TODO: Read the files from the train, test and train_cleaned folders

GetImages <- function(folder) {
  images = list.files(folder) # Get a list of all subfolders in folder
  imagelist <- list()
  for (counter in 1:length(images)) {
    cat(paste0("Getting image ", images[counter], "...\n"))
    train <- # TODO: Load the PNG file
    imagelist[[counter]] <- images[counter]
  }
  return(imagelist)
}

xpixels <- 540
ypixels <- 288

imagelist <- GetImages(trainfolder)
png <- readPNG(paste0(trainfolder, imagelist[[1]]))
z <- matrix(data=as.numeric(png), nrow=xpixels, ncol=ypixels, byrow=F)
z <- matrix(data=as.numeric(png))
table(z > 0.5) # Get/set cutoff for S/H treatment of pixels
image(t(z)[,nrow(z):1], col=pal.1(xpixels*ypixels))
image(t(z)[,nrow(z):1])

# --------------------------------------------------------------------------------------------------------------------------------
# Create the submission file
# options("scipen"=100, "digits"=8)
# MySubmission <- data.frame(id=validation.subset$id, prediction=predict.rf)
MySubmission <- data.frame(id=test$id, prediction=test$similarity) # TODO...
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "TODO")
# RF with cosine distance score: 0.04443. Not too great....!
