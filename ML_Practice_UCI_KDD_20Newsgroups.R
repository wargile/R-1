# UCI KDD 20 Newsgroups, try clustering and text mining/classification!
# ---------------------------------------------------------------------

# Main page: http://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.html
# Data description: http://kdd.ics.uci.edu/databases/20newsgroups/20newsgroups.data.html

# References: http://reports-archive.adm.cs.cmu.edu/anon/1996/CMU-CS-96-118.ps

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(ggmap)
library(mapproj)
library(ggvis)
library(dplyr)
library(tm)

# Set locale to US, to ensure that there aren't formatting issues in assignments/inputs:
Sys.setlocale("LC_ALL", "C")

set.seed(1000)
SetStandardOptions()

GetNewsgroupData <- function() {
  dataFolder <- "c:/coding/R/TestData/20_NewsGroups/"
  newsgroups <- list.files(dataFolder) # Get a list of all folders (= newsgroups)
  newsgroups.df <- data.frame()
  
  for (folder.name in newsgroups) {
    cat("Processing", folder.name, "...\n")
    newsgroupFolder <- paste0(dataFolder, folder.name)
    articles <- list.files(newsgroupFolder) # Get a list of all articles in newsgroup folders
    for (article in articles) {
      f <- file(paste0(newsgroupFolder, "/", article), "rb")
      data <- readChar(f, 320000)
      close(f)
      newsgroups.df <- rbind(newsgroups.df, data.frame(newsgroup=folder.name, article=data))
    }
  }
  
  return(newsgroups.df)
}

CreateCorpus2 <- function(data, threshold=0.99, extra.stopwords="") {
  # Create corpus
  corpus = Corpus(VectorSource(data))
  # corpus[[1]]
  # Pre-process data
  corpus = tm_map(corpus, tolower)
  # IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
  # (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after
  # this video was recorded.)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, c(extra.stopwords, stopwords("english")))
  corpus = tm_map(corpus, stemDocument)
  # Create matrix
  dtm = DocumentTermMatrix(corpus)
  # dtm
  # str(dtm)
  # Remove sparse terms
  dtm = removeSparseTerms(dtm, threshold)
  # dtm
  # Create data frame
  labeledTerms = as.data.frame(as.matrix(dtm))
  colnames(labeledTerms) <- make.names(colnames(labeledTerms))
  return(list(labeledTerms, dtm)) # Return a list with 1) the sparse matrix df and 2) the dtm
}

# -----------------------------------------------------------------------------------------------------------------------------------

if (file.exists("c:/coding/R/TestData/20_Newsgroups.rda"))
  load(file="c:/coding/R/TestData/20_Newsgroups.rda")
else {
  newsgroups.df <- GetNewsgroupData()
  dim(newsgroups.df)
  save(newsgroups.df, file="c:/coding/R/TestData/20_Newsgroups.rda")
}

# TODO: Remove stopwords + the names of the newsgroups in the articles (otherwise there's not much point in predicting!)
dataFolder <- "c:/coding/R/TestData/20_NewsGroups/"
stopword
s <- list.files(dataFolder) # Get a list of all folders (= newsgroups)
stopwords
result <- CreateCorpus2(data=newsgroups.df$article, threshold=0.92, extra.stopwords=stopwords)
corpus <- result[[1]]
dtm <- result[[2]]
dtm # 163 terms, this is OK

corpus$newsgroup <- newsgroups.df$newsgroup
rownames(corpus) <- NULL

library(caTools)
set.seed(1000)
split <- sample.split(corpus$newsgroup, SplitRatio=0.7)
train.subset <- subset(corpus, split==TRUE)
validation.subset <- subset(corpus, split==FALSE)

# Try a RF
fit <- randomForest(newsgroup ~ ., data=train.subset, ntrees=100)
varImpPlot(fit, col="blue", cex=.7, pch=16)
pred <- predict(fit, validation.subset)
table(pred)
result <- table(validation.subset$newsgroup, pred)
result
ConfusionMatrix(result, labels=unique(validation.subset$newsgroup))
sum(diag(result)) / sum(result)
