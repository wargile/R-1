# Text analysis / NLP
# -------------------

# Text analytics for a machine is hard because humans use sarcasm, homonyms, metaphors.
# Can use Amazon Mechanical Turk (https://www.mturk.com/mturk/welcome)
# Posted on AMT: 'Judge the sentiment expressed by the following item toward the software company "Apple"'
# Five workers labeled each tweet: Strongly Negative (-2), Negative (-1), Neutral (0), Positive (+1), Strongly Positive (+2)
# The large majority was classified as neutral.

# Bag of words:
# -------------
# Simple approach: It counts the number of times each word appear in a text.
# These counts are then used as independent variables.
# BUT: Should taylor approach to specific problem: sometimes it is useful to lowercase all words, remove all punctuation, etc.
# But sometimes, we want to keep extra charachers, etc.: @Apple is a tag in a tweet/message TO Apple. #Apple is a hashtag in a
# tweet ABOUT Apple, but not directly addressed to them, like the former example. Or for instance, keep /. if we want to detect URL's.
# Remove stop words (words just meaningful in a whole sentence. BUT: "The Who" are two stopwords together and the name of a band...
# Stemming: All variants of a word could be represented by its common stem. (http://en.wikipedia.org/wiki/Stemming)
# Porter Stemmer (Martin Porter 1980) is still used today in Machine Learning
# (http://snowball.tartarus.org/algorithms/porter/stemmer.html, http://tartarus.org/martin/PorterStemmer/)

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(wordcloud)
library(ROCR)

set.seed(1000)

SetStandardOptions()

# ---------------------------------------------------------------------------------------------------------------------------------------

GetAUC <- function(model, dataset, depvar) {
  return (as.numeric(performance(prediction(predict(model, type="prob", newdata=dataset)[,2], depvar), "auc")@y.values))
}

MyBigrams <- function(text, split.char=".") {
  # word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  word.vec.length <- length(word.vec)
  bigrams <- lapply(1:(word.vec.length - 1), function(x) c(word.vec[x], word.vec[x + 1]))
  #return (bigrams)
  #return (lapply(bigrams, function(x) paste(x[1], x[2])))
  return (unlist(lapply(bigrams, function(x) paste0(x[1], split.char, x[2]))))
}

CreateCorpus <- function(data, threshold=0.98, create.bigrams=F, stem.document=F) {
  # Create corpus
  corpus <- Corpus(VectorSource(data))
  # Look at corpus
  corpus
  corpus[[1]]
  
  corpus <- tm_map(corpus, content_transformer(stripWhitespace)) # Eliminating extra whitespaces
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  # Convert to lower-case
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
  
  # TEST:
  if (create.bigrams == T) {
    corpus <- tm_map(corpus, content_transformer(MyBigrams))
  }
  #corpus <- tm_map(corpus, PlainTextDocument)
  corpus[[1]]
  # corpus <- tm_map(reuters, removeNumbers) # Not sure here...
  # Stem document 
  if (stem.document)
    corpus <- tm_map(corpus, content_transformer(stemDocument))
  #corpus[[1]]
  
  # Create DTM
  frequencies <- DocumentTermMatrix(corpus)
  frequencies
  # Look at matrix 
  # inspect(frequencies[100:105,505:515]) # Lots of zeroes, a very sparse matrix
  # Check for sparsity
  findFreqTerms(frequencies, lowfreq=2) # The minimum number of times a term must appear in the matrix
  # Remove sparse terms
  sparse <- removeSparseTerms(frequencies, threshold) # The sparsity level, was: 0.99
  # Example 0.98: Only keep the terms that appear in 2 percents or more, of the data rows
  sparse
  # Convert to a data frame
  sparse.df <- as.data.frame(as.matrix(sparse))
  head(sparse.df)
  # Make all variable names R-friendly
  colnames(sparse.df) <- make.names(colnames(sparse.df))
  
  # TODO: Create TDM too?
  
  return(list(sparse.df, sparse)) # Return a list with 1) the sparse matrix df and 2) the dtm
}

# ---------------------------------------------------------------------------------------------------------------------------------------

folder <- "c:/coding/R/testdata/"
# Read in the data
tweets <- read.csv(paste0(folder, "tweets.csv"), stringsAsFactors=FALSE)
str(tweets)
# Create dependent variable
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
# Create corpus
corpus <- Corpus(VectorSource(tweets$Tweet))
# Look at corpus
corpus
corpus[[1]]
# Convert to lower-case
corpus <- tm_map(corpus, tolower)
corpus[[1]]
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred
# after this video was recorded.
corpus <- tm_map(corpus, PlainTextDocument)
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]
# Look at stop words 
stopwords("english")[1:20]
stopwords("norwegian")[1:20]
# Remove english stopwords AND the 'apple' word (only in the context as a stopword!)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]] # As we can see, 'apple' is still included, but not as a stopword
# Stem document 
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]

# Create matrix
frequencies <- DocumentTermMatrix(corpus)
frequencies
# Look at matrix 
inspect(frequencies[1000:1005,505:515]) # Lots of zeroes, a very sparse matrix
# image(as.matrix(inspect(frequencies)))
# Check for sparsity
findFreqTerms(frequencies, lowfreq=20) # The minimum number of times a term must appear in the matrix
# Remove sparse terms
sparse <- removeSparseTerms(frequencies, 0.995) # The sparsity level
# Example 0.98: Only keep the terms that appear in 2 percents or more, of the tweets
# In this case, given the number of tweets, 0.995 is 6 or more of the tweets ((1181 tweets * 0.995) / 1881 tweets)
sparse # Just 309 different terms are left now, a much better starting point
# Convert to a data frame
tweetsSparse <- as.data.frame(as.matrix(sparse))
head(tweetsSparse)
# Make all variable names R-friendly
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
# Add dependent variable
tweetsSparse$Negative <- tweets$Negative
head(tweetsSparse, n=1)
# Split the data
set.seed(1000)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)

# Build a CART model
tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART, cex=.8, col="blue", main="Tweets")
plot(tweetCART)
text(tweetCART, cex=.7, col="blue")
# Evaluate the performance of the model
predictCART <- predict(tweetCART, newdata=testSparse, type="class")
result <- table(testSparse$Negative, predictCART)
result
# Compute accuracy for CART
sum(diag(result)) / sum(result)
# Baseline accuracy on the test set (= the most frequent value in the test set)
table(testSparse$Negative)[1] / nrow(testSparse)

# For CART above: Caret train to find optimal cp parameter
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Negative ~ ., data=trainSparse,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.01
corpusCART <- rpart(Negative ~ ., data=trainSparse, method="class", cp=cp.value)
predictCART <- predict(corpusCART, newdata=testSparse, type="class")
result <- table(testSparse$Negative, predictCART)
result
# Compute accuracy for CART
sum(diag(result)) / sum(result)

# Random forest model
set.seed(1000)
tweetRF <- randomForest(Negative ~ ., data=trainSparse)
varImpPlot(tweetRF, cex=.7, col="blue", pch=16)
# Make predictions:
predictRF <- predict(tweetRF, newdata=testSparse)
result <- table(testSparse$Negative, predictRF)
result
# Compute accuracy for RF
sum(diag(result)) / sum(result)
# Small improvement over CART model. The CART model could therefore still be preferable because of its interpretability!
# By tuning the cp parameter through cross-validation in the CART model, we could probably improve the CART model too.

# Create a wordcloud from the corpus

# Try the corpus function with bigrams
result <- CreateCorpus(data=tweets$Tweet, threshold=0.995, F)
tweetsCorpus <- result[[1]]
#tweetsCorpus
dtm <- result[[2]]
dtm
tweetsCorpus$Negative <- tweets$Negative
head(tweetsCorpus, n=1)
dim(tweetsCorpus)
# Split the data
set.seed(1000)
split <- sample.split(tweetsCorpus$Negative, SplitRatio = 0.7)
trainCorpus <- subset(tweetsCorpus, split==TRUE)
testCorpus <- subset(tweetsCorpus, split==FALSE)

par(mar=c(9,3,2,1))
barplot(sort(colSums(trainCorpus[,-ncol(trainCorpus)])), las=2, cex.names=.6, cex.axis=.7, col="darkcyan")
par(mar=c(3,3,2,1))

numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Negative ~ ., data=trainCorpus,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.01
corpusCART2 <- rpart(Negative ~ ., data=trainCorpus, method="class", cp=cp.value)
prp(corpusCART2, cex=.8, col="blue", main="Tweets with bigrams")
predictCART2 <- predict(corpusCART2, newdata=testCorpus, type="class")
result <- table(testCorpus$Negative, predictCART2)
result
# Compute accuracy for CART
sum(diag(result)) / sum(result)

# Show ROC result
GetAUC(corpusCART2, testCorpus, testCorpus$Negative)

# Show ROCR colorized plot
par(mar=c(3,3,2,2))
predictCART3 <- predict(corpusCART2, newdata=testCorpus, type="prob")
predROCR = prediction(predictCART3[,2], testCorpus$Negative)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on tweets", lwd=3)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
par(mar=c(3,3,2,1))
# Compute AUC
performance(predROCR, "auc")@y.values
sn <- slotNames(predROCR)
sapply(sn, function(x) length(slot(predROCR, x)))
