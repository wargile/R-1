# The Analytics Edge summer 2015
# Deadline 03.08.2015
# https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015
# Evaluation metric is AUC (https://inclass.kaggle.com/c/15-071x-the-analytics-edge-summer-2015/details/evaluation)

set.seed(1000)
SetStandardOptions()

# ------------------------------------------------------------------------------------------------------------------
# Function def's:
source("tools.R")
#package.install("h2o")
suppressMessages(library(h2o))
library(ROCR)
library(mice)
library(ggdendro)
library(ape)
library(caret)
library(e1071)
library(stats)
library(randomForest)
library(gbm)
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
library(sqldf)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
# etc.

MyBigrams <- function(text, do.separator=T, separator="_") {
  # word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  word.vec.length <- length(word.vec)
  bigrams <- lapply(1:(word.vec.length - 1), function(x) c(word.vec[x], word.vec[x + 1]))
  #return (bigrams)
  if (do.separator)
    return (lapply(bigrams, function(x) paste0(x[1], separator, x[2])))
  else
    return (lapply(bigrams, function(x) paste(x[1], x[2])))
}

GetNegAndPosScore <- function(data, field) {
  scores <- read.csv(paste0(datafolder, "AFINN-111.txt"), header=F, sep="\t", stringsAsFactors=F)
  head(scores)
  names(scores) <- c("Sentiment", "Score")
  
  data$PhraseScore <- 0
  counter = 0
  wordcounter = 0
  
  for (counter in 1:nrow(data)) {
    #temp <- unlist(strsplit(data[counter, field], " ")) # NOTE: Better(?) regex to split words below does not improve result!
    words <- regmatches(data[counter, field], gregexpr("\\b\\w+'?\\w?\\b", data[counter, field]))[[1]]
    # temp <- regmatches(testCopy$Phrase[counter], gregexpr("\\w+", testCopy$Phrase[counter]))
    # To include hyphens in words (e.g. "uber-cool") use: "\\w+|\\w+\\-\\w+"
    # words <- temp[1:length(temp)]
    score = 0
    
    if (length(words) > 0) {
      for (wordcounter in 1:length(words)) {
        if (words[wordcounter] %in% scores$Sentiment) {
          score <- score + scores$Score[which(scores$Sentiment == words[wordcounter])]
        }
      }
    }
    data$PhraseScore[counter] <- score
  }
  
  # Use our Normalize function to add a new normalized (0-1) score column
  data$PhraseScoreNormalized <- round(Normalize(min(data$PhraseScore),
                                                    max(data$PhraseScore), 0, 1, data$PhraseScore))
  return (data)
}

SplitAndCountWords <- function(text) {
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  return (length(word.vec))
}

GetQuestionMarks <- function(data) {
  question.marks <- sapply(data, function(x) {
    result <- gregexpr("\\?", x)[[1]];
    result <- result[1:length(result)]
    ifelse(sum(result) == - 1, 0, length(result))
  })
  return (as.integer(question.marks)+1)
}

CreateCorpus2 <- function(data, threshold=0.99) {
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
  corpus = tm_map(corpus, removeWords, stopwords("english"))
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

CreateCorpus <- function(data, threshold=0.99, create.bigrams=F) {
  # Create corpus
  corpus <- Corpus(VectorSource(data))
  # Look at corpus
  corpus
  corpus[[1]]
  
  # IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line
  # before continuing (it converts corpus to a Plain Text Document). 
  corpus <- tm_map(corpus, content_transformer(stripWhitespace)) # Eliminating extra white spaces
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
  # Look at stop words 
  # stopwords("english")[1:20]
  # stopwords("norwegian")[1:20]
  # Remove english stopwords (TODO: If removing other words, add with c(...))
  # corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))

  # corpus <- tm_map(reuters, removeNumbers) # Not sure here...
  # Stem document only if we're not creating bigrams
  if (create.bigrams==F) {
    corpus <- tm_map(corpus, content_transformer(stemDocument))
  }
  #corpus[[1]]
  
  # Create DTM
  frequencies <- DocumentTermMatrix(corpus)
  frequencies
  # Look at matrix 
  inspect(frequencies[100:105,505:515]) # Lots of zeroes, a very sparse matrix
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

# TODO: n-grams from tools.R?


# -------------------------------------------------------------------------------------------------------------------------
# http://www.kaggle.com/c/malware-classification/forums/t/13509/brief-description-of-7th-place-solution/72451#post72451
# H. Boström. Estimating class probabilities in random forests. In Proc. of the International Conference on Machine Learning
# and Applications, pages 211-216, 2007.
# http://stats.stackexchange.com/questions/45627/calibration-for-random-forests
CalibrateRF <- function(data, r=.94) {
  result <- apply(data, 1, function(x) {
    #print(x[1] > x[2])
    if (x[1] == x[2])
      return (x)
    a <- x[1]
    b <- x[2]
    a.norm <- a + r * (1 - a)
    #a.norm
    b.norm <- b * (1 - r)
    #b.norm
    #a.norm + b.norm
    if (x[1] > x[2]) {
      x[1] <- a.norm
      x[2] <- b.norm
    } else {
      x[1] <- b.norm
      x[2] <- a.norm
    }
    return (x)
  })
  #rownames(result) <- NULL
  return (t(result))
}

ImputeWithMICE <- function() {
  if (file.exists(paste0(datafolder, "train.rda")) == F) {
    train2 <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
    test2 <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
  }
  
  # TODO...
  train2 <- rbind(train2[,c(-9,-10)], test2[,-9])
  train2$NewsDesk[train2$NewsDesk == ""] <- NA
  train2$SectionName[train2$SectionName == ""] <- "Unknown"
  train2$SubsectionName[train2$SubsectionName == ""] <- "Unknown"
  data2 <- data.frame(NewsDesk=train2$NewsDesk, SectionName=train2$SectionName, SubsectionName=train2$SubsectionName)
  result <- complete(mice(data2))
  return (result)  
}

imputed <- ManualImpute()
imputed <- ImputeWithMICE()
train.imputed <- head(imputed, nrow(train))
test.imputed <- tail(imputed, nrow(test))
dim(train.imputed)
dim(train)
dim(test.imputed)
dim(test)

# ------------------------------------------------------------------------------------------------------------------

datafolder <- "C:/coding/Kaggle/TheAnalyticsEdge_summer2015/Data/"
submissionfolder <- "C:/coding/Kaggle/TheAnalyticsEdge_summer2015/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "eBayiPadTrain.csv"), header=T, stringsAsFactors=T)
  test <- read.csv(paste0(datafolder, "eBayiPadTest.csv"), header=T, stringsAsFactors=T)
  save(train, file=paste0(datafolder, "train.rda"))
  save(test, file=paste0(datafolder, "test.rda"))
} else {
  if (file.exists(paste0(datafolder, "trainCleaned.rda")) == T) {
    load(paste0(datafolder, "trainCleaned.rda"))
    load(paste0(datafolder, "testCleaned.rda"))
  } else {
    load(paste0(datafolder, "train.rda"))
    load(paste0(datafolder, "test.rda"))
  }
}

head(train, n=1)
dim(train)
str(train)
summary(train)
sapply(train, class)
names(train)
CorrelationPlot(train[,c(-1, -11)])

# TODO: Re-level if different levels train/test

# Easy way to create OHE features (library stats):
as.data.frame(model.matrix(~carrier + 0, train))[1:3, ]


# Set biddable and sold as factors, set description as character
train$biddable <- as.factor(train$biddable)
test$biddable <- as.factor(test$biddable)
train$sold <- as.factor(train$sold)
# Round the startprice to reduce number of predictors
train$startprice <- round(train$startprice)
test$startprice <- round(test$startprice)

# Set description as character
train$description <- as.character(train$description)
test$description <- as.character(test$description)

# Add a wordlength col from description length
train$DescriptionWordCount <- as.integer(sapply(train$description, SplitAndCountWords))
test$DescriptionWordCount <- as.integer(sapply(test$description, SplitAndCountWords))
barplot(with(train, tapply(DescriptionWordCount, sold, mean)), col=c("red","green4"), main="Sold by description word count")

# Add a pos/neg score for description col
temp <- GetNegAndPosScore(train, "description")
train$PosNegScoreDescription <- temp$PhraseScore
temp <- GetNegAndPosScore(test, "description")
test$PosNegScoreDescription <- temp$PhraseScore
barplot(with(train, tapply(PosNegScoreDescription, sold, mean)), col=c("red","green4"), main="Sold by pos/neg description score")
par(mfrow=c(2,1))
plot(sort(temp$PhraseScore), col="blue", main="Positive and negative score on Abstract from AFINN")
table(temp$PhraseScore)
barplot(table(temp$PhraseScore), col="cornflowerblue", main="Pos/neg score on Abstract from AFINN")
par(mfrow=c(1,1))

# NOTE: Lots of empty description fields!
par(mfrow=c(1,2))
barplot(table(train$description == ""), col=c("Green4", "Red"), main="Empty train description field")
barplot(table(test$description == ""), col=c("Green4", "Red"), main="Empty test description field")
par(mfrow=c(1,1))

# NOTE: startprice is correlated with sold, and very important predictor!
cor(as.numeric(train$sold), as.numeric(train$startprice))
table(train$sold, train$startprice > 675)
tapply(train$startprice, train$sold, mean)
plot(train$startprice ~ train$sold, col=c("red", "green4"), main="Sold by startprice")
plot(sort(train$startprice), main="startprice", ylab="startprice", col="blue")

# NOTE: Storage is a numeric variable with ordinal qualities. Convert to numeric to preserve ordinality? Any effect on RF? 
# (and convert/impute 'Unknown' to 16, the most common format, or convert to 8 as a dummy value? Impute based on startprice?)
# For impute purposes:
par(mfrow=c(2,1))
with(train, barplot(tapply(startprice, storage, mean), main="Mean startprice by storage category", col="green2"))
with(train, hist(startprice, main="Startprice for storage category 'Unknown'", col="green4"))
par(mfrow=c(1,1))


train$storage <- as.character(train$storage)
test$storage <- as.character(test$storage)
train$storage[train$storage == "Unknown"] <- "8"
test$storage[test$storage == "Unknown"] <- "8"
table(train$storage)
train$storage <- as.integer(train$storage)
test$storage <- as.integer(test$storage)

# Create a new numeric variable on condition (graded from ''not working' to 'new')
train$condition_numeric <- NA
train$condition_numeric[train$condition == "New"] <- 1
train$condition_numeric[train$condition == "Used"] <- 2
train$condition_numeric[train$condition == "New other (see details)"] <- 3
train$condition_numeric[train$condition == "Seller refurbished"] <- 4
train$condition_numeric[train$condition == "Manufacturer refurbished"] <- 5
train$condition_numeric[train$condition == "For parts or not working"] <- 6
table(train$condition_numeric)
table(train$condition)


with(train[train$cellular != "Unknown", ], barplot(table(cellular, carrier), main="Cellular by carrier", col=c("red","green")))
legend("topright", fill=c("red","green"), legend=c("No","Yes"), cex=0.8, bty="n", title="Cellular")
with(train, barplot(table(cellular, carrier), main="Cellular by carrier - training set", col=c("red","green","gray")))
with(test, barplot(table(cellular, carrier), main="Cellular by carrier - test set", col=c("red","green","gray"), beside=T))
legend("topright", fill=c("red","green","gray"), legend=c("No","Yes","Unknown"), cex=0.8, title="Cellular")

par(mar=c(8,3,2,.5))
# TODO: NOTE: condition is ordinal. Convert to integer values (1 = best, n = worst)?
barplot(table(train$condition), las=2, col="orange", main="Condition")
with(train, barplot(tapply(as.numeric(sold)-1, condition, mean), main="Mean sold by condition", las=2, col="green"))
with(train, barplot(tapply(as.numeric(sold)-1, condition, sum), main="Sum sold by condition", las=2, col="green"))
# NOTE: productline is also ordinal (complicated relationship between models here!)
barplot(table(train$productline), las=2, col="green", main="Productline")
with(train, barplot(tapply(as.numeric(sold)-1, productline, mean), main="Mean sold by productline", las=2, col="green"))
with(train, barplot(tapply(as.numeric(sold)-1, productline, sum), main="Sum sold by productline", las=2, col="green"))
with(train, tapply(as.numeric(sold)-1, productline, sum))
with(train, barplot(tapply(as.numeric(sold)-1, color, mean), main="Mean sold by color", las=2, col="green"))
with(train, barplot(tapply(as.numeric(sold)-1, carrier, mean), main="Mean sold by carrier", las=2, col="green"))
# Can return various stats at the same time with tapply too!
with(train, tapply(as.numeric(sold)-1, productline, function(x) c(sum(x), mean(x), sd(x))))
par(mar=c(3,3,2,1))

par(mar=c(9,3,2,1))
hist(round(train$startprice), las=2, main="startprice", col="violetred3")
barplot(table(train$condition), las=2, main="condition", col="violetred3")
barplot(table(train$carrier), las=2, main="carrier", col="violetred3")
barplot(table(train$color), las=2, main="color", col="violetred3")
barplot(table(train$productline), las=2, main="productline", col="violetred3")
barplot(table(train$storage), las=2, main="storage", col="violetred3")
par(mar=c(3,3,2,1))

table(is.na(train$startprice))
table(is.na(train$condition))
table(is.na(train$carrier))
table(is.na(train$color))
table(is.na(train$productline))
table(is.na(train$storage))

# TEST: Create 2-grams from description
n <- 100
two.grams <- MyBigrams(train$description[1:n])
BigramTokenizer <- function(x) (MyBigrams(x)) # create 2-grams
#tdm <- TermDocumentMatrix(VectorSource(train$description[1:n]), control = list(tokenize = BigramTokenizer))
corpus <- tm_map(Corpus(VectorSource(train$description)), content_transformer(BigramTokenizer))
corpus <- tm_map(Corpus(VectorSource(train$description)), content_transformer(MyBigrams))
frequencies <- DocumentTermMatrix(corpus)
frequencies
# Look at matrix 
# inspect(frequencies[100:105,505:515]) # Lots of zeroes, a very sparse matrix
# Check for sparsity
findFreqTerms(frequencies, lowfreq=2) # The minimum number of times a term must appear in the matrix
# Remove sparse terms
threshold <- 0.978
sparse <- removeSparseTerms(frequencies, threshold) # The sparsity level, was: 0.99
# Example 0.98: Only keep the terms that appear in 2 percents or more, of the data rows
sparse
# Convert to a data frame
sparse.df <- as.data.frame(as.matrix(sparse))
head(sparse.df)
image(as.matrix(sparse.df))
par(mar=c(5,2,2,.5))
barplot(sort(colSums(sparse.df), decreasing=T)[1:50], las=2, col="powderblue", main="colSums corpus")
par(mar=c(3,3,2,1))

# ------------------------------------------------------------------------------------------------------------------------------

# Save Feature Engineered datasets for later use
save(train, file=paste0(datafolder, "trainCleaned.rda"))
save(test, file=paste0(datafolder, "testCleaned.rda"))

# -------------------------------------------------------------------------------------------------------------------------------

# Create a baseline model (= the most frequent outcome)
table(train$sold)[1] / nrow(train) # Baseline accuracy (not sold): 0.54

# ------------------------------------------------------------------------------------------------------------------------------

# Create a corpus on train.total$description
CreateTheCorpus <- function(data, threshold=0.987, do.bigrams=F) {
  result <- CreateCorpus(data$description, threshold, do.bigrams)
  #result <- CreateCorpus2(data$description, threshold)
  
  corpus <- result[[1]] # Get the sparse DTM as df 
  #corpus
  dtm <- result[[2]] # Get the sparse DTM
  dtm
  dim(corpus)
  #image(as.matrix(corpus))
  #par(mar=c(8,2,2,.5))
  #barplot(sort(colSums(corpus), decreasing=T)[1:50], las=2, col="powderblue", main="colSums corpus")
  #par(mar=c(3,3,2,1))
  
  # Add columns to corpus:
  corpus$startprice <- data$startprice
  corpus$productline <- data$productline
  corpus$condition <- data$condition
  corpus$storage <- data$storage
  corpus$biddable <- data$biddable
  #corpus$sold <- data$sold
  return (corpus)
}

# Create corpus on train set
corpus <- CreateTheCorpus(train, 0.995, T)
# If we have bigrams, remove the "na_na":
which(names(corpus) == "na_na")
corpus <- corpus[,-which(names(corpus) == "na_na")]
par(mar=c(8,2,2,.5))
barplot(sort(colSums(corpus[1:61]), decreasing=T), las=2, col="powderblue", main="colSums corpus")
par(mar=c(3,2,2,.5))
corpus$sold <- train$sold
# Create corpus on total set
train.rows <- nrow(train)
test.rows <- nrow(test)
corpus <- CreateTheCorpus(c(train, test), 0.995, T)
# If we have bigrams, remove the "na_na":
which(names(corpus) == "na_na")
corpus <- corpus[,-which(names(corpus) == "na_na")]
corpus.train <- head(corpus, train.rows)
corpus.test <- tail(corpus, test.rows)
nrow(train)
nrow(corpus.train)
nrow(test)
nrow(corpus.test)
corpus.train$sold <- train$sold

# ------------------------------------------------------------------------------------------------------------------------------

# Do a wordcloud on the corpus
wordCloud <- wordcloud(colnames(corpus), colSums(corpus), scale=c(4, 0.85), colors=brewer.pal(9, "Blues"))

# ------------------------------------------------------------------------------------------------------------------------------

# Create train and validation sets
result <- CreateTrainAndValidationSets(train)
train.subset <- result[[1]]
validation.subset <- result[[2]]
# or do:
set.seed(1000)
split <- sample.split(train$sold, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)
train.subset <- subset(corpus, split==TRUE)
validation.subset <- subset(corpus, split==FALSE)
# TIP: Train 2 models on biddable = True versus False?

# ---------------------------------------------------------------------------------------------------------------------------

# TODO: REVISE!
# Set up caret and train control to do repeated CV
# Cross-validation:
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
# http://cran.r-project.org/web/packages/randomForest/randomForest.pdf

# K-fold cross validation: Split the training set into <k> pieces. Then predict fold <1:k>, one at a time,
# with the rest of the folds. Last, average the accuracy over all the folds to determine which parameter values
# we want to use for the final model. For tree, it is called "cp" (Complexity Paramater), works like UC and adj.R^2.
# Measures trade-off between model complexity and accuracy on training set.
# A smaller cp value leads to a bigger tree, so might overfit.
getModelInfo("rf")
numFolds <- trainControl(method="cv", number=10, classProbs=T)
#cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
corpus.train2 <- corpus.train
corpus.train2$sold <- as.factor(ifelse(corpus.train2$sold == 0, "No", "Yes")) # Convert so caret doesen't bork on the name
train.caret <- train(Popular ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+
                       WordCountHeadline+WordCountAbstract+Weekday+Hour,
                     data=corpus.train2, method="rf", trControl=numFolds, metric="Accuracy")
summary(train.caret)
predict.caret <- predict(train.caret, newdata=corpus.validation, type="prob")
prediction.result.caret <- table(corpus.validation$sold, predict.caret[,2] > 0.5)
prediction.result.caret
sum(diag(prediction.result.caret)) / sum(prediction.result.caret)

# ---------------------------------------------------------------------------------------------------------------------------

# Try a simple glm
model.glm <- glm(sold ~ ., data=train.subset[, c(2:3,5:10,14)], family=binomial(link="logit")) # Condition numeric
model.glm <- glm(sold ~ ., data=train.subset[, c(2:10)], family=binomial(link="logit"))
model.glm <- glm(sold ~ ., data=train.subset[, c(2:4,7:10)], family=binomial(link="logit"))
summary(model.glm)
pred.glm <- plogis(predict(model.glm, newdata=validation.subset))
result <- table(round(pred.glm), as.numeric(validation.subset$sold)-1 > 0.5)
result
sum(diag(result)) / nrow(validation.subset)
PlotROC(pred.glm, validation.subset$sold, "ROCR plot of glm model prediction")


# Script from: 
# https://www.kaggle.com/benhamner/liberty-mutual-group-property-inspection-prediction/random-forest-benchmark

# Create the response variable
y <- train.subset$sold

# Create the predictor data set and encode categorical variables using caret library.
mtrain <- train.subset[, 1:9]
mtest <- validation.subset[, 1:9]
dummies <- dummyVars(~ ., data=mtrain)
mtrain <- predict(dummies, newdata=mtrain)
mtest <- predict(dummies, newdata=mtest)

cat("Training model - RF\n")
set.seed(8)
rf <- randomForest(mtrain, y, ntree=150, imp=TRUE, do.trace=TRUE)
predict_rf <- predict(rf, mtest, type="prob")[,2]

# https://github.com/dmlc/xgboost/blob/master/R-package/demo/basic_walkthrough.R
# Set necessary parameters and use parallel threads
param <- list("objective"="reg:linear", "nthread"=8, "verbose"=0)

cat("Training model - Xgboost\n")
# Fit the model
xgb.fit <- xgboost(param=param, data=mtrain, label=y, nrounds=1700, eta=.01, max_depth=7, 
                   min_child_weight=5, scale_pos_weight=1.0, subsample=0.8) 
predict_xgboost <- predict(xgb.fit, mtest)
plot(sort(plogis(predict_xgboost)))

# Predict sold for the test set
submission <- data.frame(Id=validation.subset$UniqueID)
submission$prediction1 <- (predict_rf + plogis(predict_xgboost)) / 2
head(submission)

# ---------------------------------------------------------------------------------------------------------------------------

# TODO: REVISE! Do clustering on corpus
# http://beyondvalence.blogspot.no/2014/01/text-mining-5-hierarchical-clustering.html

# From: http://michael.hahsler.net/SMU/CSE7337/install/tm.R
dtm_tfxidf <- weightTfIdf(dtm)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
cl <- kmeans(m_norm, 10) # TODO: Error in do_one(nmeth) : NA/NaN/Inf in foreign function call (arg 1)
cl
table(cl$cluster)

head(corpus.train, n=1)
distances <- dist(corpus.train, method="euclidean") # Takes a long time sometimes, so save for later use
save(distances, file=paste0(datafolder, "distances.corpus.train.rda"))
# load(file=paste0(folder, "distances.rda"))

cluster.description <- hclust(distances, method="ward.D")
# plot basic tree
plot(as.phylo(cluster.description), cex=0.7, label.offset=1)
hcd <- as.dendrogram(cluster.description)
summary(cluster.description)
# plot(hcd)
cluster.description.cuts <- cutree(cluster.description, k=2)
summary(cluster.description.cuts)
table(cluster.description.cuts)
table(train$sold)

set.seed(1000)

km.clusters <- kmeans(distances, centers=2)
km.clusters <- kmeans(corpus.train, centers=2, iter.max=10, nstart=4)
# Important, use data frame here if only numeric cols!
str(km.clusters)
#clusterGroups.km <- cutree(km.clusters, k=7) # Cut the tree into 7 groups of data (just for hclust)
clusterList.km <- lapply(1:2, function(x) subset(corpus.train, km.clusters$cluster == x))
# save(clusterList.km, paste0(folder, "clusterList.km.rda"))
# load(paste0(folder, "clusterList.km.rda"))
KmeansClusters <- split(corpus.train[, 1:(ncol(corpus.train)-1)], km.clusters$cluster)
sapply(clusterList.km, nrow)
km.clusters$size
# or just use:
table(km.clusters$cluster)
table(corpus.train$sold)
# NOTE: This...
sum(sapply(clusterList.km, nrow))
# ...should be equal to:
nrow(corpus.train)
table(clusterList.km[[1]]$sold)
table(clusterList.km[[2]]$sold)

# Do CART model
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(sold ~ ., data=train.subset[, 1:10],
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.01
corpusCART <- rpart(sold ~ ., data=train.subset[, 1:10], method="class", cp=cp.value)
prp(corpusCART, cex=.8, col="blue", main="Corpus")
plot(corpusCART)
text(corpusCART, cex=.7, col="blue")
# Evaluate the performance of the model
predictCART <- predict(corpusCART, newdata=validation.subset, type="class")
result <- table(validation.subset$sold, predictCART)
result
# Compute accuracy for CART
sum(diag(result)) / sum(result)


# -------------------------------------------------------------------------------------------------------------------------------

# Try h2o

#localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
dat_h2o <- as.h2o(localH2O, train.subset, destination_frame='train')
dat_h2o.test <- as.h2o(localH2O, validation.subset, destination_frame='test')
dat_h2o <- as.h2o(localH2O, corpus.train, destination_frame='train')
dat_h2o.test <- as.h2o(localH2O, corpus.test, destination_frame='test')
dat_h2o <- as.h2o(localH2O, train, destination_frame='train')
dat_h2o.test <- as.h2o(localH2O, test, destination_frame='test')
x.cols <- c(2:9)
y.col <- 10
x.cols <- c(2:9,12,13) # Does not improve score...
y.col <- 10
x.cols <- c(1:66) # corpus
y.col <- 67

# TODO: Try binomial GLM/ridge/elasticnet/lasso with h2o:
# https://leanpub.com/glm/read
# http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
# http://learn.h2o.ai/content/hands-on_training/regression.html
model.glm <- h2o.glm(x=x.cols, y=y.col, family="binomial", link="logit", training_frame=dat_h2o,
                     lambda_search=T, max_iterations=50, nlambda=10, alpha=c(0, 0.25, 0.5, 0.75, 1))
model.glm

# Do gbm
model.gbm <- h2o.gbm(x=x.cols, y=y.col, distribution = "bernoulli", training_frame=dat_h2o, ntrees=500)
model.gbm

# Do rf
model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, training_frame=dat_h2o, model_id="rf", ntree=500,
                   mtries= -1, nbins=20, seed= -1, balance_classes=F, max_depth=20)
model.rf

h2o_yhat_test.glm <- h2o.predict(model.glm, dat_h2o.test)
df_h2o_yhat_test.glm <- as.data.frame(h2o_yhat_test.glm)
head(df_h2o_yhat_test.glm)

h2o_yhat_test.gbm <- h2o.predict(model.gbm, dat_h2o.test)
df_h2o_yhat_test.gbm <- as.data.frame(h2o_yhat_test.gbm)
head(df_h2o_yhat_test.gbm)

h2o_yhat_test.rf <- h2o.predict(model.rf, dat_h2o.test)
df_h2o_yhat_test.rf <- as.data.frame(h2o_yhat_test.rf) 
head(df_h2o_yhat_test.rf)

plot(sort(1-df_h2o_yhat_test.glm[,2]), col="blue")
plot(sort(1-df_h2o_yhat_test.gbm[,2]), col="blue")
plot(sort(1-df_h2o_yhat_test.rf[,2]), col="blue")
plot(sort(1-((df_h2o_yhat_test.rf[,2]+df_h2o_yhat_test.glm[,2])/2)), col="blue")
min(df_h2o_yhat_test.rf$predict)
max(df_h2o_yhat_test.rf$predict)
min(df_h2o_yhat_test.gbm$predict)
max(df_h2o_yhat_test.gbm$predict)
min(df_h2o_yhat_test.glm$predict)
max(df_h2o_yhat_test.glm$predict)

prediction.result.gbm <- table(validation.subset$sold, (1-df_h2o_yhat_test.gbm[,2]) > 0.5)
prediction.result.gbm
sum(diag(prediction.result.gbm)) / sum(prediction.result.gbm)

prediction.result.glm <- table(validation.subset$sold, (1-df_h2o_yhat_test.glm[,2]) > 0.5)
prediction.result.glm
sum(diag(prediction.result.glm)) / sum(prediction.result.glm)

prediction.result.rf <- table(validation.subset$sold, df_h2o_yhat_test.rf$predict > 0.5)
#prediction.result.rf <- table(corpus.validation$sold, 1-df_h2o_yhat_test.rf[,2] > 0.5)
prediction.result.rf
sum(diag(prediction.result.rf)) / sum(prediction.result.rf)

PlotROC(1 - df_h2o_yhat_test.gbm[,2], validation.subset$sold, "ROCR plot of gbm model prediction")
PlotROC(1 - df_h2o_yhat_test.glm[,2], validation.subset$sold, "ROCR plot of glm model prediction")
PlotROC(1 - df_h2o_yhat_test.rf[,2], validation.subset$sold, "ROCR plot of rf model prediction")
# Combined
PlotROC(1 - ((df_h2o_yhat_test.rf[,2] + df_h2o_yhat_test.glm[,2]) / 2), validation.subset$sold,
        "ROCR plot of rf+glm model prediction")

# -----------------------------------------------------------------------------------------------------------------

# Create submission file
prediction <- df_h2o_yhat_test.rf$p1
prediction <- df_h2o_yhat_test.gbm$p1
prediction <- (df_h2o_yhat_test.rf$p1 + df_h2o_yhat_test.glm$p1) / 2
par(mfrow=c(2,2))
hist(prediction, col="wheat")
plot((prediction), col="blue", pch=21, bg="cyan", main="Prediction result")
plot(sort(prediction), type="o", col="blue", main="Sorted prediction curve")
par(mfrow=c(1,1))

# Create the submission file
options("scipen"=100, "digits"=8)
MySubmission <- data.frame(UniqueID=test$UniqueID, Probability1=prediction)
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "h2o_GLM")
# Score: 0.84204 with h2o RF, ntree=500, predictor cols = 2:9
# NOTE: No bag-of-words used for best score!
