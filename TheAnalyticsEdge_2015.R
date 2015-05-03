# The Analytics Edge 2015
# http://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/data
# Deadline: 04.05.2015

# The evaluation metric for this competition is AUC.

# Submission format:
# For every observation in the test set, submission files should contain two columns: UniqueID and Probability1

# Outcome/dependent variable: Popular, which labels if an article had 25 or more comments in its online comment section
#   (equal to 1 if it did, and 0 if it did not)
# Independent variables:
# NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
# SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
# SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
# Headline = the title of the article
# Snippet = a small portion of the article text
# Abstract = a summary of the blog article, written by the New York Times
# WordCount = the number of words in the article
# PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
# UniqueID = a unique identifier for each article

# Things to try:
# - Cluster-then-predict on all text predictors, any effect here?
# - Do local repeated CV? trainControl(method="cv", number=10). find best mtry for RF woth caret train?
# - n-grams?
# - Ensembling: http://www.r-bloggers.com/an-intro-to-ensemble-learning-in-r/
# - Split bag-of-words and other predictors in separate models and combine/average
# - Get some functions and ideas from SentimentAnalysisOfMovieReviews.R (and try proper bag-of-words on that one too!)
# - Use the "cut" function to create bins for the count variables - any improvement over using them as-is?
set.seed(1000)
SetStandardOptions()

# ------------------------------------------------------------------------------------------------------------------
# Function def's:
library(ROCR)

GetAUC <- function(model, dataset, depvar, type="prob") {
  #result <- as.numeric(performance(prediction(predict(model, type="response", newdata=dataset), depvar), "auc")@y.values)
  if (class(model)[1] %in% c("glm","gbm"))
    result <- as.numeric(performance(prediction(predict(model, type="response", newdata=dataset), depvar), "auc")@y.values)
  else if (class(model)[2] == "randomForest")
    result <- as.numeric(performance(prediction(predict(model, type="prob", newdata=dataset)[,2], depvar), "auc")@y.values)
  else
    result <- as.numeric(performance(prediction(predict(model, type=type, newdata=dataset), depvar), "auc")@y.values)
  
  
  #perf <- performance(prediction(predict(model, type="response", newdata=dataset), depvar), "auc")@y.values, "tpr","fpr")
  #plot(perf, col="blue")
  return(result)
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
    
    for (wordcounter in 1:length(words)) {
      if (words[wordcounter] %in% scores$Sentiment) {
        score <- score + scores$Score[which(scores$Sentiment == words[wordcounter])]
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
  library(tm)
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
  library(tm)
  # install.packages("SnowballC")
  library(SnowballC)
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
  # Stem document 
  corpus <- tm_map(corpus, content_transformer(stemDocument))
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

# Manual Impute
# NewsDesk  SectionName  	SubsectionName
# ------------------------------------------------------
# Culture   Arts
# Business  Business Day
# Business  Technology
# Business  Crosswords/Games
# Science   Health
# OpEd      Opinion
# Styles		Style
# Foreign		World
# TStyle		?
# Magazine	Magazine
# Travel		Travel
# Styles		U.S.
# Metro		  N.Y. / Region
# National	U.S.
# ?		      Multimedia
# Sports		?
ManualImpute <- function() {
  if (file.exists(paste0(datafolder, "train.rda")) == T) {
    train2 <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
    test2 <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
  }
  
  data <- rbind(train2[,c(-9,-10)], test2[,-9])

  data$NewsDesk[data$NewsDesk == ""] <- "Unknown"
  data$SectionName[data$SectionName == ""] <- "Unknown"
  data$SubsectionName[data$SubsectionName == ""] <- "Unknown"

  newsdesks <- c("Culture","Science","OpEd","Styles","Foreign","Magazine","Travel","Styles","Metro","National")
  sections <- c("Arts","Health","Opinion","Style","World","Magazine","Travel","U.S.","N.Y. / Region","U.S.")
  data$NewsDesk <- as.character(data$NewsDesk)
  data$SectionName <- as.character(data$SectionName)
  
  for (counter in 1:length(newsdesks)) {
    print(paste("Section:", sections[counter]))
    print(table(data$NewsDesk[data$Section == sections[counter]]))
  }
  
  # Impute SectionName
  for (counter in 1:length(newsdesks)) {
    data$SectionName[data$SectionName=="Unknown" & data$NewsDesk==newsdesks[counter]] <- sections[counter]
  }
  # Impute NewsDesk
  for (counter in 1:length(newsdesks)) {
    data$NewsDesk[data$SectionName==sections[counter] & data$NewsDesk=="Unknown"] <- newsdesks[counter]
  }
  #table(data$NewsDesk)
  #table(data$SectionName)
  data$NewsDesk <- as.factor(data$NewsDesk)
  data$SectionName <- as.factor(data$SectionName)
  data$SubsectionName <- as.factor(data$SubsectionName)
  
  return(data)
}

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

library(mice)
ImputeWithMICE <- function() {
  if (file.exists(paste0(datafolder, "train.rda")) == F) {
    train2 <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
    test2 <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
  }
  
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

datafolder <- "C:/coding/Kaggle/TheAnalyticsEdge_2015/Data/"
submissionfolder <- "C:/coding/Kaggle/TheAnalyticsEdge_2015/Submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F)
  test <- read.csv(paste0(datafolder, "test.csv"), header=T, stringsAsFactors=F)
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
#CorrelationPlot(train[,])

train$NewsDesk[nchar(train$NewsDesk) == 0] <- "Unknown"
test$NewsDesk[nchar(test$NewsDesk) == 0] <- "Unknown"
train$SectionName[nchar(train$SectionName) == 0] <- "Unknown"
test$SectionName[nchar(test$SectionName) == 0] <- "Unknown"
train$SubsectionName[nchar(train$SubsectionName) == 0] <- "Unknown"
test$SubsectionName[nchar(test$SubsectionName) == 0] <- "Unknown"

# Create factor variables
train$Popular <- as.factor(train$Popular)
test$Popular <- as.factor(test$Popular)
train$NewsDesk <- as.factor(train$NewsDesk)
test$NewsDesk <- as.factor(test$NewsDesk)
train$SectionName <- as.factor(train$SectionName)
test$SectionName <- as.factor(test$SectionName)
train$SubsectionName <- as.factor(train$SubsectionName)
test$SubsectionName <- as.factor(test$SubsectionName)

# TODO: Merge Headline, Snippet and Abstract into one Corpus?
# TODO: Remember to merge variables (c(train$Headline, test$Headline)) into one Corpus before processing, and then split after!
# TODO: Extract date part (and convert into year, month, day?), (hour?), weekday from PubDate into separate variables.
# (Check train/test dates (if it makes sense with date) and/or if weekday is better)

train$PubDate = strptime(train$PubDate, "%Y-%m-%d %H:%M:%S")
test$PubDate = strptime(test$PubDate, "%Y-%m-%d %H:%M:%S")
# train$DatePart <- as.Date(as.Date(train$PubDate)) # NOTE: Not usable: Different dates/months in train and test!
# test$DatePart <- as.Date(as.Date(test$PubDate))
train$Weekday <- as.factor(train$PubDate$wday)
test$Weekday <- as.factor(test$PubDate$wday)

train$Hour <- as.factor(train$PubDate$hour)
test$Hour <- as.factor(test$PubDate$hour)

train$WordCountLog <- log(train$WordCount + 1) # IMPORTANT to avoid Inf problem, since we have some zero values
#train$WordCountLog[is.infinite(train$WordCountLog)] <- 0
test$WordCountLog <- log(test$WordCount + 1) # IMPORTANT to avoid Inf problem, since we have some zero values
#test$WordCountLog[is.infinite(test$WordCountLog)] <- 0

train$WordCountHeadline <- as.integer(sapply(train$Headline, SplitAndCountWords))
train$WordCountAbstract <- as.integer(sapply(train$Abstract, SplitAndCountWords))
test$WordCountHeadline <- as.integer(sapply(test$Headline, SplitAndCountWords))
test$WordCountAbstract <- as.integer(sapply(test$Abstract, SplitAndCountWords))

# Get question marks in abstract
train$QuestionMarks <- as.factor(GetQuestionMarks(train$Abstract))
test$QuestionMarks <- as.factor(GetQuestionMarks(test$Abstract))
tapply(train$Popular, train$QuestionMarks, mean)

# Imputed cols
train$NewsDeskImputed <- train.imputed$NewsDesk
test$NewsDeskImputed <- test.imputed$NewsDesk
train$SectionImputed <- train.imputed$Section
test$SectionImputed <- test.imputed$Section
train$SubsectionImputed <- train.imputed$Subsection
test$SubsectionImputed <- test.imputed$Subsection

# TEST: Cut the wordcounts into bins, check distribution with Popular outcome
# TODO: To ensure same factor levels, sets will have to be combined first!
nrow.train <- nrow(train)
nrow.test <- nrow(test)
train.test <- rbind(train[,-9], test)
train.test$WordCountCut <- cut(train.test$WordCount, breaks=8)
train.test$WordCountLogCut <- cut(train.test$WordCountLog + 1, breaks=8)
train.test$WordCountHeadlineCut <- cut(train.test$WordCountHeadline, breaks=6)
train.test$WordCountAbstractCut <- cut(train.test$WordCountAbstract, breaks=6)

train$WordCountCut <- head(train.test$WordCountCut, n=nrow.train)
test$WordCountCut <- tail(train.test$WordCountCut, n=nrow.test)
train$WordCountLogCut <- head(train.test$WordCountLogCut, n=nrow.train)
test$WordCountLogCut <- tail(train.test$WordCountLogCut, n=nrow.test)
levels(train$WordCountCut)
levels(test$WordCountCut)
barplot(table(train$WordCountCut))
barplot(table(test$WordCountCut))

barplot(table(train$WordCountLogCut), las=2)
barplot(table(train$WordCountHeadlineCut), las=2)
barplot(table(train$WordCountAbstractCut), las=2)

# TODO: Do word stemming both on AFINN set and train/test col before analysis here?
temp <- rbind(train[,-9], test)
temp <- train
temp <- GetNegAndPosScore(temp, "Abstract")
par(mfrow=c(1,2))
plot(sort(temp$PhraseScore), col="blue", main="Positive and negative score on Abstract from AFINN")
table(temp$PhraseScore)
barplot(table(temp$PhraseScore), col="cornflowerblue", main="Pos/neg score on Abstract from AFINN")
par(mfrow=c(1,1))
temp <- GetNegAndPosScore(temp, "Snippet")
par(mfrow=c(1,2))
plot(sort(temp$PhraseScore), col="blue", main="Pos/neg score on Snippet from AFINN")
table(temp$PhraseScore)
barplot(table(temp$PhraseScore), col="cornflowerblue", main="Pos/neg score on Snippet from AFINN")
par(mfrow=c(1,1))
barplot(tapply(temp$Popular, temp$PhraseScore, mean))

# TEST: Create 2-grams from Headline and Abstract
two.grams <- MyBigrams(train$Headline[1:100])
BigramTokenizer <- function(x) (MyBigrams(x)) # create 2-grams
tdm <- TermDocumentMatrix(train$Headline[1:100], control = list(tokenize = BigramTokenizer))

# ------------------------------------------------------------------------------------------------------------------------------

# Save Feature Engineered datasets for later use
save(train, file=paste0(datafolder, "trainCleaned.rda"))
save(test, file=paste0(datafolder, "testCleaned.rda"))

# -------------------------------------------------------------------------------------------------------------------------------

# Check variance, correlation
names(corpus.train)
cor.result <- cor(train[, c("WordCountHeadline","WordCountAbstract","WordCountLog")])
cor.result
#CorrelationPlot(result)
var.result <- var(train[, c("WordCountHeadline","WordCountAbstract","WordCount")])
var.result
cov.result <- cov(train[, c("WordCountHeadline","WordCountAbstract","WordCount")])
cov.result
# --------------------------------------------------------------------------------------------------------------------------------

# Do some visualizations
par(mfrow=c(2,1))
hist(train$WordCount, col="wheat", main="WordCount train") # Right skewed - log transform!
hist(train$WordCountLog, col="cornflowerblue", main="WordCount train") # Right skewed - log transform!
# hist(round(train$WordCountLog), col="cornflowerblue", main="WordCount train") # Right skewed - log transform!
par(mfrow=c(1,1))
par(mfrow=c(2,1))
hist(test$WordCount, col="wheat", main="WordCount test") # Right skewed - log transform!
hist(test$WordCountLog, col="cornflowerblue", main="WordCount test") # Right skewed - log transform!
par(mfrow=c(1,1))

hist(nchar(train$Headline), col="wheat", main="Length of Headline", freq=T)
mean(nchar(train$Headline))
hist(nchar(train$Abstract), col="wheat", main="Length of Abstract", freq=T)
mean(nchar(train$Abstract))
hist(nchar(train$Snippet), col="wheat", main="Length of Snippet", freq=T)
mean(nchar(train$Snippet))
hist(as.numeric(train$Hour), col="wheat", main="Hour", freq=T)

par(mar=c(6,3,2,1))
par(mfrow=c(2,2))
# TODO: Very different popularity, add weighted variables for these (but check and scale/adjust with var frequency)?
barplot(tapply(train$Popular, train$NewsDesk, mean), las=2, main="Popular by NewsDesk", col="wheat")
barplot(tapply(train$Popular, train$SubsectionName, mean), las=2, main="Popular by SubsectionName", col="cornflowerblue")
barplot(tapply(train$Popular, train$SectionName, mean), las=2, main="Popular by SectionName", col="cornflowerblue")
barplot(tapply(train$Popular, train$Weekday, mean), las=2, main="Popular by Weekday", col="wheat")
par(mfrow=c(1,1))
par(mar=c(3,3,2,1))

# TODO: Get the proportions, adjusted for frequency

par(mar=c(6,3,2,1))
par(mfrow=c(2,2))
barplot(table(train$NewsDesk), main="NewsDesk", col="wheat", las=2)
barplot(table(train$SubsectionName), main="SubsectionName", col="cornflowerblue", las=2)
barplot(table(train$SectionName), main="SectionName", col="cornflowerblue", las=2)
barplot(table(train$Weekday), main="Weekday", col="wheat", las=1)
par(mfrow=c(1,1))
par(mar=c(3,3,2,1))

barplot(with(train, tapply(WordCount, Popular, mean)), main="WordCount by Popular 0/1", col=c("orange","darkcyan"))
par(mfrow=c(1,2))
barplot(with(train[train$WordCount > 1500,], tapply(WordCount, Popular, mean)),
        main="WordCount (> 1500) by Popular 0/1", col=c("orange","darkcyan")) # NOTE: Very different proportions...
barplot(with(train[train$WordCount <= 1500,], tapply(WordCount, Popular, mean)),
        main="WordCount (<= 1500) by Popular 0/1", col=c("orange","darkcyan"))
par(mfrow=c(1,1))

barplot(with(train, tapply(WordCountAbstract, Popular, mean)), main="WordCountAbstract by Popular 0/1", col=c("orange","darkcyan"))
barplot(with(train, tapply(WordCountHeadline, Popular, mean)), main="WordCountHeadline by Popular 0/1", col=c("orange","darkcyan"))
barplot(with(train, tapply(as.numeric(Hour), Popular, mean)), main="Hour by Popular 0/1", col=c("orange","darkcyan"))

hist(train$WordCount, col="wheat", main="WordCount") # Only WordCount is right skewed
hist(train$WordCountHeadline, col="wheat", main="WordCountHeadline")
hist(train$WordCountAbstract, col="wheat", main="WordCountAbstract")

result <- table(train$Section, train$Subsection)
result.total <- rowSums(result)

# ------------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------

# Create a baseline model (= the most frequent outcome)
table(train$Popular)[1] / nrow(train) # Baseline accuracy (not popular (0)): 0.8327

# ------------------------------------------------------------------------------------------------------------------------------

train.total <- train[, -10] # IMPORTANT! Get rid of uniqueId column if using the set "as-is"!
#train.total <- ManualImpute(train.total)
#train.total <- ImputeWithMICE(train.total)
#train.total$SectionName <- train.imputed$SectionName # TEST imputed!
#train.total$SubsectionName <- train.imputed$SubsectionName # TEST imputed!

# ------------------------------------------------------------------------------------------------------------------------------

# Create a corpus on train.total$Headline
result <- CreateCorpus(train.total$Headline, 0.987) # BEST SO FAR (27 terms). Was: 0.99, then 0.98, then 0.985, then 0.987
result <- CreateCorpus(train.total$Headline, 0.995) # Was: 0.99, then 0.98, then 0.985, then 0.987
result <- CreateCorpus(train.total$Abstract, 0.99)
result <- CreateCorpus(train.total$Snippet, 0.997)
result <- CreateCorpus(train.total$Snippet, 0.997, T)
result <- CreateCorpus2(c(train.total$Headline, train.total$Abstract), 0.99) # GLM: AUC=?
result <- CreateCorpus2(c(train.total$Headline, train.total$Abstract), 0.995) # GLM: AUC=0.7362
result <- CreateCorpus2(c(train.total$Headline, train.total$Abstract), 0.99) # GLM: AUC=0.6801

#result <- CreateCorpus(c(train.total$Abstract, train.total$Headline), 0.987) # Try a combined corpus
corpus <- result[[1]] # Get the sparse DTM as df 
#corpus
dtm <- result[[2]] # Get the sparse DTM
dtm
dim(corpus)

par(mar=c(6,3,2,1))
barplot(sort(colSums(corpus), decreasing=T)[1:50], las=2, col="powderblue", main="colSums corpus")
par(mar=c(3,3,2,1))

# TEST: Create some cols with popular words, and check
corpus$New <- as.factor(ifelse(corpus[, c("new")] > 0, 1, 0))
table(corpus$New)
corpus$York <- as.factor(ifelse(corpus[, c("york")] > 0, 1, 0))
corpus$Week <- as.factor(ifelse(corpus[, c("week")] > 0, 1, 0))
corpus$Fashion <- as.factor(ifelse(corpus[, c("fashion")] > 0, 1, 0))
table(corpus$York)
table(corpus$Popular, corpus$New)
table(corpus$Popular, corpus$York)
table(corpus$Popular, corpus$Week)
table(corpus$Popular, corpus$Fashion)

# ------------------------------------------------------------------------------------------------------------------------------

# Do a wordcloud on the corpus
library(wordcloud)
library(RColorBrewer)
wordCloud <- wordcloud(colnames(corpus), colSums(corpus), scale=c(4, 0.85), colors=brewer.pal(9, "Blues"))

# ------------------------------------------------------------------------------------------------------------------------------

# Add predictors
#temp <- GetNegAndPosScore(train, "Abstract")
#corpus$PhraseScore <- as.factor(temp$PhraseScore)

corpus$Weekday <- train$Weekday
corpus$Hour <- train$Hour

corpus$WordCount <- train$WordCount # NOTE: non-logtransformed wordcount
corpus$WordCountLog <- train$WordCountLog # NOTE: log(wordcount) TEST: Works best on Logistic Regression, use non-log on RF??
#corpus$WordCountLog <- round(train$WordCountLog) # NOTE: log(wordcount)
corpus$WordCountHeadline <- train$WordCountHeadline
corpus$WordCountAbstract <- train$WordCountAbstract

#corpus$WordCountCut <- train$WordCountCut
#corpus$WordCountLogCut <- train$WordCountLogCut
# corpus$WordCountHeadlineCut <- train$WordCountHeadlineCut
# corpus$WordCountAbstractCut <- train$WordCountAbstractCut

corpus$QuestionMarks <- train$QuestionMarks # TEST!
corpus$NewsDesk <- train$NewsDesk
corpus$NewsDeskImputed <- train.imputed$NewsDesk
corpus$SectionName <- train$SectionName
corpus$SectionNameImputed <- train.imputed$SectionName
corpus$SubsectionName <- train.total$SubsectionName
#corpus$SubsectionNameImputed <- train.imputed$SubsectionName
corpus$Popular <- as.factor(train$Popular)
head(corpus, n=1)
sapply(corpus, class)

# corpus.train <- head(corpus, n=nrow.train)
# corpus.validation <- tail(corpus, n=nrow.validation)
#corpus.validation$PhraseScore <- factor(corpus.validation$PhraseScore, levels=levels(corpus.train$PhraseScore))
# dim(corpus.train)
# dim(train.subset) # Should be equal
# http://www.r-bloggers.com/an-intro-to-ensemble-learning-in-r/

# TEST: Scale some vars for GLM/GLMNET:
corpus.scaled <- corpus
corpus.scaled$WordCount <- scale(corpus$WordCount)
corpus.scaled$WordCountLog <- scale(corpus$WordCountLog)
corpus.scaled$WordCountHeadline <- scale(corpus$WordCountHeadline)
corpus.scaled$WordCountAbstract <- scale(corpus$WordCountAbstract)

# Create train and validation sets
result <- CreateTrainAndValidationSets(corpus)
train.subset <- result[[1]]
validation.subset <- result[[2]]
# or do:
library(caTools)
set.seed(1000)
split <- sample.split(corpus$Popular, SplitRatio=0.7)
train.subset <- subset(corpus, split==TRUE)
validation.subset <- subset(corpus, split==FALSE)
corpus.train <- subset(corpus, split==TRUE)
corpus.validation <- subset(corpus, split==FALSE)

# Same, quantitative predictors scaled
set.seed(1000)
split <- sample.split(corpus.scaled$Popular, SplitRatio=0.7)
train.subset <- subset(corpus.scaled, split==TRUE)
validation.subset <- subset(corpus.scaled, split==FALSE)
corpus.train <- subset(corpus.scaled, split==TRUE)
corpus.validation <- subset(corpus.scaled, split==FALSE)

# Get rows
#nrow.train <- nrow(train.subset)
#nrow.validation <- nrow(validation.subset)
#train.total <- rbind(train.subset, validation.subset)
#dim(train.total)

# Try GBM
library(gbm)
GBM_NTREES = 300
GBM_SHRINKAGE = 0.2
GBM_DEPTH = 25
GBM_MINOBS = 25
# Build the GBM model
names(corpus.train)
x.cols <- c(17:26,28)
GBM_model <- gbm.fit(x = corpus.train[, x.cols], y = as.integer(corpus.train$Popular)-1, distribution="bernoulli",
                     n.trees = GBM_NTREES, shrinkage = GBM_SHRINKAGE, interaction.depth = GBM_DEPTH,
                     n.minobsinnode = GBM_MINOBS, verbose = TRUE) 
summary(GBM_model, las=2)
predict.gbm <- predict(GBM_model, newdata=corpus.validation[, x.cols], type="response", n.trees=GBM_NTREES)
plot(sort(predict.gbm), col="blue", main="predict.gbm")
prediction.result.gbm <- table(corpus.validation$Popular, predict.gbm > 0.5)
prediction.result.gbm
sum(diag(prediction.result.gbm)) / sum(prediction.result.gbm)

# Try NaiveBayes
library(e1971)
model.nb <- naiveBayes(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCountLog+Weekday+Hour,
                       data=corpus.train, type="class")
model.nb <- naiveBayes(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCountLog+Weekday+Hour,
                       data=corpus.train, type="class")
summary(model.nb)
predict.nb <- predict(model.nb, newdata=corpus.validation, type="class")
plot(predict.nb, col=c("Red","Green"), main="predict.nb")
prediction.result.nb <- table(as.integer(corpus.validation$Popular)-1, as.integer(predict.nb)-1 > 0.5)
prediction.result.nb
sum(diag(prediction.result.nb)) / sum(prediction.result.nb)

# Try SVM on corpus with added predictors (only scaled numerics here?)
library(e1071)
model.svm <- svm(Popular ~ NewsDesk+SectionName+SubsectionName+WordCountLog+Weekday+Hour,
                 data=corpus.train)
predict.svm <- predict(model.svm, newdata=corpus.validation, type="class")
barplot(table(predict.svm), col=c("red","green"), main="predict.svm")
prediction.result.svm <- table(corpus.validation$Popular, (as.integer(predict.svm)-1) > 0.5)
prediction.result.svm
sum(diag(prediction.result.svm)) / sum(prediction.result.svm)

# Try GLM on train.subset (NewsDesk+SectionName+SubsectionName+WordCountLog+Weekday+Hour)
model.glm <- glm(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCountLog+Weekday+Hour,
                 data=corpus.train, family=binomial)
model.glm <- glm(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCountLog+WordCountHeadline+
                   WordCountAbstract+Weekday+Hour+QuestionMarks,
                 data=corpus.train, family=binomial)
model.glm <- glm(as.factor(Popular) ~ WordCountLog+WordCountHeadline+
                   WordCountAbstract+Weekday+Hour+QuestionMarks,
                 data=corpus.train, family=binomial)
model.glm <- glm(as.factor(Popular) ~ WordCountLog+WordCountHeadline+WordCountAbstract,
                 data=corpus.train, family=binomial)
model.glm <- glm(as.factor(Popular) ~ ., data=corpus.train, family=binomial(link="logit"))
summary(model.glm)
predict.glm <- predict(model.glm, newdata=corpus.validation, type="response")
plot(sort(predict.glm), type="o", col="blue", main="predict.glm")
prediction.result.glm <- table(corpus.validation$Popular, predict.glm > 0.5)
prediction.result.glm
sum(diag(prediction.result.glm)) / sum(prediction.result.glm)
anova(model.glm)

# Try GLM on corpus with added predictors
model.glm <- glm(as.factor(Popular) ~ ., data=corpus.train, family=binomial)
summary(model.glm)
predict.glm <- predict(model.glm, newdata=corpus.validation, type="response")
# predict.glm
#predict.glm[predict.glm < 0] <- 0 
plot(sort(predict.glm), type="o", col="blue", main="predict.glm")
prediction.result.glm <- table(corpus.validation$Popular, predict.glm > 0.5)
prediction.result.glm
sum(diag(prediction.result.glm)) / sum(prediction.result.glm)

# TODO: k-fold cross-validation on glm. See cv.glm in the boot package
library(boot)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
cv.glm(data=corpus.train[,c(246,249:252,256:258)], glmfit=model.glm, cost=cost, K=10)

scores <- numeric()
pos <- 1
for (counter in seq(50,1000,25)) {
  model.rf1 <- randomForest(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCount+
                              WordCountHeadline+WordCountAbstract+Weekday+Hour,
                            data=corpus.train, ntree=counter) # BEST SO FAR
  predict.rf1 <- predict(model.rf1, newdata=corpus.validation, type="prob")
  prediction.result.rf1 <- table(corpus.validation$Popular, predict.rf1[,2] > 0.5)
  scores[pos] <- sum(diag(prediction.result.rf1)) / sum(prediction.result.rf1)
  print(paste0("Score ", pos, ": ", scores[pos]))
  pos <- pos + 1
}
plot(scores, type="o", col="blue", main="RF scores")


# Try Random Forest with added predictors (TODO: Loop though array of ntree values and get best AUC)
model.rf1 <- randomForest(as.factor(Popular) ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+WordCountHeadline+
                            WordCountAbstract+Weekday+Hour+QuestionMarks,
                          data=corpus.train) # BEST SO FAR!
model.rf1 <- randomForest(as.factor(Popular) ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+
                            WordCountHeadline+WordCountAbstract+Weekday+Hour+QuestionMarks,
                          data=corpus.train, nodesize=2, mtry=4) # NOTE: With imputed cols
model.rf1 <- randomForest(as.factor(Popular) ~ NewsDesk+SectionName+SubsectionName+WordCount+WordCountHeadline+
                            WordCountAbstract+Weekday+Hour+QuestionMarks,
                          data=corpus.train)
varImpPlot(model.rf1, col="blue", pch=16, cex=.8)
# *** TODO: Cross-validate RF? And/or tune number of trees
# http://stackoverflow.com/questions/19760169/how-to-perform-random-forest-cross-validation-in-r

names(corpus.train)
# predict.rf <- predict(model.rf, newdata=corpus.validation, type="response")
predict.rf1 <- predict(model.rf1, newdata=corpus.validation, type="prob")
plot(sort(predict.rf1[,2]), type="o", col="blue", main="predict.rf1")
prediction.result.rf1 <- table(corpus.validation$Popular, predict.rf1[,2] > 0.5)
prediction.result.rf1
sum(diag(prediction.result.rf1)) / sum(prediction.result.rf1)

# Combine the GLM and RF
ratio <- 4
predict.combined <- ((predict.rf1[,2] * (ratio - 1)) + predict.glm) / ratio
#predict.combined[predict.combined < 0] <- 0
min(predict.combined)
max(predict.combined)
plot(sort(predict.combined), type="o", col="blue", main="predict.combined")
prediction.result.combined <- table(corpus.validation$Popular, predict.combined > 0.5)
prediction.result.combined
sum(diag(prediction.result.combined)) / sum(prediction.result.combined)

# Calibrate RF score
predict.rf1.calibrated <- CalibrateRF(predict.rf1, .94)
par(mfrow=c(1,2))
plot(sort(predict.rf1.calibrated[,2]), type="o", col="blue", main="predict.rf1.calibrated")
plot(sort(predict.rf1[,2]), type="o", col="blue", main="predict.rf1")
head(predict.rf1, n=10)
head(predict.rf1.calibrated, n=10)
prediction.result.rf1 <- table(corpus.validation$Popular, predict.rf1[,2] > 0.5)
prediction.result.rf1
prediction.result.rf1.calibrated <- table(corpus.validation$Popular, predict.rf1.calibrated[,2] > 0.5)
prediction.result.rf1.calibrated
#ConfusionMatrix(prediction.result.rf1.calibrated, labels=c("0","1"))
par(mfrow=c(1,1))
sum(diag(prediction.result.rf1.calibrated)) / sum(prediction.result.rf1.calibrated)
sum(diag(prediction.result.rf1)) / sum(prediction.result.rf1)

# ---------------------------------------------------------------------------------------------------------------------------

# Set up caret and train control to do repeated CV
# Cross-validation:
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
# http://cran.r-project.org/web/packages/randomForest/randomForest.pdf

# K-fold cross validation: Split the training set into <k> pieces. Then predict fold <1:k>, one at a time,
# with the rest of the folds. Last, average the accuracy over all the folds to determine which parameter values
# we want to use for the final model. For tree, it is called "cp" (Complexity Paramater), works like UC and adj.R^2.
# Measures trade-off between model complexity and accuracy on training set.
# A smaller cp value leads to a bigger tree, so might overfit.
library(caret)
library(e1071)
getModelInfo("rf")
numFolds <- trainControl(method="cv", number=10, classProbs=T)
#cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
corpus.train2 <- corpus.train
corpus.train2$Popular <- as.factor(ifelse(corpus.train2$Popular == 0, "No", "Yes")) # Convert so caret doesen't bork on the name
train.caret <- train(Popular ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+
                       WordCountHeadline+WordCountAbstract+Weekday+Hour,
                     data=corpus.train2, method="rf", trControl=numFolds, metric="Accuracy")
summary(train.caret)
predict.caret <- predict(train.caret, newdata=corpus.validation, type="prob")
prediction.result.caret <- table(corpus.validation$Popular, predict.caret[,2] > 0.5)
prediction.result.caret
sum(diag(prediction.result.caret)) / sum(prediction.result.caret)

# ---------------------------------------------------------------------------------------------------------------------------

# Get AUC
#GetAUC(train.caret, corpus.validation, corpus.validation$Popular)
GetAUC(model.glm, corpus.validation, corpus.validation$Popular)
GetAUC(model.gbm, corpus.validation, corpus.validation$Popular)
GetAUC(model.rf, corpus.validation, corpus.validation$Popular)
GetAUC(model.rf1, corpus.validation, corpus.validation$Popular)
GetAUC(model.nb, corpus.validation, corpus.validation$Popular)
GetAUC(model.glm, corpus.train, corpus.train$Popular)
GetAUC(model.rf, corpus.train, corpus.train$Popular)
GetAUC(model.rf1, corpus.train, corpus.train$Popular)
GetAUC(model.nb, corpus.train, corpus.train$Popular)

# Show ROCR colorized plot
library(ROCR)
par(mar=c(3,3,2,2))
predROCR = prediction(predict.rf1[,2], corpus.validation$Popular)
predROCR = prediction(predict.glm, corpus.validation$Popular)
predROCR = prediction(predict.gbm, corpus.validation$Popular)
predROCR = prediction(predict.combined, corpus.validation$Popular)
predROCR = prediction(predict.caret[,2], corpus.validation$Popular)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on Corpus", lwd=3)
lines(c(0,1),c(0,1), col="gray", lty=2)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
# NOTE: At a cutoff of 0.6-0.8, we predict a good TP rate, while at the same time having a low FP rate.
par(mar=c(3,3,2,1))
# Compute AUC
performance(predROCR, "auc")@y.values
sn <- slotNames(predROCR)
sapply(sn, function(x) length(slot(predROCR, x)))


# ---------------------------------------------------------------------------------------------------------------------------

# Do CART model on corpus (Not working here?)
library(rpart)
library(rpart.plot)
rownames(corpus.train) <- NULL

# Train to find optimal cp parameter
library(caret)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(as.factor(Popular) ~ ., data=corpus.train,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.005
corpusCART <- rpart(as.factor(Popular) ~ ., data=corpus.train, method="class", cp=cp.value)
corpusCART <- rpart(as.factor(Popular) ~ ., data=corpus.train[,c(1:243,246)], method="class", cp=cp.value)
summary(corpusCART)
prp(corpusCART, cex=.8, col="blue", main="Headline")
# Evaluate the performance of the model
predictCART <- predict(corpusCART, newdata=corpus.validation, type="class")
result <- table(corpus.validation$Popular, predictCART)
result
# Compute accuracy
sum(diag(result)) / sum(result)

# ---------------------------------------------------------------------------------------------------------------------------

# Do clustering on corpus
# http://beyondvalence.blogspot.no/2014/01/text-mining-5-hierarchical-clustering.html
library(stats) # hclust and kmeans

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
distances <- dist(corpus.train[, 1:(ncol(corpus.train)-1)], method="euclidean") # Takes a long time sometimes, so save for later use
save(distances, file=paste0(datafolder, "distances.corpus.train.rda"))
# load(file=paste0(folder, "distances.rda"))

cluster.headline <- hclust(distances, method="ward.D")
library(ggdendro)
# load package ape; remember to install it: install.packages('ape')
library(ape)
# plot basic tree
plot(as.phylo(cluster.headline), cex=0.7, label.offset=1)
hcd <- as.dendrogram(cluster.headline)
summary(cluster.headline)
# plot(hcd)
cluster.headline.cuts <- cutree(cluster.headline, k=2)
summary(cluster.headline.cuts)
table(cluster.headline.cuts)
table(train$Popular)

set.seed(1000)
km.clusters <- kmeans(distances, centers=2)
km.clusters <- kmeans(corpus.train[, 1:(ncol(corpus.train)-1)], centers=2, iter.max=10, nstart=4)
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
table(corpus.train$Popular)
# NOTE: This...
sum(sapply(clusterList.km, nrow))
# ...should be equal to:
nrow(corpus.train)
table(clusterList.km[[1]]$Popular)
table(clusterList.km[[2]]$Popular)


# ------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------

# Create the final model
nrow.train <- nrow(train)
nrow.test <- nrow(test)
#train.corpora <- c(train.total$Headline, train.total$Abstract)
#test.corpora <- c(test$Headline, test$Abstract)
result <- CreateCorpus(c(train.total$Headline, test$Headline), 0.997) # BEST SO FAR
result <- CreateCorpus2(c(train.total$Headline, train.total$Abstract, test$Headline, test$Abstract), 0.99) # TEST!
#result <- CreateCorpus(c(train.corpora, test.corpora), 0.987)
corpus <- result[[1]]
dtm <- result[[2]]
dtm
corpus.train <- head(corpus, n=nrow.train)
corpus.test <- tail(corpus, n=nrow.test)
dim(train)
dim(corpus.train)
dim(test)
dim(corpus.test)

corpus.train$Weekday <- train$Weekday
corpus.train$Hour <- train$Hour
corpus.train$WordCount <- train$WordCount
corpus.train$WordCountLog <- train$WordCountLog
corpus.train$WordCountHeadline <- train$WordCountHeadline
corpus.train$WordCountAbstract <- train$WordCountAbstract

#corpus.train$WordCountLogCut <- train$WordCountLogCut
#corpus.train$WordCountHeadlineCut <- train$WordCountHeadlineCut
#corpus.train$WordCountAbstractCut <- train$WordCountAbstractCut

corpus.train$QuestionMarks <- train$QuestionMarks # TEST!
# corpus.train$WordCountLog <- train$WordCountLog
#corpus.train$WordCountLog[corpus.train$WordCountLog < 0] <- 0
corpus.train$NewsDesk <- train$NewsDesk
corpus.train$SectionName <- train$SectionName
corpus.train$SubsectionName <- train$SubsectionName
corpus.train$NewsDeskImputed <- train.imputed$NewsDesk
corpus.train$SectionNameImputed <- train.imputed$SectionName # TEST imputed!
#corpus.train$SubsectionNameImputed <- train.imputed$SubsectionName # TEST imputed!
corpus.train$Popular <- train$Popular
dim(corpus.train)
dim(train)
sapply(corpus.train, class)

corpus.test$Weekday <- test$Weekday
corpus.test$Hour <- test$Hour
corpus.test$WordCount <- test$WordCount
corpus.test$WordCountLog <- test$WordCountLog
corpus.test$WordCountHeadline <- test$WordCountHeadline
corpus.test$WordCountAbstract <- test$WordCountAbstract

#corpus.test$WordCountLogCut <- test$WordCountLogCut
#corpus.test$WordCountHeadlineCut <- test$WordCountHeadlineCut
#corpus.test$WordCountAbstractCut <- test$WordCountAbstractCut

corpus.test$QuestionMarks <- test$QuestionMarks # TEST!
# corpus.test$WordCountLog <- test$WordCountLog
# corpus.test$WordCountLog[corpus.test$WordCountLog < 0] <- 0
# http://stackoverflow.com/questions/24872489/randomforest-does-not-work-when-training-set-has-more-factors-than-test-set?rq=1
corpus.test$NewsDesk <- factor(test$NewsDesk, levels=levels(train$NewsDesk)) # NOTE syntax to create the same levels in train/test
corpus.test$SectionName <- factor(test$SectionName, levels=levels(train$SectionName)) # NOTE syntax to create the same levels in train/test
corpus.test$SubsectionName <- factor(test$SubsectionName, levels=levels(train$SubsectionName)) # NOTE syntax to create the same levels in train/test
corpus.test$NewsDeskImputed <- factor(test.imputed$NewsDesk, levels=levels(train$NewsDesk)) # NOTE syntax to create the same levels in train/test
corpus.test$SectionNameImputed <- factor(test.imputed$SectionName, levels=levels(train$SectionName)) # TEST imputed!
corpus.test$SubsectionNameImputed <- test.imputed$SubsectionName # TEST imputed!
dim(corpus.test)
dim(test)

# Do GBM
library(gbm)
GBM_NTREES = 300
GBM_SHRINKAGE = 0.2
GBM_DEPTH = 25
GBM_MINOBS = 25
# Build the GBM model
names(corpus.train)
GBM_model <- gbm.fit(x = corpus.train[, 220:229], y = corpus.train$Popular, distribution="bernoulli",
                     n.trees = GBM_NTREES, shrinkage = GBM_SHRINKAGE, interaction.depth = GBM_DEPTH,
                     n.minobsinnode = GBM_MINOBS, verbose = TRUE) 
summary(GBM_model, las=2)
predict.gbm <- predict(GBM_model, newdata=corpus.test[, 220:229], type="response", n.trees=GBM_NTREES)
plot(sort(predict.gbm), type="o", col="blue", main="predict.gbm, full dataset")
min(predict.gbm)
max(predict.gbm)

# Do GLM
model.glm <- glm(as.factor(Popular) ~ ., data=corpus.train, family=binomial)
model.glm <- glm(as.factor(Popular) ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+WordCountHeadline+
                            WordCountAbstract+Weekday+Hour+QuestionMarks,
                          data=corpus.train, family=binomial(link="logit"))
model.glm <- glm(as.factor(Popular) ~ ., data=corpus.train[, c(1:100,113)], family=binomial)

summary(model.glm)
predict.glm <- predict(model.glm, newdata=corpus.test, type="response")
predict.glm
plot(sort(predict.glm), type="o", col="blue", main="predict.glm, full dataset")
min(predict.glm)
max(predict.glm)
anova(model.glm)

# Do Random Forest
model.rf <- randomForest(Popular ~ ., data=corpus.train)
model.rf <- randomForest(as.factor(Popular) ~ ., data=corpus.train)
summary(model.rf)
model.rf1 <- randomForest(as.factor(Popular) ~ NewsDeskImputed+SectionNameImputed+SubsectionName+WordCount+WordCountHeadline+
                            WordCountAbstract+Weekday+Hour+QuestionMarks,
                          data=corpus.train) # BEST SO FAR!
varImpPlot(model.rf1, col="blue", pch=16, cex=.8)
predict.rf <- predict(model.rf, newdata=corpus.test, type="prob")
predict.rf <- predict(model.rf, newdata=corpus.test, type="response")
predict.rf1 <- predict(model.rf1, newdata=corpus.test, type="prob")
predict.rf2 <- predict(model.rf2, newdata=corpus.test, type="prob")
predict.rf2 <- predict(model.rf2, newdata=corpus.test, type="response")
#predict.rf[,2]
plot(sort(predict.rf[,2]), type="o", col="blue", main="predict.rf, full dataset")
plot(sort(predict.rf1[,2]), type="o", col="blue", main="predict.rf1, full dataset")
plot(sort(predict.rf2[,2]), type="o", col="blue", main="predict.rf2, full dataset")
plot(sort(predict.rf), type="o", col="blue", main="predict.rf, full dataset")
plot(sort(predict.rf2), type="o", col="blue", main="predict.rf2, full dataset")
min(predict.rf[,2])
max(predict.rf[,2])
min(predict.rf1[,2])
max(predict.rf1[,2])
min(predict.rf2[,2])
max(predict.rf2[,2])
min(predict.rf)
max(predict.rf)
min(predict.rf2)
max(predict.rf2)

# combine rf and glm
ratio <- 2
# RF and GLM:
predict.combined <- ((predict.rf1[,2] * (ratio - 1)) + predict.glm) / ratio
# RF and GBM:
predict.combined <- ((predict.rf1[,2] * (ratio - 1)) + predict.gbm) / ratio
min(predict.combined)
max(predict.combined)
plot(sort(predict.combined), type="o", col="blue", main="predict.combined")

# -------------------------------------------------------------------------------------------------------------------------------

# Get ROC
GetAUC(model.glm, corpus.train, corpus.train$Popular)
GetAUC(model.gbm, corpus.train, corpus.train$Popular)
GetAUC(model.rf, corpus.train, corpus.train$Popular)
GetAUC(model.rf1, corpus.train, corpus.train$Popular)
GetAUC(model.rf2, corpus.train, corpus.train$Popular)

# TODO: Plot the AUC curve

# -------------------------------------------------------------------------------------------------------------------------------

# Try h2o
package.install("h2o")
suppressMessages(library(h2o))

#train$target <- as.factor(train$target)

# TODO: Scale all feat_<n> cols?

#localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
dat_h2o <- as.h2o(localH2O, corpus.train, key='train')
dat_h2o.test <- as.h2o(localH2O, corpus.validation, key='test')
dat_h2o <- as.h2o(localH2O, corpus.train, key='train')
dat_h2o.test <- as.h2o(localH2O, corpus.test, key='test')
x.cols <- c(115:126)
y.col <- 127

model.dl <- 
  h2o.deeplearning(x=x.cols, # column numbers for predictors
                   y=y.col, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   classification=T,
                   activation="RectifierWithDropout", # or 'TanhWithDrouput'
                   #autoencoder=T,
                   input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=F,
                   fast_mode=T,
                   l1=1e-5,
                   #l2=1e-5
                   hidden=c(256, 256, 256), # three layers of 256 nodes
                   epochs=10) # max. no. of epochs (try epocs=0.1??)
# Get model info:
# http://learn.h2o.ai/content/hands-on_training/deep_learning.html
model.dl
best_model <- model.dl@model[[1]]
best_model
best_params <- best_model@model$params
best_params$activation
best_params$hidden
best_params$l1

model.gbm <-
  h2o.gbm(x=x.cols, y=y.col, distribution = "bernoulli", data=dat_h2o, key="gbm", n.trees=100, 
          interaction.depth=5, n.minobsinnode=10, shrinkage=0.1, n.bins=20,
          group_split=T, importance=FALSE, nfolds=0, holdout.fraction=0,
          balance.classes=T, max.after.balance.size=5, class.sampling.factors=NULL,
          grid.parallelism=1)
model.gbm

# Do classification
model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, data=dat_h2o, key="rf", classification=TRUE, ntree=500,
                   depth=20, mtries= -1, sample.rate=2/3, nbins=20, seed= -1, 
                   importance=FALSE, score.each.iteration=FALSE, nfolds=0, 
                   holdout.fraction=0, nodesize=1, balance.classes=F, # Do F?
                   max.after.balance.size=5, class.sampling.factors=NULL, 
                   doGrpSplit=TRUE, verbose=FALSE, oobee=TRUE, stat.type="ENTROPY", 
                   type="fast")
# Do regression
model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, data=dat_h2o, key="rf", classification=F, ntree=500,
                   depth=20, mtries= -1, sample.rate=2/3, nbins=20, seed= -1, 
                   importance=FALSE, score.each.iteration=FALSE, nfolds=0, 
                   holdout.fraction=0, nodesize=1, balance.classes=F, # Do F? 
                   max.after.balance.size=5, class.sampling.factors=NULL, 
                   doGrpSplit=TRUE, verbose=FALSE, oobee=TRUE, stat.type="ENTROPY", 
                   type="BigData")
model.rf

h2o_yhat_test.dl <- h2o.predict(model.dl, dat_h2o.test)
df_h2o_yhat_test.dl <- as.data.frame(h2o_yhat_test.dl)
head(df_h2o_yhat_test.dl)

h2o_yhat_test.gbm <- h2o.predict(model.gbm, dat_h2o.test)
df_h2o_yhat_test.gbm <- as.data.frame(h2o_yhat_test.gbm)
head(df_h2o_yhat_test.gbm)

h2o_yhat_test.rf <- h2o.predict(model.rf, dat_h2o.test)
df_h2o_yhat_test.rf <- as.data.frame(h2o_yhat_test.rf) 
head(df_h2o_yhat_test.rf)

plot(sort(df_h2o_yhat_test.rf$predict))
plot(sort(1-df_h2o_yhat_test.gbm[,2]))
plot(sort(1-df_h2o_yhat_test.rf[,2]))
min(df_h2o_yhat_test.rf$predict)
max(df_h2o_yhat_test.rf$predict)

prediction.result.gbm <- table(corpus.validation$Popular, (1-df_h2o_yhat_test.gbm[,2]) > 0.5)
sum(diag(prediction.result.gbm)) / sum(prediction.result.gbm)
prediction.result.rf <- table(corpus.validation$Popular, df_h2o_yhat_test.rf$predict > 0.5)
prediction.result.rf <- table(corpus.validation$Popular, 1-df_h2o_yhat_test.rf[,2] > 0.5)
prediction.result.rf
sum(diag(prediction.result.rf)) / sum(prediction.result.rf) # 0.9112 with threshold 0.987

# -----------------------------------------------------------------------------------------------------------------

# Create submission file
predict.rf[predict.rf < 0] <- 0 # TODO: Find out why predictions are below zero (even if it is miniscule values)!
predict.rf2[predict.rf2 < 0] <- 0 # TODO: Find out why predictions are below zero (even if it is miniscule values)!
prediction <- predict.rf[,2] # TODO: Replace with whatever model works best
prediction <- predict.rf1[,2] # TODO: Replace with whatever model works best
prediction <- predict.rf2[,2] # TODO: Replace with whatever model works best
prediction <- predict.rf # TODO: Replace with whatever model works best
prediction <- predict.rf2 # TODO: Replace with whatever model works best
prediction <- predict.combined # TODO: Replace with whatever model works best
prediction <- df_h2o_yhat_test.rf$predict
# prediction <- predict.combined # TODO: Replace with whatever model works best
# prediction <- df_h2o_yhat_test.rf$predict
par(mfrow=c(2,2))
hist(prediction, col="wheat")
plot((prediction), col="blue", pch=21, bg="cyan", main="Prediction result")
plot(sort(prediction), type="o", col="blue", main="Sorted prediction curve")
par(mfrow=c(1,1))
# Create the submission file
options("scipen"=100, "digits"=8)
MySubmission <- data.frame(UniqueID=test$UniqueID, Probability1=prediction)
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "RF_GLM_ensemble")
# Score: 0.93018 with regular RF (classification, standard params), ntree=500, Weekday, WordCount, factor(NewsDeskimputed),
# factor(SectionNameImputed), factor(SubSectionName), Hour, WordCountHeadline, WordCountAbstract
# NOTE: No bag-of-words used for best score!

