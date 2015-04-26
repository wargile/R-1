# The Analytics Edge week 5 - Text Analytics
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/text-analytics/
# ------------------------------------------------------------------------------------

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)

# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 5/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) TURNING TWEETS INTO KNOWLEDGE: AN INTRODUCTION TO TEXT ANALYTICS
# -------------------------------------------------------------------

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

# Video 5
# Read in the data
tweets <- read.csv(paste0(folder, "tweets.csv"), stringsAsFactors=FALSE)
str(tweets)
# Create dependent variable
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
# Install new packages
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)
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

# Video 6
# Create matrix
frequencies <- DocumentTermMatrix(corpus)
frequencies
# Look at matrix 
inspect(frequencies[1000:1005,505:515]) # Lots of zeroes, a very sparse matrix
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
library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)

# Video 7
# Build a CART model
library(rpart)
library(rpart.plot)
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
library(caret)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Negative ~ ., data=trainSparse,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.03
corpusCART <- rpart(Negative ~ ., data=trainSparse, method="class", cp=cp.value)

# Random forest model
library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data=trainSparse)
# Make predictions:
predictRF <- predict(tweetRF, newdata=testSparse)
result <- table(testSparse$Negative, predictRF)
result
# Compute accuracy for RF
sum(diag(result)) / sum(result)
# Small improvement over CART model. The CART model could therefore still be preferable because of its interpretability!
# By tuning the cp parameter through cross-validation in the CART model, we could probably improve the CART model too.




# Website "tweetfeel" (http://web.peanutlabs.com/) gives real-time analytics result on Twitter data, from any term

# Good advice on text analytics:
# - Select the special features that are SPECIFIC to the problem
# - Applying problem specific knowledge to the text corpus can give better results
#   (number of words, length of sentences, all caps usage, special symbols, tags, links, etc.)


# 2) MAN VS. MACHINE: HOW IBM BUILT A JEOPARDY CHAMPION
# -----------------------------------------------------
# How Watson works:
# Step 1: Question Analysis
# - Figure out what the question is looking for (Trying to find the Lexical Answer Type (LAT) of the question.
#   The LAT is the word or noun in the question that specifies the type of answer.
# Step 2: Hypothesis Generation
# - Search information sources for possible answers (ex. all planet names if the question asks for a planet)
# Step 3: Scoring Hypotheses
# - Compute confidence levels for each answer:
#   1. Stage: What is the likelihood that a candidate answer is an instance of a LAT?
#   2. Stage: Passage Search, which gives the best return/number of occurences that matches other words in the question?
# Step 4: Final Ranking
# - Look for a highly supported answer 
#   NOTE: Multiple candidate answers might be equal, so needs to be combined/averaged: "Abraham Lincoln" and "Honest Abe" are equal
#   Rank the hypotheses and rank them using predictive analytics. Watson uses Logistic Regression.


# 3) PREDICTIVE CODING: BRINGING TEXT ANALYTICS TO THE COURTROOM (Recitation)
# ---------------------------------------------------------------------------

emails = read.csv(paste0(folder, "energy_bids.csv"), stringsAsFactors=FALSE)
# Data from: http://trec-legal.umiacs.umd.edu/
str(emails)
# Email: text of message
# Responsive: Does email relate to energy schedules or bids?
emails$email[1]
strwrap(emails$email[1]) # Better display of looong content...
emails$responsive[1] # How do we find documents relevant to a lawsuit? "Responsive documents", in legal parlance
emails$email[2]
emails$responsive[2]
strwrap(emails$email[2]) # Better display of looong content...
# Responsive emails
table(emails$responsive)
# Video 3
# Load tm package
library(tm)
# Create corpus
corpus = Corpus(VectorSource(emails$email))
corpus[[1]]
# Pre-process data
corpus = tm_map(corpus, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after
# this video was recorded.)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
# Look at first email
corpus[[1]]
# Video 4
# Create matrix
dtm = DocumentTermMatrix(corpus)
dtm
str(dtm) # Lots of terms...
# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm
# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))
# Add in the outcome variable
labeledTerms$responsive = emails$responsive
str(labeledTerms)
# Video 5
# Split the data
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)
# Build a CART model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~ ., data=train, method="class")
prp(emailCART) # The "jeff" node refers to Enron CEO Jeff Skillings, who was jailed for fraud
# Video 6
# Make predictions on the test set
pred = predict(emailCART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]
# Compute accuracy
table(test$responsive, pred.prob >= 0.5)
(195+25)/(195+25+17+20)
# Baseline model accuracy (that document is always non-responsive)
table(test$responsive)
215/(215+42)
# Video 7
# ROC curve
# The cost for False Negatives (predicted false when it was true) is greater than False Positives, as these
# will be cases that are completely lost on a later manual review by humans. Manual review would only look at 
# cases that where labeled as Positives (1), to weed out the ones that were in fact Non-Posotives.

#          | Predicted=0          | Predicted=1          |
# ---------+----------------------+----------------------+
# Actual=0 | True Negatives (TN)  | False Positives (FP) |  
# Actual=1 | False Negatives (FN) | True Positives (TP)  |

library(ROCR)
par(mar=c(3,3,2,2))
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on Email", lwd=3)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
# NOTE: At a cutoff of 0.6-0.8, we predict a good TP rate, while at the same time having a low FP rate.
par(mar=c(3,3,2,1))
# Compute AUC
performance(predROCR, "auc")@y.values
sn <- slotNames(predROCR)
sapply(sn, function(x) length(slot(predROCR, x)))

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (TURNING TWEETS INTO KNOWLEDGE: AN INTRODUCTION TO TEXT ANALYTICS)
# ------------------------------------------------------------------------------------

# 1) Which of these problems is the LEAST likely to be a good application of natural language processing?
# Answer: Judging the winner of a poetry contest

# 2) Which of the three alternative metrics (Majority Score, Median Score, Minimum Score) below would best capture
# the typical opinion of the five Amazon Mechanical Turk workers, would be less affected by mistakes, and is
# well-defined regardless of the five labels?
# Anwer: An overall score equal to the median (middle) score
# Explanation:
# The correct answer is the first one - the median would capture the typical opinion of the workers and tends to be
# less affected by significant mistakes. The majority score might not have given a score to all tweets because they
# might not all have a majority score (consider a tweet with scores 0, 0, 1, 1, and 2). The minimum score does not
# necessarily capture the typical opinion and could be highly affected by mistakes (consider a tweet with scores
# -2, 1, 1, 1, 1).

# 3) Change sentence from: "Data is useful AND powerful!"
#    1) To: Data useful powerful!
#       Answer: Removing stop words
#    2) To: data is useful and powerful
#       Answer: Cleaning up irregularities (changing to lowercase and removing punctuation)
#    3) Data is use AND power!
#       Answer: Stemming

# 4) Given a corpus in R, how many commands do you need to run in R to clean up the irregularities
# (removing capital letters and punctuation)? Answer: 2
# How many commands do you need to run to stem the document? Answer: 1

# 5) Which of the following words appear at least 100 times? Answer: iphon, itun, new
findFreqTerms(frequencies, lowfreq=100) # The minimum number of times a term must appear in the matrix

# 6) GLM accuracy: 0.7521
tweetLog <- glm(Negative ~ ., data=trainSparse, family=binomial)
summary(tweetLog)
predictions = predict(tweetLog, newdata=testSparse, type="response")
predictionsTrain = predict(tweetLog, type="response")
cm <- table(testSparse$Negative, predictions > 0.5)
cm
sum(diag(cm)) / nrow(testSparse)
cm2 <- table(trainSparse$Negative, predictionsTrain > 0.5)
cm2
sum(diag(cm2)) / nrow(trainSparse) # An excellent result on train set, but not so on test set. Overfitting!
# Explanation: Note that you might have gotten a different answer than edX, because the glm function struggles with this
# many variables. The warning messages that you have seen in this problem have to do with the number of variables,
# and the fact that the model is overfitting to the training set.

# Quick questions 2 (MAN VS. MACHINE: HOW IBM BUILT A JEOPARDY CHAMPION)
# ----------------------------------------------------------------------

# 1) What were the goals of IBM when they set out to build Watson? Select all that apply.
# Answer: To build a computer that could compete with the best human players at Jeopardy!.
#         To build a computer that could answer questions that are commonly believed to require human intelligence.

# 2) For which of the following reasons is Jeopardy! challenging?
# Answer: A wide variety of categories
#         Speed is required - you have to buzz in faster than your competitors
#         The categories and clues are often cryptic

# 3) Which of the following two questions do you think would be EASIEST for a computer to answer?
#    Was Abraham Lincoln generally considered a happy man?
#    What year was Abraham Lincoln born? Answer: EASIEST

# 4) Find the LAT of these questions:
# NICHOLAS II WAS THE LAST RULING CZAR OF THIS ROYAL FAMILY (Hint: The answer is "The Romanovs")
# Answer: The LAT is: THIS ROYAL FAMILY THIS ROYAL FAMILY
# REGARDING THIS DEVICE, ARCHIMEDES SAID, "GIVE ME A PLACE TO STAND ON, AND I WILL MOVE THE EARTH" (Hint: The answer is "A lever")
# Answer: The LAT is: THIS DEVICE

# 5) To predict which candidate answer is correct, we said that Watson uses logistic regression.
#    Which of the other techniques that we have learned could be used instead?
# Answer: CART and Random Forests

# ---------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK:

# 1) DETECTING VANDALISM ON WIKIPEDIA
# -----------------------------------
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/78a8f19e5e54432a938ae62dc0246780/ac54e7a579bd44258ab0b446ac8091c4/
# The data for this problem is based on the revision history of the page Language (http://en.wikipedia.org/wiki/Language)
wiki <- read.csv(paste0(folder, "wiki.csv"), stringsAsFactors=F)
str(wiki)
# Vandal = 1 if this edit was vandalism, 0 if not.
# Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
# Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they did not.
# Added = The unique words added.
# Removed = The unique words removed.
wiki$Vandal = as.factor(wiki$Vandal)

# PROBLEM 1.1 - BAGS OF WORDS. Answer: 1815 cases of page vandalism 
table(wiki$Vandal)

# PROBLEM 1.2 - BAGS OF WORDS. How many terms appear in dtmAdded? Answer: 6675 terms
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after
# this video was recorded.)
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
length(stopwords("english")) # Should be 174
corpusAdded[[1]]
# Video 4
# Create matrix
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

# PROBLEM 1.3 - BAGS OF WORDS. Answer: terms in sparseAdded: 166
# Remove sparse terms
findFreqTerms(dtmAdded, lowfreq=20) # The minimum number of times a term must appear in the matrix
sparseAdded <- removeSparseTerms(dtmAdded, 0.997) # NOTE: 0.3 % of the revisions. So a value of 0 here is 100%, 1 is 0%
sparseAdded
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# PROBLEM 1.4 - BAGS OF WORDS. Answer: 162 words in wordsRemoved data frame
corpusRemoved <- Corpus(VectorSource(wiki$Removed)) # NOTE: Remember to take the Removed column!
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after
# this video was recorded.)
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
length(stopwords("english")) # Should be 174
corpusRemoved[[1]]
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997) # NOTE: 0.3 % of the revisions. So a value of 0 here is 100%, 1 is 0%
sparseRemoved
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# PROBLEM 1.5 - BAGS OF WORDS. Answer: Baseline accuracy on test set (Not Vandalism): 0.5314
wikiWords <- cbind(wordsAdded, wordsRemoved)
dim(wikiWords)
dim(wordsRemoved)
dim(wordsAdded)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiWordsTrain <- subset(wikiWords, split==TRUE)
wikiWordsTest <- subset(wikiWords, split==FALSE)
table(wikiWordsTest$Vandal)[1] / nrow(wikiWordsTest)

# PROBLEM 1.6 - BAGS OF WORDS. Answer: Accuracy on test set for CART prediction: 0.5426
# Remember that if you add the argument type="class" when making predictions, the output of predict will automatically
# use a threshold of 0.5.
wikiWordsCART <- rpart(Vandal ~ ., data=wikiWords, method="class")
prp(wikiWordsCART, cex=.8, col="blue", main="WikiWords")
# Evaluate the performance of the model
predictCART <- predict(wikiWordsCART, newdata=wikiWordsTest, type="class")
result <- table(wikiWordsTest$Vandal, predictCART)
result
# Compute accuracy
sum(diag(result)) / sum(result)

# PROBLEM 1.7 - BAGS OF WORDS. Answer: 2 stems on PRP plot
prp(wikiWordsCART, cex=.8, col="blue", main="WikiWords")

# PROBLEM 1.8 - BAGS OF WORDS. Answer:
# Explanation: There is no reason to think there was anything wrong with the split. CART did not overfit,
# which you can check by computing the accuracy of the model on the training set. Over-sparsification is
# plausible but unlikely, since we selected a very high sparsity parameter. The only conclusion left is simply
# that bag of words didn't work very well in this case. 

# PROBLEM 2.1 - PROBLEM-SPECIFIC KNOWLEDGE. Answer: Links: 217
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed=TRUE), 1, 0)
# Check for http as part of words, revealing links/URLs to promotional sites as typical sign of vandalism
table(wikiWords2$HTTP)

# PROBLEM 2.2 - PROBLEM-SPECIFIC KNOWLEDGE. Answer: Accuracy on test set is 0.5727
# Use the split var created earlier:
wikiTrain2 = subset(wikiWords2, split==TRUE) # Remember to use 'split' var created earlier and not 'spl' from previous homework!
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiWordsCART2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiWordsCART2, cex=.8, col="blue", main="WikiWords")
# Evaluate the performance of the model
predictCART2 <- predict(wikiWordsCART2, newdata=wikiTest2, type="class")
result <- table(wikiTest2$Vandal, predictCART2)
result
# Compute accuracy, better result now, CART tree splits on HTTP yes/no as first split
sum(diag(result)) / sum(result)

# PROBLEM 2.3 - PROBLEM-SPECIFIC KNOWLEDGE. Answer: Mean number of words added: 4.05
# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2:
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# PROBLEM 2.4 - PROBLEM-SPECIFIC KNOWLEDGE. Answer: Accuracy on test set: 0.6552
wikiTrain2 = subset(wikiWords2, split==TRUE) # Remember to use 'split' var created earlier and not 'spl' from previous homework!
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiWordsCART2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiWordsCART2, cex=.8, col="blue", main="WikiWords")
# Evaluate the performance of the model
predictCART2 <- predict(wikiWordsCART2, newdata=wikiTest2, type="class")
result <- table(wikiTest2$Vandal, predictCART2)
result
# Compute accuracy, better result now, CART tree splits on HTTP yes/no as first split
sum(diag(result)) / sum(result)

# PROBLEM 3.1 - USING NON-TEXTUAL DATA. Answer: Accuracy on test set: 0.7188
wikiWords3 = wikiWords2
# Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, split==TRUE) # Remember to use 'split' var created earlier and not 'spl' from previous homework!
wikiTest3 = subset(wikiWords3, split==FALSE)
wikiWordsCART3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiWordsCART3, cex=.8, col="blue", main="WikiWords")
# Evaluate the performance of the model
predictCART3 <- predict(wikiWordsCART3, newdata=wikiTest3, type="class")
result <- table(wikiTest3$Vandal, predictCART3)
result
# Compute accuracy, better result now, CART tree splits on HTTP yes/no as first split
sum(diag(result)) / sum(result)

# PROBLEM 3.2 - USING NON-TEXTUAL DATA. Answer: 3 splits on the CART tree
# By adding new independent variables, we were able to significantly improve our accuracy without making the
# model more complicated!

# 2) AUTOMATING REVIEWS IN MEDICINE
# ---------------------------------

# IMPORTANT NOTE: Some students have been getting errors like "invalid multibyte string" when performing certain parts
# of this homework question. If this is happening to you, use the argument fileEncoding="latin1" when reading in the file
# with read.csv. This should cause those errors to go away.

# Data from: http://www.ncbi.nlm.nih.gov/pubmed
trials <- read.csv(paste0(folder, "clinical_trial.csv"), stringsAsFactors=F)
str(trials)
summary(trials)
table(trials$trial)

# PROBLEM 1.1 - LOADING THE DATA. Answer: 3708
max(nchar(trials$abstract))

# PROBLEM 1.2 - LOADING THE DATA. Answer: 112
table(nchar(trials$abstract) == 0)

# PROBLEM 1.3 - LOADING THE DATA. Answer: A decade of letrozole: FACE.
trials$title[which(nchar(trials$title) == min(nchar(trials$title)))]

# PROBLEM 2.1 - PREPARING THE CORPUS. Answer: 31 terms in dtmTitle, 335 terms in dtmAbstract
corpusTitle <- trials$title
corpusAbstract <- trials$abstract
corpusTitle <- Corpus(VectorSource(corpusTitle))
corpusAbstract <- Corpus(VectorSource(corpusAbstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
length(stopwords("english")) # Should be 174
corpusTitle[[1]]
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmTitle
sparseTitle <- removeSparseTerms(dtmTitle, 0.95) # NOTE: 0.5 % of the revisions. So a value of 0 here is 100%, 1 is 0%
sparseTitle

corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
length(stopwords("english")) # Should be 174
corpusAbstract[[1]]
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmAbstract
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95) # NOTE: 0.5 % of the revisions. So a value of 0 here is 100%, 1 is 0%
sparseAbstract

dtmTitle <- as.data.frame(as.matrix(sparseTitle))
dtmAbstract <- as.data.frame(as.matrix(sparseAbstract))
rownames(dtmTitle) <- NULL
rownames(dtmAbstract) <- NULL
dim(dtmTitle)
dim(dtmAbstract)

# PROBLEM 2.2 - PREPARING THE CORPUS. Answer: Abstracts tend to have many more words than titles

# PROBLEM 2.3 - PREPARING THE CORPUS. Answer: Most frequenct word: patient
names(dtmAbstract)[colSums(dtmAbstract) == max(colSums(dtmAbstract))]

# PROBLEM 3.1 - BUILDING A MODEL. Answer: The effect was: Adding the letter T in front of all the title variable names
# and adding the letter A in front of all the abstract variable names.
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))

# PROBLEM 3.2 - BUILDING A MODEL. Answer: 367 columns in combined data frame "dtm"
dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
ncol(dtm)

# PROBLEM 3.3 - BUILDING A MODEL. Answer: Accuracy on train set (the most frequenct outcome): 0.5607
library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
trainDtm <- subset(dtm, split==TRUE)
testDtm <- subset(dtm, split==FALSE)
table(trainDtm$trial)[1] / nrow(trainDtm)

# PROBLEM 3.4 - BUILDING A MODEL. Answer: "Tphase" is first variable the model is split on
trialCART <- rpart(trial ~ ., data=trainDtm, method="class")
prp(trialCART, cex=.8, col="blue", main="Clinical Trials")
# Evaluate the performance of the model

# PROBLEM 3.5 - BUILDING A MODEL. Answer: Max prob. of result being a trial (1) is: 0.8719
predictCART1 <- predict(trialCART) # Predict probs on train set
head(predictCART1)
max(predictCART1[,2])

# PROBLEM 3.6 - BUILDING A MODEL. Answer: The maximum predicted probability will likely be exactly the same
# in the testing set. (And it is: 0.8719)
predictCART2 <- predict(trialCART, newdata=testDtm)
head(predictCART2)
max(predictCART2[,2])

# PROBLEM 3.7 - BUILDING A MODEL. Answer: Accuracy: 0.8233 Sensitivity: 0.771 specificity: 0.8644
predictCART1 <- predict(trialCART) # Predict probs on train set
head(predictCART1)
result <- table(trainDtm$trial, predictCART1[,2] > 0.5)
result
accuracy <- sum(diag(result)) / nrow(trainDtm)
accuracy
#          | Predicted=0          | Predicted=1          |
# ---------+----------------------+----------------------+
# Actual=0 | True Negatives (TN)  | False Positives (FP) |  
# Actual=1 | False Negatives (FN) | True Positives (TP)  |
# Sensitivity: TP / (TP + FN) (a.k.a. "The true positive rate", that is, all the positives in the dataset)
# Specificity: TN / (TN + FP) (a.k.a. "The true negative rate", the actual good care cases (0) that we classified correctly)
sensitivity <- (result[4] / (result[2] + result[4]))
specificity <- (result[1] / (result[1] + result[3]))
sensitivity
specificity

# PROBLEM 4.1 - EVALUATING THE MODEL ON THE TESTING SET. Answer: Accuracy on test set: 0.7581
predictCART3 <- predict(trialCART, newdata=testDtm)
head(predictCART3)
max(predictCART3[,2])
result <- table(testDtm$trial, predictCART3[,2] > 0.5)
result
accuracy <- sum(diag(result)) / nrow(testDtm)
accuracy

# PROBLEM 4.2 - EVALUATING THE MODEL ON THE TESTING SET. Answer: AUC for test set: 0.8371
library(ROCR)
ROCRpred <- prediction(predictCART3[,2], testDtm$trial)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PART 5.1 - DECISION-MAKER TRADEOFFS. Answer: See below.
# What is the cost associated with the model in Step 1 making a false negative prediction?
# Answer: A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3. 

# PART 5.2 - DECISION-MAKER TRADEOFFS. Answer: See below.
# What is the cost associated with the model in Step 1 making a false positive prediction?
# A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the
# quality of the results of Step 3. 

# PART 5.3 - DECISION-MAKER TRADEOFFS. Answer: See below.
# Given the costs associated with false positives and false negatives, which of the following is most accurate?
# A false negative is more costly than a false positive; the decision maker should use a probability threshold
# LESS than 0.5 for the machine learning model.
# Compare:
result <- table(trainDtm$trial, predictCART1[,2] > 0.1)
result
# to:
result <- table(trainDtm$trial, predictCART1[,2] > 0.5)
result

# 3) SEPARATING SPAM FROM HAM (PART 1)
# ------------------------------------

emails <- read.csv(paste0(folder, "emails.csv"), stringsAsFactors=F)
str(emails)
summary(emails)

# PROBLEM 1.1 - LOADING THE DATASET. Answer: 5728 emails in dataset
nrow(emails)

# PROBLEM 1.2 - LOADING THE DATASET. Answer: 1368 emails are spam
table(emails$spam)

# PROBLEM 1.3 - LOADING THE DATASET. Answer: The word "subject" appears as first word in every email
emails$text[1]

# PROBLEM 1.4 - LOADING THE DATASET
# Could a spam classifier potentially benefit from including the frequency of the word that appears in every email?
# Answer: Yes -- the number of times the word appears might help us differentiate spam from ham. 

# PROBLEM 1.5 - LOADING THE DATASET. Answer: Longest email: 43952 chars
max(nchar(emails$text))

# PROBLEM 1.6 - LOADING THE DATASET. Answer: Row 1992
# Which row contains the shortest email in the dataset?
which(nchar(emails$text) == min(nchar(emails$text)))
emails$text[which(nchar(emails$text) == min(nchar(emails$text)))]

# PROBLEM 2.1 - PREPARING THE CORPUS. Answer: Terms in dtm: 28687
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
length(stopwords("english")) # Should be 174
corpus[[1]]
dtm <- DocumentTermMatrix(corpus)
dtm

# PROBLEM 2.2 - PREPARING THE CORPUS. Answer: Terms in spdtm: 330
spdtm <- removeSparseTerms(dtm, 0.95) # NOTE: 0.5 % of the revisions. So a value of 0 here is 100%, 1 is 0%
spdtm

# PROBLEM 2.3 - PREPARING THE CORPUS. Answer: Most frequenct word (col name): enron
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

# PROBLEM 2.4 - PREPARING THE CORPUS. Answer: 6 word stems appear at least 5000 times in the ham emails
emailsSparse$spam <- emails$spam
which(colSums(emailsSparse[emailsSparse$spam == 0, -ncol(emailsSparse)]) >= 5000)

# PROBLEM 2.5 - PREPARING THE CORPUS. Answer: 3 word stems appear at least 1000 times in the spam emails
which(colSums(emailsSparse[emailsSparse$spam == 1, -ncol(emailsSparse)]) >= 1000)

# PROBLEM 2.6 - PREPARING THE CORPUS.
# The lists of most common words are significantly different between the spam and ham emails. What does this likely imply?
# Answer: The frequencies of these most common words are likely to help differentiate between spam and ham. 

# PROBLEM 2.7 - PREPARING THE CORPUS. Answer:
# The ham dataset is certainly personalized to Vincent Kaminski, and therefore it might not generalize well to a general
# email user. Caution is definitely necessary before applying the filters derived in this problem to other email users.

# PROBLEM 3.1 - BUILDING MACHINE LEARNING MODELS. Answer: 3046, 954, 10
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
trainEmails <- subset(emailsSparse, split==TRUE)
testEmails <- subset(emailsSparse, split==FALSE)
table(trainEmails$spam)[1] / nrow(trainEmails)
spamLog <- glm(spam ~., data=trainEmails, family=binomial(link="logit"))
summary(spamLog)
spamCART <- rpart(spam ~., data=trainEmails, method="class")
summary(spamCART)
prp(spamCART, cex=.8, col="blue")
set.seed(123)
spamRF <- randomForest(spam ~., data=trainEmails)
summary(spamRF)
# Predict on training set:
predLOG <- predict(spamLog, type="response")
predCART <- predict(spamCART)
predRF <- predict(spamRF, type="prob")
length(which(predLOG < 0.00001)) # 3046
length(which(predLOG > 0.99999)) # 954
length(which(predLOG > 0.00001 & predLOG < 0.99999)) # 10

# PROBLEM 3.2 - BUILDING MACHINE LEARNING MODELS. Answer: 0 variables labeled as significant p=0.05)
summary(spamLog)

# PROBLEM 3.3 - BUILDING MACHINE LEARNING MODELS. Answer: vinc: 1, enron: 1 = 2 variables appear in the cart tree
# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree? 
prp(spamCART, cex=.8, col="blue")

# PROBLEM 3.4 - BUILDING MACHINE LEARNING MODELS. Answer: Accuracy on train set: 0.999
# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
result <- table(trainEmails$spam, predLOG > 0.5) # Whoa!!! What accuracy with good ol' GLM!
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 3.5 - BUILDING MACHINE LEARNING MODELS. Answer: AUC=1
# What is the training set AUC of spamLog?
library(ROCR)
pred <- prediction(predLOG, trainEmails$spam) # PredictROC[,2] = outcome 1 column
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 3.6 - BUILDING MACHINE LEARNING MODELS. Answer: Accuracy on train set: 0.9424
# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?
result <- table(trainEmails$spam, predCART[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 3.7 - BUILDING MACHINE LEARNING MODELS. Answer: AUC on train set: 0.9696
pred <- prediction(predCART[,2], trainEmails$spam) # PredictROC[,2] = outcome 1 column
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 3.8 - BUILDING MACHINE LEARNING MODELS. Answer: Accuracy on train set: 0.9793
result <- table(trainEmails$spam, predRF[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 3.9 - BUILDING MACHINE LEARNING MODELS. Answer: AUC on train set: 0.9979
pred <- prediction(predRF[,2], trainEmails$spam) # PredictROC[,2] = outcome 1 column
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 3.10 - BUILDING MACHINE LEARNING MODELS. Answer: Logistic Regression performs best

# PROBLEM 4.1 - EVALUATING ON THE TEST SET. Answer: GLM accuracy on test set: 0.9511
predLOGtest <- predict(spamLog, newdata=testEmails) # PredictROC[,2] = outcome 1 column
predCARTtest <- predict(spamCART, newdata=testEmails) # PredictROC[,2] = outcome 1 column
predRFtest <- predict(spamRF, newdata=testEmails, type="prob") # PredictROC[,2] = outcome 1 column
result <- table(testEmails$spam, predLOGtest > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 4.2 - EVALUATING ON THE TEST SET. Answer: AUC on test set: 0.9768
pred <- prediction(predLOGtest, testEmails$spam) # PredictROC[,2] = outcome 1 column
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 4.3 - EVALUATING ON THE TEST SET. Answer: Accuracy on test set: 0.9395
result <- table(testEmails$spam, predCARTtest[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 4.4 - EVALUATING ON THE TEST SET. Answer: AUC on test set: 0.9632
pred <- prediction(predCARTtest[,2], testEmails$spam)
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 4.5 - EVALUATING ON THE TEST SET. Answer: Accuracy on test set: 0.975
result <- table(testEmails$spam, predRFtest[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 4.6 - EVALUATING ON THE TEST SET. Answer: AUC on test set: 0.9976
pred <- prediction(predRFtest[,2], testEmails$spam)
perf <- performance(pred, "tpr","fpr")
plot(perf, col="blue")
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 4.7 - EVALUATING ON THE TEST SET. Answer: RF had best accuracy on test set

# PROBLEM 4.8 - EVALUATING ON THE TEST SET. Answer: GLM model
# Which model demonstrated the greatest degree of overfitting?

# 4) SEPARATING SPAM FROM HAM (PART 2 - OPTIONAL)
# -----------------------------------------------

# PROBLEM 5.1 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: See below.
# Consider the case of an email provider using the spam filter we have developed. The email provider moves all of
# the emails flagged as spam to a separate "Junk Email" folder, meaning those emails are not displayed in the main
# inbox. The emails not flagged as spam by the algorithm are displayed in the inbox. Many of this provider's email
# users never check the spam folder, so they will never see emails delivered there.

#          | Predicted=0          | Predicted=1          |
# ---------+----------------------+----------------------+
# Actual=0 | True Negatives (TN)  | False Positives (FP) |  
# Actual=1 | False Negatives (FN) | True Positives (TP)  |

# In this scenario, what is the cost associated with the model making a false negative error?
# Answer: A false negative means the model labels a spam email as ham.
# This results in a spam email being displayed in the main inbox.

# In this scenario, what is the cost associated with our model making a false positive error?
# Answer: A false positive means the model labels a ham email as spam.
# This results in a ham email being sent to the Junk Email folder.

# PROBLEM 5.2 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: A FP is most costly

# PROBLEM 5.3 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: See below.
# What sort of user might assign a particularly high cost to a false negative result?
# Answer: A user who is particularly annoyed by spam email reaching their main inbox

# PROBLEM 5.4 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: See below.
# What sort of user might assign a particularly high cost to a false positive result?
# Answer: A user who never checks his/her Junk Email folder

# PROBLEM 5.5 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: See below.
# Consider another use case for the spam filter, in which messages labeled as spam are still delivered to the main
# inbox but are flagged as "potential spam." Therefore, there is no risk of the email user missing an email regardless
# of whether it is flagged as spam. What is the largest way in which this change in spam filter design affects the
# costs of false negative and false positive results?
# Answer: The cost of false positive results is decreased (ham erroneously ending up in spam filter) 

# PROBLEM 5.6 - ASSIGNING WEIGHTS TO DIFFERENT TYPES OF ERRORS. Answer: See below.
# Consider a large-scale email provider with more than 100,000 customers. Which of the following represents an approach
# for approximating each customer's preferences between a false positive and false negative that is both practical
# and personalized?
# Answer: Automatically collect information about how often each user accesses his/her Junk Email folder to infer preferences

# PROBLEM 6.1 - INTEGRATING WORD COUNT INFORMATION. Answer: See below.
# What would have occurred if we had instead created wordCount using spdtm instead of dtm?
# Answer: wordCount would have only counted some of the words, but would have returned a result for all the emails
dtm
wordCount = rowSums(as.matrix(dtm))
nrow(dtm)
nrow(spdtm) # Same, so all the emails (rows) are here too

# PROBLEM 6.2 - INTEGRATING WORD COUNT INFORMATION. Answer: The data is skew right -- there are a large number of
# small wordCount values and a small number of large values. 
hist(wordCount, main="Word count of dtm", col="cornflowerblue")

# PROBLEM 6.3 - INTEGRATING WORD COUNT INFORMATION. Answer: The data is not skewed -- there are roughly the same number
# of unusually large and unusually small log(wordCount) values. 
hist(log(wordCount), main="log(Word count) of dtm", col="cornflowerblue") # Very normal distribution from log()!

# PROBLEM 6.4 - INTEGRATING WORD COUNT INFORMATION. Answer: See below.
# Answer: We can see that the 1st quartile, median, and 3rd quartiles are all slightly lower for spam messages than for ham messages.
wordCountLog = log(rowSums(emailsSparse[,-ncol(emailsSparse)]))
emailsSparse$logWordCount <- wordCountLog
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam, col=rgb(.55, .45, .90), main="Log(wordcount) on ham(0) / spam(1)")

# PROBLEM 6.5 - INTEGRATING WORD COUNT INFORMATION. Answer: Yes, the new variable logWordCount was used.
# Was the new variable used in the new CART tree spam2CART?
table(split)
trainEmails2 <- subset(emailsSparse, split==TRUE)
testEmails2 <- subset(emailsSparse, split==FALSE)
spam2CART <- rpart(spam ~., data=trainEmails2)
set.seed(123)
spam2RF <- randomForest(spam ~., data=trainEmails2)
prp(spam2CART, cex=.8, col="blue", main="spam2CART")

# PROBLEM 6.6 - INTEGRATING WORD COUNT INFORMATION. Answer: Test set accuracy is: 0.9232
predCART2 <- predict(spam2CART, newdata=testEmails2)
result <- table(testEmails2$spam, predCART2[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 6.7 - INTEGRATING WORD COUNT INFORMATION. Answer: AUC on test set is: 0.9573
predROCR = prediction(predCART2[,2], testEmails2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on Email", lwd=3)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
# NOTE: At a cutoff of 0.95, we predict a good TP rate, while at the same time having a low FP rate.
# Compute AUC
performance(predROCR, "auc")@y.values

# PROBLEM 6.8 - INTEGRATING WORD COUNT INFORMATION. Answer: Test set accuracy is: 0.976
predRF2 <- predict(spam2RF, newdata=testEmails2, type="prob")
result <- table(testEmails2$spam, predRF2[,2] > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 6.9 - INTEGRATING WORD COUNT INFORMATION. Answer: AUC on test set is: 0.9945
predROCR = prediction(predRF2[,2], testEmails2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, main="ROCR on Email", lwd=3)
# TODO: Add text
# http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
# NOTE: At a cutoff of 0.995, we predict a good TP rate, while at the same time having a low FP rate.
# Compute AUC
performance(predROCR, "auc")@y.values
# NOTE: In this case, adding the logWordCounts variable did not result in improved results on the test set for the
# CART or random forest model (See result in previous homework without the wordCountLog variable added).
