# Sentiment Analysis on Movie Reviews
# -----------------------------------
# Ends: 11:59 pm, Saturday 28 February 2015 UTC (365 total days)
# http://www.kaggle.com/c/sentiment-analysis-on-movie-reviews
# http://www.kaggle.com/c/sentiment-analysis-on-movie-reviews/data

# TIPS:
# http://yukatherin.github.io/ (Katherine Yu, interesting!)
# https://github.com/rafacarrascosa/samr (0.65 on the LB)
# http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
# https://github.com/tavishsrivastava/Sentiment-analysis-FIFA/blob/master/fifa_pred.R
# http://www.r-bloggers.com/qdap-1-3-1-release-demoing-dispersion-plots-sentiment-analysis-easy-hash-lookups-boolean-searches-and-more/
# http://stats.stackexchange.com/questions/13658/text-classification-in-r?rq=1
# http://en.wikipedia.org/wiki/Kernel_methods
# http://www.jstatsoft.org/v25/i05/paper
# http://cran.r-project.org/web/views/NaturalLanguageProcessing.html
# http://streamhacker.com/2010/05/10/text-classification-sentiment-analysis-naive-bayes-classifier/
# http://www.jstatsoft.org/v25/i05/paper
# http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# http://stackoverflow.com/questions/7927367/r-text-file-and-text-mining-how-to-load-data
# http://web.letras.up.pt/bhsmaia/EDV/apresentacoes/Bradzil_Classif_withTM.pdf (obsolete?)
# http://statmath.wu.ac.at/courses/SNLP/Presentations/Unit_4-Classification.pdf
# http://mlwave.com/movie-review-sentiment-analysis-with-vowpal-wabbit/
# http://web.letras.up.pt/bhsmaia/EDV/apresentacoes/Bradzil_Classif_withTM.pdf
# https://github.com/tlfvincent/StatOfMind/blob/master/Sentiment_Analysis_of_TV_shows/misc.R
# http://www.rexamine.com/2014/06/text-mining-in-r-automatic-categorization-of-wikipedia-articles/
# Logistic regression on bigrams with decent hyperparameter setting is good for around 0.615.
# http://bommaritollc.com/2011/02/16/pre-processing-text-rtm-vs-pythonnltk/
# Bigrams: https://gist.github.com/corynissen/8935664
# https://github.com/rafacarrascosa/samr (Python code for 0.65 LB score)

# Search for: Sentiment Analysis in R
# Naive Bayes classifier

# Multi-class classification (multinomial)?
# http://stackoverflow.com/questions/15585501/usage-of-caret-with-gbm-method-for-multiclass-classification
# http://www.kaggle.com/c/sentiment-analysis-on-movie-reviews/forums/t/7319/what-algorithms-should-i-consider-for-this-project

# The dataset is comprised of tab-separated files with phrases from the Rotten Tomatoes dataset.
# The train/test split has been preserved for the purposes of benchmarking, but the sentences have been
# shuffled from their original order. Each Sentence has been parsed into many phrases by the Stanford parser.
# Each phrase has a PhraseId. Each sentence has a SentenceId. Phrases that are repeated (such as short/common words)
# are only included once in the data.

# train.tsv contains the phrases and their associated sentiment labels. We have additionally provided a SentenceId
# so that you can track which phrases belong to a single sentence.
# test.tsv contains just phrases. You must assign a sentiment label to each phrase.

# The sentiment labels are:
  
# 0 - negative
# 1 - somewhat negative
# 2 - neutral
# 3 - somewhat positive
# 4 - positive

# Use qdap package in addition to tm?

# From: https://gist.github.com/corynissen/8935664
bigrams <- function(text){
  word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec.length <- length(word.vec)
  lapply(1:(word.vec.length - 1), function(x) c(word.vec[x], word.vec[x + 1]))
}

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/SentimentAnalysisOnMovieReviews/data/"
codeFolder <- "C:/coding/Kaggle/SentimentAnalysisOnMovieReviews/code/"
submissionsFolder <- "C:/coding/Kaggle/SentimentAnalysisOnMovieReviews/submissions"

SetStandardOptions()

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.tsv"), header=T, sep="\t", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.tsv"), header=T, sep="\t", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

negwords <- read.table(paste0(dataFolder, "negwords.txt"), stringsAsFactors=F)
head(negwords)
poswords <- read.table(paste0(dataFolder, "poswords.txt"), stringsAsFactors=F)
head(poswords)
# See also: http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
#           http://andybromberg.com/sentiment-analysis/

head(train)
head(test)

dim(train)
dim(test)

package.install("tm")
library(tm)
# Use qdap package in addition to tm?
package.install("qdap")
library(qdap)
library(NLP) # Needed?

# -------------------------------------------
# Example with e1071, tm, etc.
# https://code.google.com/p/rtexttools/source/browse/NaiveBayes.R?r=c8ec81e0f0c7dd089b8b44e9be360ea4617fe9d8 

library('e1071');
library('SparseM');
library('tm');

# http://web.letras.up.pt/bhsmaia/EDV/apresentacoes/Bradzil_Classif_withTM.pdf
path <- "C:/coding/Kaggle/SentimentAnalysisOnMovieReviews/data/"

# LOAD DATA FROM CSV
data <- read.csv("C:/coding/Kaggle/SentimentAnalysisOnMovieReviews/data/train.tsv",
                 header=T, sep="\t", stringsAsFactors=F);
head(data)
barplot(table(data$Sentiment))

# -------------------------------------------

CreateCorpus <- function(data, threshold=0.99) {
  library(tm)
  # install.packages("SnowballC")
  library(SnowballC)
  # Create corpus
  corpus <- Corpus(VectorSource(data))
  # Look at corpus
  corpus
  corpus[[1]]
  corpus <- tm_map(corpus, stripWhitespace) # Eliminating extra white spaces
  # Convert to lower-case
  corpus <- tm_map(corpus, tolower)
  corpus[[1]]
  # IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line
  # before continuing (it converts corpus to a Plain Text Document). 
  corpus <- tm_map(corpus, PlainTextDocument)
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  corpus[[1]]
  # Look at stop words 
  # stopwords("english")[1:20]
  # stopwords("norwegian")[1:20]
  # Remove english stopwords (TODO: If removing other words, add with c(...))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # corpus <- tm_map(reuters, removeNumbers) # Not sure here...
  # Stem document 
  corpus <- tm_map(corpus, stemDocument)
  corpus[[1]]
  
  # Create DTM
  frequencies <- DocumentTermMatrix(corpus)
  frequencies
  # Look at matrix 
  inspect(frequencies[1000:1005,505:515]) # Lots of zeroes, a very sparse matrix
  # Check for sparsity
  findFreqTerms(frequencies, lowfreq=20) # The minimum number of times a term must appear in the matrix
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

# ------------------------------------------------------------------------------------------------------------------------------

# NEW TEST AFTER DEADLINE:
--------------------------
names(data)
# Get the average number of phrases in each sentence
dim(data)
table(data$SentenceId)
sort(table(data$SentenceId), decreasing=T)[1:20] # Max: 63 phrases in the longest sentences

result <- CreateTrainAndValidationSets(data)
train.subset <- result[[1]]
validation.subset <- result[[2]]
nrow.train <- nrow(train.subset)
nrow.validation <- nrow(validation.subset)

# Create a corpus on Phrase
result <- CreateCorpus(data$Phrase, 0.992) # Was: 0.987 (11 vars) Takes a looong time, so save result...
save(result, file=paste0(path, "corpus.rda"))
# result <- load(paste0(path, "corpus.rda"))
corpus <- result[[1]] # Get the sparse DTM as df 
dtm <- result[[2]] # Get the sparse DTM
dtm

corpus$Sentiment <- data$Sentiment
corpus.train <- head(corpus, n=nrow.train)
corpus.validation <- tail(corpus, n=nrow.validation)
dim(corpus.train)
dim(train.subset) # Should be equal

# Add outcome variable

model.rf <- randomForest(as.factor(Sentiment) ~ ., data=corpus.train, ntree=50)
predict.rf <- predict(model.rf, newdata=corpus.validation)
head(predict.rf)
table(as.integer(corpus.validation$Sentiment), as.integer(predict.rf))
result <- table(corpus.validation$Sentiment, predict.rf)
result
ConfusionMatrix(table(corpus.validation$Sentiment, predict.rf), labels=c(0:4))
accuracy <- sum(diag(result)) / sum(result)
accuracy

# ------------------------------------------------------------------------------------------------------------------------------

# Create data frames for train and test
traindata <- as.data.frame(data[1:750, c(3, 4)]);
testdata <- as.data.frame(data[751:1000, c(3, 4)]);

# TEST: Get negwords and poswords count in Phrase
found <- numeric(length(traindata$Phrase))
phrasecounter <- 0

for (phrasecounter in 1:length(traindata$Phrase)) {
  for (counter in 1:length(poswords$V1)) {
    if (gregexpr(poswords$V1[counter], traindata$Phrase[phrasecounter])[[1]] != -1)
      found[phrasecounter] <- found[phrasecounter] + 1
  }
  
  for (counter in 1:length(negwords$V1)) {
    if (gregexpr(negwords$V1[counter], traindata$Phrase[phrasecounter])[[1]] != -1)
      found[phrasecounter] <- found[phrasecounter] - 1
  }
  
  phrasecounter <- phrasecounter + 1
}

# ------------------------------------------------------------------------------------------------------------
# http://www.r-bloggers.com/clustering-the-words-of-william-shakespeare/
# http://www.rexamine.com/2014/06/text-mining-in-r-automatic-categorization-of-wikipedia-articles/

#package.install("stringi")
library(stringi)
library(proxy)

# Get a subset of the traindata to play with:
trainDataSubset <- traindata[1:100, ]

docs <- Corpus(VectorSource(trainDataSubset$Phrase))

docs[[1]]
docs2 <- tm_map(docs, content_transformer(function(x) stri_replace_all_regex(x, "<.+?>", " ")))
docs3 <- tm_map(docs2, content_transformer(function(x) stri_replace_all_fixed(x, "\t", " ")))
docs4 <- tm_map(docs3, PlainTextDocument)
docs5 <- tm_map(docs4, stripWhitespace)
docs6 <- tm_map(docs5, removeWords, stopwords("english"))
docs7 <- tm_map(docs6, removePunctuation)
docs8 <- tm_map(docs7, tolower)
docs8[[1]]

docsTDM <- TermDocumentMatrix(Corpus(VectorSource(docs8)))
inspect(docsTDM[1:10, 1:10])

library(cluster)
docdissim <- dist(scale(docsTDM))
h <- hclust(docsdissim, method="ward.D")
plot(h, which.plots=2, main="", sub="", xlab="")

# Small test:
# http://pyevolve.sourceforge.net/wordpress/?p=2497
documents = c(
  "The sky is blue",
  "The sun is bright",
  "The sun in the sky is bright",
  "We can see the shining sun the bright sun"
)
docsTDM2 <- TermDocumentMatrix(Corpus(VectorSource(documents)))
docdissim2 <- dist(scale(docsTDM2))
h2 <- hclust(docdissim2, method="ward.D2")
plot(h2, main="Dendrogram Test", sub="", xlab="")


# ------------------------------------------------------------------------------------------------------------


oldmar=par()$mar
par(mar=c(4.3,2,1,1))
# plot results to compare sentiment and poswords/negwords result:
plot(found[601:700], type="l", col="blue", ylim=c(-4, 4), lwd=1, cex.axis=.8)
lines(traindata$Sentiment[601:700]-2, col="red")
par(mar=oldmar)
# col <- Corpus(DirSource(path), readerControl=list(reader=readPlain, language="en", load=T))
# NOTE: Can also use:
corpus <- Corpus(VectorSource(traindata$Phrase), readerControl=list(reader=readPlain, language="en", load=T))
corpus <- tm_map(tm_map(tm_map(corpus, stripWhitespace), tolower), stemDocument)
tdm <- TermDocumentMatrix(corpus, control=list(tokenize="NGramTokenizer", removePunctuation=T, stopwords=T))
inspect(tdm[1:10,1])
df <- as.data.frame(inspect(tdm))

corpus <- Corpus(VectorSource(traindata$Phrase), readerControl=list(reader=readPlain, language="en", load=T))
dtm <- DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf, minWordLength=2, minDocFreq=5))
# Slow... save result to .rda?

freqterms2.tdm <- findFreqTerms(tdm, lowfreq=2)
freqterms2.tdm
freqterms40.dtm <- findFreqTerms(dtm, lowfreq=10, highfreq=50)
freqterms40.dtm[1:50] # Works! But returns way too much!

# TODO:
dt <- rpart(traindata$Sentiment ~ traindata$Phrase, data=traindata)
dt
dt.predictions.ts <- predict(dt, testdata$Phrase, type="class")
table(class.ts, dt.predictions.ts)

# Try SVM (error in variable lengths...):
Corpus.svm <- svm(traindata$Sentiment ~., data=df)

# Does not work...
library(nnet)
nnet.classifier <- nnet(traindata$Sentiment ~ traindata$Phrase, data=traindata, size=2, rang=0.1, decay=5e-4, maxit=200)
predictions <- predict(nnet.classifier, newdata=testdata$Phrase, type="class")

# SEPARATE TEXT VECTOR TO CREATE Source(),
# Corpus() CONSTRUCTOR FOR DOCUMENT TERM
# MATRIX TAKES Source()
trainvector <- as.vector(traindata$Phrase);
testvector <- as.vector(testdata$Phrase);

# DEBUGGING
is.vector(trainvector);
is.vector(testvector);

# CREATE SOURCE FOR VECTORS
trainsource <- VectorSource(trainvector);
testsource <- VectorSource(testvector);

# CREATE CORPUS FOR DATA
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)

# STEM WORDS, REMOVE STOPWORDS, TRIM WHITESPACE
traincorpus <- tm_map(traincorpus, stripWhitespace)
traincorpus <- tm_map(traincorpus, tolower)
system.time(
  traincorpus <- tm_map(traincorpus, removeWords, stopwords(kind="en"))
)

testcorpus <- tm_map(testcorpus, stripWhitespace)
testcorpus <- tm_map(testcorpus, tolower)
system.time(
  testcorpus <- tm_map(testcorpus, removeWords, stopwords(kind="en"))
)

# CREATE DOCUMENT TERM MATRIX
trainmatrix <- t(TermDocumentMatrix(traincorpus));
testmatrix <- t(TermDocumentMatrix(testcorpus));

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Sentiment CLASS VECTOR
model <- naiveBayes(as.matrix(trainmatrix), as.factor(traindata$Sentiment));

# PREDICTION
results <- predict(model, as.matrix(testmatrix));
trueresult <- (results == testdata$Sentiment)
length(trueresult[trueresult == TRUE])
table(results, testdata$Sentiment)

# Quick test, too simple, bad result...?
model <- naiveBayes(traindata, as.factor(traindata$Sentiment));
results <- predict(model, testdata[,-2]);
model <- naiveBayes(traindata$Phrase, as.factor(traindata$Sentiment));
results <- predict(model, testdata$Phrase);
trueresult <- (results == testdata$Sentiment)
length(trueresult[trueresult == TRUE])
# Create confusion matrix:
table(results, testdata$Sentiment)
ConfusionMatrix(table(results, testdata$Sentiment), c("0","1","2","3","4"))

# Quick test with the whole shebang....
model <- naiveBayes(train$Phrase, as.factor(train$Sentiment));
results <- predict(model, test$Phrase);
results
# Create submission file:
df <- data.frame(PhraseId=test$PhraseId, Sentiment=as.integer(results)-1)
#names(df) <- c("PhraseId", "Sentiment")
KaggleSubmission(df, submissionsFolder, "NaiveBayes", competitionName="SentimentAnalysis", rowNames=F)
  

# -------------------------------------------
# Example with e1071 package:
library(e1071)
## Categorical data only:
data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
summary(model)
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type = "raw")

pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)

## using laplace smoothing:
model <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)


## Example of using a contingency table:
data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic))

## Example with metric predictors:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris), iris[,5])

# Do a Classification tree:
# Does not work, Phrase must be a factor with max 32 levels...
library(tree)
trainTree <- tree(Sentiment ~ as.factor(Phrase), data=traindata)
plot(trainTree)
text(trainTree, cex=.8)
summary(trainTree)

# -------------------------------------------------------------------------------------------------------------------------------
# Use the AFINN-111.txt sentiment score file directly on the test set (do not use train set for anything!)

# TIPS for using AFINN with stopwords:
# https://github.com/tlfvincent/StatOfMind/tree/master/Sentiment_Analysis_of_TV_shows

# 1) Load the AFINN-111.txt file into a data frame
# 2) Parse the sentiment col of the test file, and lookup the words in the AFINN data frame
# TODO: Add functionality from term_sentiment.py for better sentiment detection in the phrase as a whole?

scores <- read.csv(paste0(dataFolder, "AFINN-111.txt"), header=F, sep="\t", stringsAsFactors=F)
head(scores)
names(scores) <- c("Sentiment", "Score")

testCopy <- test
# Add new score col to copy of test set
testCopy$PhraseScore <- 0
counter = 0
wordcounter = 0

for (counter in 1:length(testCopy$Phrase)) {
  temp <- unlist(strsplit(testCopy$Phrase[counter], " ")) # NOTE: Better(?) regex to split words below does not improve result!
  # temp <- regmatches(testCopy$Phrase[counter], gregexpr("\\w+", testCopy$Phrase[counter]))
  # To include hyphens in words (e.g. "uber-cool") use: "\\w+|\\w+\\-\\w+"
  words <- temp[1:length(temp)]
  score = 0
  
  for (wordcounter in 1:length(words)) {
    if (words[wordcounter] %in% scores$Sentiment) {
      score = score + scores$Score[which(scores$Sentiment == words[wordcounter])]
    }
  }
  
  testCopy$PhraseScore[counter] <- score
}

# Use our Normalize function to add a new normalized (0-4) score column
testCopy$PhraseScoreNormalized <- round(Normalize(min(testCopy$PhraseScore),
                                                  max(testCopy$PhraseScore), 0, 4, testCopy$PhraseScore))
# Create submission file
df <- data.frame(PhraseId=test$PhraseId, Sentiment=testCopy$PhraseScoreNormalized)
KaggleSubmission(df, submissionsFolder, "AFINN_SentimentScore", competitionName="SentimentAnalysis", rowNames=F)
# This creates best entry so far: 0.53482

# ----------------------------------------------------------------------------------------------------------------
# TODO: Ensemble the afinn result with a GLM on other summary statistics? Not very useful maybe....
# - Number of words in phrase (use MyNgrams)
# - Number of punctuation marks (few, none??)
# - Number of exlamation marks (None??)
# - Average length of sentences in phrase, etc.

# Create a train and validation set from the train set and do RMSE!
result <- CreateTrainAndValidationSets(train)
train.subset <- result[[1]]
validation.subset <- result[[2]]

num.words <- sapply(train$Phrase, function(x) length(MyNgrams(x, 1, T)))
num.words <- as.integer(num.words)
plot(sort(num.words), type="l", col="blue", main="Number of words", ylab="Words")

# length(gregexpr("!", "Balle! Skalle!")[[1]])
punctuation.marks <- sapply(train$Phrase, function(x) length(gregexpr("\\.", x)[[1]]))
punctuation.marks <- as.integer(punctuation.marks)
plot(sort(punctuation.marks), type="l", col="blue", main="Number of punctuation marks", ylab="Punctuation marks")

num.sentences <- sapply(train$Phrase, function(x) length(gregexpr("'[[:alnum:] ][.!?]", x)[[1]]))
num.sentences <- as.integer(num.sentences)
plot(sort(num.sentences), type="l", col="blue", main="Number of sentences", ylab="Sentences")

text <- "Balle is here. Skalle is over there. Here."
result <- gregexpr("\\.", text)[[1]]
result <- as.integer(result)
mean(result)

text <- 'Hello world!! Here are two sentences for you...'
length(gregexpr('[[:alnum:] ][.!?]', text)[[1]])

#attr(result, "match.length")

# -----------------------------------------------------------------------------------------------------------------
# Try h2o

package.install("h2o")
suppressMessages(library(h2o))
library(mlbench) # For testing on datasets in example referenced at http://blenditbayes.blogspot.co.uk/

localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
dat_h2o <- as.h2o(localH2O, train, key='train')
dat_h2o.test <- as.h2o(localH2O, test, key='test')

model <- 
  h2o.deeplearning(x=3, # column numbers for predictors (just one predictor not working?)
                   y=4, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   activation="Tanh", # or 'TanhWithDrouput'
                   #input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=F,
                   # l2, # TODO: How to set L1 or L2 regularization?
                   hidden=c(50, 50, 50), # three layers of 50 nodes
                   epochs=100) # max. no. of epochs

h2o_yhat_test <- h2o.predict(model, dat_h2o.test)
df_yhat_test <- as.data.frame(h2o_yhat_test)
table(df_yhat_test$predict)

#submission <- data.frame(ID=test.id, Cover_Type=df_yhat_test$predict)
#head(submission)
