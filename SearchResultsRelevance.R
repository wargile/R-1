# Search Result Relevance
# https://www.kaggle.com/c/crowdflower-search-relevance
# Deadline: 06.07.2015
# http://www.crowdflower.com/

# The evaluation metric for this competition is quadratic weighted kappa, which measures the agreement between two ratings.
# Submission format: You must submit a csv file with the product id and a predicted search relevance for each search record.
# More info: https://www.kaggle.com/c/crowdflower-search-relevance/details/evaluation

# TIPS/CODE:
# https://www.kaggle.com/users/114978/triskelion/crowdflower-search-relevance/normalized-kaggle-distance
# https://www.kaggle.com/c/crowdflower-search-relevance/forums/t/14159/beating-the-benchmark-again
# https://www.kaggle.com/users/993/ben-hamner/crowdflower-search-relevance/python-benchmark
# https://code.google.com/p/word2vec/
# https://www.kaggle.com/c/word2vec-nlp-tutorial/details/part-1-for-beginners-bag-of-words
# https://github.com/wendykan/DeepLearningMovies
# https://www.kaggle.com/c/crowdflower-search-relevance/forums/t/14169/any-similar-kaggle-competition

# TIPS:
# tfidf+SVD is a method called LSA (latent semantic analysis). It's commonly used in text mining for dimension reduction.
# http://blog.josephwilk.net/projects/latent-semantic-analysis-in-python.html
# LSA: http://meefen.github.io/blog/2013/03/11/analyze-text-similarity-in-r-latent-semantic-analysis-and-multidimentional-scaling/
# http://cran.r-project.org/web/packages/lsa/index.html
# Use Latent Dirichlet Allocation (LDA; (Blei et al., 2003)) ?
# Use BeautifulSoup (Python only) to remove HTML tags before doiong NLP. R equivalent:
# http://www.r-bloggers.com/migrating-table-oriented-web-scraping-code-to-rvest-wxpath-css-selector-examples/
# I added these features (number of words, etc.) after tf-idf and SVD (just like Abhisekh's code). Then do scaling before feeding to the SVM.

# TODO: Impute product_description with product_title if not present

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
# install.packages("Metrics") # ?MeanQuadraticWeightedKappa
library(Metrics)
library(SnowballC)
library(tm)
# etc.
SetStandardOptions()

# ----------------------------------------------------------------------------------------------------------------------------
SplitAndCountWords <- function(text) {
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  return (length(word.vec))
}

## NOT_MINE: Function to compute matching words (see NOT_MINE folder)
## TODO: Also try with or add product description??
GetNumberOfMatchingWords <- function(terms){
  term1 <- terms[1]
  term2 <- terms[2]
  corpus <- Corpus(VectorSource(list(term1, term2))) ## Create corpus of 2 documents
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  frequencies <- DocumentTermMatrix(corpus)
  tf_matrix <- as.matrix(frequencies)
  similarity <- sum(tf_matrix[1, ] > 0 & tf_matrix[2, ] > 0) # Number of words present in query which are also present in response
  return (similarity)
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
  corpus = tm_map(corpus, removeNumbers)
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

# ------------------------------------------------------------------------------------------------------------------

datafolder <- "C:/coding/Kaggle/SearchResultsRelevance/data/"
submissionfolder <- "C:/coding/Kaggle/SearchResultsRelevance/submissions/"

if (file.exists(paste0(datafolder, "train.rda")) == F) {
  train <- read_csv(paste0(datafolder, "train.csv"))
  #train <- read.csv(paste0(datafolder, "train.csv"), header=T, stringsAsFactors=F, fileEncoding="UTF-8", allowEscapes=T,
  #                  quote="\"", encoding="UTF-8", sep=",")
  test <- read_csv(paste0(datafolder, "test.csv"))
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

par(mar=c(8,3,2,1))
barplot(table(sort(train$query, decreasing=T))[1:20], las=2, col="wheat")
par(mar=c(3,3,2,1))
# The number of unique train queries
length(unique(train$query))
# The number of unique test queries
length(unique(test$query))
# Are any queries different between the sets?
length(setdiff(unique(train$query), unique(test$query)))

y = train$median_relevance

# ----------------------------------------------------------------------------------------------------------------------------
# TEST Cluster the description Corpus

result <- CreateCorpus2(train$product_description, 0.97)
corpus <- result[[1]]
dtm <- result[[2]]
dtm
n <- 5000
distances <- dist(corpus[1:n, ], method="euclidean")
class(distances) # class 'dist'
attr(distances, "Labels")
attr(distances, "Size")
length(distances)
# Create clusters on the 'distances' class:
cluster.product.description <- hclust(distances, method="ward.D")
# plot(clusterMovies) # Makes R crash...
clusterGroups <- cutree(cluster.product.description, k=10) # Cut the tree into 10 groups of data
par(mfrow=c(2,1))
barplot(tapply(corpus$wash[1:n], clusterGroups, mean), main="Mean 'wash' term by clusterGroup", col="wheat")
barplot(tapply(corpus$water[1:n], clusterGroups, mean), main="Mean 'water' term by clusterGroup", col="wheat")
par(mfrow=c(1,1))
# Above gives the percentage of movies belonging in the Action and Romance clusters respectively
df <- do.call(rbind, lapply(corpus[1:n,], function(x) tapply(x, clusterGroups, mean)))
df <- round(df, 2)
colnames(df) <- paste0("C", 1:10)
df[rownames(df) %in% c("wash","water"), ]
df[rownames(df) %in% c("stylish","dress","wear"), ]

# ----------------------------------------------------------------------------------------------------------------------------
# TEST: Get cosine distance between search term and product description:
# Corpus/tm handling better, so we can remove stopwords, etc.?
GetCosineSimilarity <- function(data, do.normalized=T) {
  cosine.similarity <- numeric()
  cosine.similarity.raw <- numeric()
  score <- numeric()
  for (counter in 1:nrow(data)) {
    if (counter %% 1000 == 0)
      cat("Processing", counter, "...\n")
    #term1 <- unlist(MyBigrams(train$query[counter]))
    #term2 <- unlist(MyBigrams(train$product_title[counter]))
    term1 <- tolower(unlist(MyNgrams(data$query[counter], 1)))
    term2 <- tolower(unlist(MyNgrams(data$product_title[counter], 1)))
    #cat("Term1:", term1, "Term2:", term2, "\n")
    cosine.similarity.raw[counter] <- CosineSimilarity2(term1, term2)
    #score[counter] <- train$median_relevance[counter]
  }
  
  if (do.normalized == T) {
    min.cos <- min(cosine.similarity.raw)
    max.cos <- max(cosine.similarity.raw)
    for (counter in 1:nrow(data)) {
      if (counter %% 1000 == 0)
        cat("Processing", counter, "...\n")
      cosine.similarity[counter] <- round(Normalize(min.cos, max.cos,
                                                  1, 4, cosine.similarity.raw[counter]))
      #cosine.similarity[counter] <- round(Normalize(min(cosine.similarity.raw), 1,
      #                                              1, 4, cosine.similarity.raw[counter]))
    }
    return (cosine.similarity)
  } else {
    return (cosine.similarity.raw)
  }
}

cos.sim.train <- GetCosineSimilarity(train, F)
cos.sim.test <- GetCosineSimilarity(test, F)
train$cosine.distance <- cos.sim.train # Not normalized
train$cosine.distance <- as.factor(as.integer(cos.sim.train))
test$cosine.distance <- as.factor(as.integer(cos.sim.test))

# TODO: Expand to handle product_description too?
GetWordFrequency <- function(data) {
  word.freq <- numeric(0)
  
  for (counter in 1:nrow(data)) {
    if (counter %% 1000 == 0)
      cat("Processing", counter, "...\n")
    term1 <- tolower(unlist(MyNgrams(data$query[counter], 1)))
    term2 <- tolower(unlist(MyNgrams(data$product_title[counter], 1)))
    found <- 0
    for (wcounter in 1:length(term1)) {
      if (term1[wcounter] %in% term2)
        found <- found + 1
    }
    word.freq[counter] <- found / length(term1)
  }
  return(word.freq)
}

word.freq.train <- GetWordFrequency(train)
word.freq.test <- GetWordFrequency(test)
train$word.freq <- word.freq.train
test$word.freq <- word.freq.test

# TODO: Check col numbers. Also, check for cleaned train/test set provided by Oreo on kaggle forum 
# https://www.kaggle.com/c/crowdflower-search-relevance/forums/t/14159/beating-the-benchmark-yet-again/79440#post79440
# TODO: Also find similarity between product title and product description?
similarity.train <- apply(train[, c(2, 3)], 1, GetNumberOfMatchingWords) ## Find number of matching words between query and product title
pred_train <- ifelse(similarity.train == 0, 1, ifelse(similarity.train >= train$n_keywords, 4,
                                                      ifelse(similarity.train == 1, 2, 3)))
train$similarity <- pred_train
# TODO: pred_train is used here as being the input for the final submission (the relevance score).
# Instead, add pred_train/test result as another predictor variable in FeatureEngineering (TODO) function?

# Business rule to convert number of matching keywords to relevance score
# Rater.a, rater.b, min.rating, max.rating
ScoreQuadraticWeightedKappa(pred_train, train$median_relevance, 1, 5)
ScoreQuadraticWeightedKappa(similarity.train, train$median_relevance, 1, 5)
# Train score:

## Predict on test
# TODO: Also find similarity between product title and product description?
similarity.test <- apply(test[, c(2, 3)], 1, GetNumberOfMatchingWords) # Find number of matching words between query and product title
pred_test <- ifelse(similarity.test == 0, 1, ifelse(similarity.test >= test$n_keywords, 4,
                                                    ifelse(similarity.test == 1, 2, 3)))
test$similarity <- pred_test
# TODO: pred_test is used here as being the input for the final submission (the relevance score).
# Instead, add pred_train/test result as another predictor variable in FeatureEngineering (TODO) function?

# Save cleaned train and test sets
save(train, file=paste0(datafolder, "trainCleaned.rda"))
save(test, file=paste0(datafolder, "testCleaned.rda"))

par(mfrow=c(2,1))
plot(sort(train$cosine.distance), main="Cosine distance", col="blue", type="l")
plot(sort(train$word.freq), main="Word frequency", col="blue", type="l")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
par(mar=c(3,3,2,1))
barplot(table(train$similarity), col="wheat", main="Similarity")
plot(as.numeric(train$cosine.distance[50:170]), type="o", col="blue", main="Cosine similarity", ylim=c(1,4))
lines(train$median_relevance[50:170], type="o", col="red")
par(mfrow=c(1,1))


#set.seed(1000)
#split <- sample.split(train$median_relevance, SplitRatio=0.7)
#train.subset <- subset(train, split==TRUE)
#validation.subset <- subset(train, split==FALSE)

train.rows <- nrow(train)
test.rows <- nrow(test)

# Create a corpus
result <- CreateCorpus2(c(train$query, train$product_title, train$product_description), 0.986)
corpus <- result[[1]] # Get the sparse DTM as df 
dtm <- result[[2]] # Get the sparse DTM
dtm
dim(corpus)

corpus$median_relevance <- as.factor(train$median_relevance)
corpus$cosine.distance <- train$cosine.distance
corpus$word.freq <- train$word.freq
corpus$similarity <- as.factor(train$similarity)
corpus$n_keywords <- as.factor(train$n_keywords)

# Create a corpus on train and test
result <- CreateCorpus2(c(train$query, train$product_title, train$product_description,
                          test$query, test$product_title, test$product_description), 0.986)
corpus.final <- result[[1]] # Get the sparse DTM as df 
dtm.final <- result[[2]] # Get the sparse DTM
dtm.final
dim(corpus.final)

corpus.train.final <- head(corpus.final, train.rows)
corpus.test.final <- tail(corpus.final, test.rows)
corpus.train.final$median_relevance <- as.factor(train$median_relevance)
corpus.train.final$cosine.distance <- train$cosine.distance
corpus.test.final$cosine.distance <- test$cosine.distance
dim(corpus.train.final)
dim(corpus.test.final)

# ------------------------------------------------------------------------------------------------------------------------

# Do a wordcloud on the corpus
wordCloud <- wordcloud(colnames(corpus), colSums(corpus), scale=c(2.5, 0.5), colors=brewer.pal(9, "Blues"))

par(mfrow=c(1,1))
par(mar=c(6,3,2,1))
barplot(sort(colSums(corpus), decreasing=T)[1:50], las=2, col="powderblue", main="colSums corpus")
par(mar=c(3,3,2,1))

# ------------------------------------------------------------------------------------------------------------------------

# Split train in train and validation sets:
# set.seed(1000)
# split <- sample.split(train$median_relevance, SplitRatio=0.7)
# train.subset <- subset(train, split==TRUE)
# validation.subset <- subset(train, split==FALSE)

# Split corpus train in train and validation sets:
set.seed(1000)
split <- sample.split(corpus$median_relevance, SplitRatio=0.7)
train.subset <- subset(corpus, split==TRUE)
validation.subset <- subset(corpus, split==FALSE)

# ------------------------------------------------------------------------------------------------------------------------

# Try some clustering stuff:
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

cols <- 1:134 # Bag-of-words cols
head(train.subset, n=1)
distances <- dist(train.subset[, cols], method="euclidean") # Takes a long time sometimes, so save for later use
save(distances, file=paste0(datafolder, "distances.corpus.train.rda"))
# load(file=paste0(folder, "distances.corpus.train.rda"))

cluster.search <- hclust(distances, method="ward.D")
library(ggdendro)
# load package ape; remember to install it: install.packages('ape')
library(ape)
# plot basic tree
plot(as.phylo(cluster.search), cex=0.7, label.offset=1)
#hcd <- as.dendrogram(cluster.search)
#summary(cluster.search)
# plot(hcd)
cluster.search.cuts <- cutree(cluster.search, k=4)
summary(cluster.search.cuts)
table(cluster.search.cuts)
table(train.subset$median_relevance)

set.seed(1000)
km.clusters <- kmeans(distances, centers=4)
km.clusters <- kmeans(train.subset[, cols], centers=4, iter.max=10, nstart=4)
# Important, use data frame here if only numeric cols!
str(km.clusters)
#clusterGroups.km <- cutree(km.clusters, k=7) # Cut the tree into 7 groups of data (just for hclust)
clusterList.km <- lapply(1:4, function(x) subset(train.subset, km.clusters$cluster == x))
# save(clusterList.km, paste0(folder, "clusterList.km.rda"))
# load(paste0(folder, "clusterList.km.rda"))
KmeansClusters <- split(train.subset[, cols], km.clusters$cluster)
sapply(clusterList.km, nrow)
km.clusters$size
# or just use:
table(km.clusters$cluster)
table(train.subset$median_relevance)
# NOTE: This...
sum(sapply(clusterList.km, nrow))
# ...should be equal to:
nrow(train.subset)
table(clusterList.km[[1]]$median_relevance)
table(clusterList.km[[2]]$median_relevance)


# Try SVM. Remember to normalize all predictors:
library(e1071)
fit.svm <- svm(median_relevance ~ cosine.distance + word.freq, data=train.subset, scale=F)
summary(fit.svm)
predict.svm <- predict(fit.svm, newdata=validation.subset) # prob?
plot(sort(predict.svm), col="blue", main="predict.svm")


# Try random forest:
fit.rf <- randomForest(as.factor(median_relevance) ~ ., data=train.subset, na.action=na.omit)
summary(fit.rf)
varImpPlot(fit.rf, col="blue", pch=16, cex=.8)

predict.rf <- predict(fit.rf, newdata=validation.subset, type="class") # prob?
plot(sort(predict.rf), col="blue", main="predict.rf")
prediction.result.rf <- table(validation.subset$median_relevance, predict.rf)
prediction.result.rf
sum(diag(prediction.result.rf)) / sum(prediction.result.rf)

# Try random forest on final train set:
fit.rf.final <- randomForest(as.factor(median_relevance) ~ ., data=corpus.train.final, na.action=na.omit)
summary(fit.rf.final)
varImpPlot(fit.rf.final, col="blue", pch=16, cex=.8)

predict.rf.final <- predict(fit.rf.final, newdata=corpus.test.final, type="class") # prob?
plot(sort(predict.rf.final), col="blue", main="predict.rf.final")
table(predict.rf.final)


# Try a CART (not working...):
rownames(corpus.train) <- NULL
# Train to find optimal cp parameter
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(as.factor(median_relevance) ~ ., data=train.subset,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.1
corpusCART <- rpart(as.factor(median_relevance) ~ ., data=train.subset, method="class", cp=cp.value)
summary(corpusCART)
prp(corpusCART, cex=.8, col="blue", main="Median_Relevance")
# Evaluate the performance of the model
predictCART <- predict(corpusCART, newdata=validation.subset, type="class")
result <- table(validation.subset$median_relevance, predictCART)
result
# Compute accuracy
sum(diag(result)) / sum(result)

# -------------------------------------------------------------------------------------------------------------------------------------
# Try h2o
package.install("h2o")
suppressMessages(library(h2o))

#train$target <- as.factor(train$target)

# TODO: Scale all feat_<n> cols?

#localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
dat_h2o <- as.h2o(localH2O, train.subset, key='train')
dat_h2o.test <- as.h2o(localH2O, validation.subset, key='test')
#dat_h2o <- as.h2o(localH2O, corpus.train, key='train')
#dat_h2o.test <- as.h2o(localH2O, corpus.test, key='test')
# x.cols <- c(136:138)
# y.col <- 135
# x.cols <- c(1:134,136:139)
# y.col <- 135
x.cols <- c(1:134,138)
y.col <- 135

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
                   #l1=1e-5,
                   l2=1e-5,
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
  h2o.gbm(x=x.cols, y=y.col, distribution="multinomial", data=dat_h2o, key="gbm", n.trees=250, 
          interaction.depth=5, n.minobsinnode=30, shrinkage=0.1, n.bins=20,
          group_split=T, importance=FALSE, nfolds=0, holdout.fraction=0,
          balance.classes=F, max.after.balance.size=5, class.sampling.factors=NULL,
          grid.parallelism=4)
model.gbm

# Do classification
model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, data=dat_h2o, key="rf", classification=TRUE, ntree=250,
                   depth=20, mtries= -1, sample.rate=2/3, nbins=20, seed= -1, 
                   importance=FALSE, score.each.iteration=FALSE, nfolds=0, 
                   holdout.fraction=0, nodesize=1, balance.classes=F, # Do F?
                   max.after.balance.size=5, class.sampling.factors=NULL, 
                   doGrpSplit=TRUE, verbose=FALSE, oobee=TRUE, stat.type="ENTROPY", 
                   type="fast")
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

# Score it:
prediction.result <- table(validation.subset$median_relevance, df_h2o_yhat_test.dl$predict)
prediction.result <- table(validation.subset$median_relevance, df_h2o_yhat_test.gbm$predict)
prediction.result <- table(validation.subset$median_relevance, df_h2o_yhat_test.rf$predict)
prediction.result
sum(diag(prediction.result)) / sum(prediction.result)

# Rater.a, rater.b, min.rating, max.rating
ScoreQuadraticWeightedKappa(df_h2o_yhat_test.dl$predict, validation.subset$median_relevance, 1, 5)
ScoreQuadraticWeightedKappa(df_h2o_yhat_test.gbm$predict, validation.subset$median_relevance, 1, 5)
ScoreQuadraticWeightedKappa(df_h2o_yhat_test.rf$predict, validation.subset$median_relevance, 1, 5)
# Compare to....
ScoreQuadraticWeightedKappa(validation.subset$similarity, validation.subset$median_relevance, 1, 5)

# Create the submission file
# options("scipen"=100, "digits"=8)
# MySubmission <- data.frame(id=validation.subset$id, prediction=predict.rf)
MySubmission <- data.frame(id=test$id, prediction=test$similarity)
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "Similarity")
# RF with cosine distance score: 0.04443. Not too great....!
