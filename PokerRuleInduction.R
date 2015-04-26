# Kaggle Poker Rule Induction
# http://www.kaggle.com/c/poker-rule-induction/data
# Ends: Mon 1 Jun 2015 
# -----------------------------------------------------------------

# Data from: https://archive.ics.uci.edu/ml/datasets/Poker+Hand

# Predictors:
# 1) S1 "Suit of card #1" 
# Ordinal (1-4) representing {Hearts, Spades, Diamonds, Clubs} 
# 2) C1 "Rank of card #1" 
# Numerical (1-13) representing (Ace, 2, 3, ... , Queen, King) 
# 3) S2 "Suit of card #2" 
# Ordinal (1-4) representing {Hearts, Spades, Diamonds, Clubs} 
# 4) C2 "Rank of card #2" 
# Numerical (1-13) representing (Ace, 2, 3, ... , Queen, King) 
# 5) S3 "Suit of card #3" 
# Ordinal (1-4) representing {Hearts, Spades, Diamonds, Clubs} 
# 6) C3 "Rank of card #3" 
# Numerical (1-13) representing (Ace, 2, 3, ... , Queen, King) 
# 7) S4 "Suit of card #4" 
# Ordinal (1-4) representing {Hearts, Spades, Diamonds, Clubs} 
# 8) C4 "Rank of card #4" 
# Numerical (1-13) representing (Ace, 2, 3, ... , Queen, King) 
# 9) S5 "Suit of card #5" 
# Ordinal (1-4) representing {Hearts, Spades, Diamonds, Clubs} 
# 10) C5 "Rank of card 5" 
# Numerical (1-13) representing (Ace, 2, 3, ... , Queen, King) 
# 11) CLASS "Poker Hand" 
# Ordinal (0-9) 

# Outcome variable "hand":
# 0: Nothing in hand; not a recognized poker hand 
# 1: One pair; one pair of equal ranks within five cards 
# 2: Two pairs; two pairs of equal ranks within five cards 
# 3: Three of a kind; three equal ranks within five cards 
# 4: Straight; five cards, sequentially ranked with no gaps 
# 5: Flush; five cards with the same suit 
# 6: Full house; pair + different rank three of a kind 
# 7: Four of a kind; four equal ranks within five cards 
# 8: Straight flush; straight + flush 
# 9: Royal flush; {Ace, King, Queen, Jack, Ten} + flush

# http://www.kaggle.com/c/poker-rule-induction/forums/t/11177/if-you-get-a-perfect-score
# https://github.com/weedjy/kaggle-poker-rule/commits?author=weedjy

# TIPS: - Absolute value of difference between neighboring cards (sorted according to rank)
#       - The number of each suit

oldpar <- par()
# Set some standard graphical params for plot
SetStandardOptions()

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/PokerRuleInduction/data/"
submissionsFolder <- "C:/coding/Kaggle/PokerRuleInduction/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  if (file.exists(paste0(dataFolder, "df.train.rda")) == T) {
    load(paste0(dataFolder, "df.train.rda"))
    load(paste0(dataFolder, "df.test.rda"))
  }
}
str(train)
unlist(sapply(train, class))
CorrelationPlot(train) # Naturally not much correlation here...

# Add data columns:
# Get values in all rank columns, and sort them (sort with suit? There's 5 "suit/rank" pairs in each row)
# Somehow combine/multiply rank/suit pair?

DoFeatureEngineering <- function(data, do.suits.and.ranks=F) {
  ranks <- data[, c(2,4,6,8,10)] # Ranks: 1 (Ace) to 13 (King)
  suits <- data[, c(1,3,5,7,9)] # Suits: {Hearts, Spades, Diamonds, Clubs}
  
  sorted.ranks <- t(apply(ranks, 1, sort))
  #sorted.ranks.and.suits <- t(apply(ranks * suits, 1, sort))
  
  df1 <- data.frame(sorted.ranks)
  #df1 <- data.frame(sorted.ranks.and.suits)
  
  sorted.ranks.diff <- t(apply(sorted.ranks, 1, diff))
  #sorted.ranks.and.suits.diff <- t(apply(sorted.ranks.and.suits, 1, diff))
  
  df1 <- cbind(df1, sorted.ranks.diff)
  #df1 <- cbind(df1, sorted.ranks.and.suits.diff)
  
  # TODO: Find the combinations (2+3 equal cards, 1+4 and so on):

  # Find suit frequency (optional)
  if (do.suits.and.ranks == T) {
    sorted.suits <- as.data.frame(matrix(unlist(apply(suits, 1, tabulate, nbins=4)), ncol=4, byrow=T))
    df1 <- cbind(df1, sorted.suits)
    sorted.ranks <- as.data.frame(matrix(unlist(apply(ranks, 1, tabulate, nbins=13)), ncol=13, byrow=T))
    df1 <- cbind(df1, sorted.ranks) # NOTE: Adding this did NOT improve the score! Try only ranks freq?
  }
  
  names(df1) <- c("C1","C2","C3","C4","C5","D1","D2","D3","D4","S1","S2","S3","S4",paste0("R", 1:13))
  #names(df1) <- c("C1","C2","C3","C4","C5","D1","D2","D3","D4",paste0("R", 1:13))
  
  # Find rows that are a flush variant
  flushRows <- which(df1$S1 == 5 | df1$S2 == 5 | df1$S3 == 5 | df1$S4 == 5)
  df1$FR <- 0
  df1$FR[flushRows] <- 1

  if (ncol(data) == 11) {
    df1$hand <- as.factor(data$hand)
  }

  return (df1)  
}

df.train <- DoFeatureEngineering(train)
df.train2 <- DoFeatureEngineering(train, T)
df.test <- DoFeatureEngineering(test[, -1]) # Skip the id column (not present in train set)
df.test2 <- DoFeatureEngineering(test[, -1], T) # Skip the id column (not present in train set)
save(df.train, file=paste0(dataFolder, "df.train.rda"))
save(df.test, file=paste0(dataFolder, "df.test.rda"))
save(df.train2, file=paste0(dataFolder, "df.train2.rda"))
save(df.test2, file=paste0(dataFolder, "df.test2.rda"))

result <- CreateTrainAndValidationSets(df.train)
result <- CreateTrainAndValidationSets(df.train2)
train.subset <- result[[1]]
validation.subset <- result[[2]]

# rpart on train/validation subsets:
# TODO: Get best cp value
model <- rpart(hand ~ ., data=train.subset, method="class", minbucket=10, cp=0.007)
summary(model)
prp(model, cex=.8, col="blue", main="Poker Rule Induction", cex.main=.8)
plot(model, main="Poker Rule Induction", cex.main=.8)
text(model, cex=.8, col="blue")
pred <- predict(model, newdata=validation.subset, type="class")
cm <- table(validation.subset$hand, pred)
ConfusionMatrix(cm, 0:9)
accuracy <- sum(diag(cm)) / nrow(validation.subset)
accuracy

# rpart on complete datasets:
# TODO: Get best cp value
model <- rpart(hand ~ ., data=df.train, method="class", minbucket=10, cp=0.007)
summary(model)
prp(model, cex=.8, col="blue", main="Poker Rule Induction", cex.main=.8)
plot(model, main="Poker Rule Induction", cex.main=.8)
text(model, cex=.8, col="blue")
pred <- predict(model, newdata=df.test, type="class")
table(pred)
submission.rpart <- data.frame(id=1:nrow(test), hand=pred)
head(submission.rpart)
KaggleSubmission(submission.rpart, submissionsFolder, "rpart")
# Code above with rpart(method="class", minbucket=10, cp=0.007) creates the best score for rpart: 0.99245 (NICE!)

#package.install("h2o")
suppressMessages(library(h2o))

train$hand <- as.factor(train$hand)
train.subset$hand <- as.factor(train.subset$hand)

localH2O <- h2o.init()
dat_h2o <- as.h2o(localH2O, df.train2, key='train')
dat_h2o.test <- as.h2o(localH2O, df.test2, key='test')
dat_h2o <- as.h2o(localH2O, train.subset, key='train')
dat_h2o.test <- as.h2o(localH2O, validation.subset, key='test')

# TODO: model.dl gives extremely bad predictions! Parameter tuning/replacement?
# TIPS HERE, maybe NN is good after all (feed-forward NN):
# http://learn.h2o.ai/content/hands-on_training/deep_learning.html
# http://docs.h2o.ai/datascience/deeplearning.html
# http://0xdata.com/blog/2015/02/deep-learning-performance/

model.dl <- 
  h2o.deeplearning(x=1:9, # column numbers for predictors
                   y=10, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   classification=TRUE,
                   activation="Tanh", # or 'TanhWithDrouput'
                   #autoencoder=T,
                   input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=F,
                   fast_mode=TRUE,
                   #l1=1e-5,
                   #l2=1e-5,
                   hidden=c(50,50,50), # three layers of 256 nodes
                   epochs=10) # max. no. of epochs (try epocs=0.1??)
model.dl

model.rf <-
  h2o.randomForest(x=1:27, y=28, data=dat_h2o, key="rf", classification=TRUE, ntree=1500, # Was 250 
                   depth=20, mtries=-1, sample.rate=2/3, nbins=20, seed= -1, # mtries was -1
                   importance=FALSE, score.each.iteration=FALSE, nfolds=0, 
                   holdout.fraction=0, nodesize=1, balance.classes=F, # was T 
                   max.after.balance.size=5, class.sampling.factors=NULL, 
                   doGrpSplit=TRUE, verbose=FALSE, oobee=TRUE, stat.type="ENTROPY", 
                   type="fast")
model.rf
# Best overall score so far: 0.99951 (NOTE: adding ranks frequency cols did NOT improve the score)

# Get model info:
# http://learn.h2o.ai/content/hands-on_training/deep_learning.html
model.rf
best_model <- model.rf@model[[1]]
best_model
best_params <- best_model@model$params
best_params$activation
best_params$hidden
best_params$l1

h2o_yhat_test.rf <- h2o.predict(model.rf, dat_h2o.test)
df_h2o_yhat_test.rf <- as.data.frame(h2o_yhat_test.rf)
head(df_h2o_yhat_test.rf)

submission.rf <- data.frame(id=1:nrow(test), hand=df_h2o_yhat_test.rf$predict)
head(submission.rf)

table(submission.rf$hand)
barplot(table(submission.rf$hand))
barplot(table(train$hand))

KaggleSubmission(submission.rf, submissionsFolder, "h2o_RF")   

