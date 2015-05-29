# Otto Group Product Classification Challenge
# Deadline: 18.05.2015
# https://www.kaggle.com/c/otto-group-product-classification-challenge

# Description:
# ------------
# Each row corresponds to a single product. There are a total of 93 numerical features, which represent counts
# of different events. All features have been obfuscated and will not be defined any further.
# There are nine categories for all products. Each target category represents one of our most important product
# categories (like fashion, electronics, etc.). The products for the training and testing sets are selected randomly.

# TIPS:
# https://www.kaggle.com/c/otto-group-product-classification-challenge/scripts
# https://www.kaggle.com/users/993/ben-hamner/otto-group-product-classification-challenge/t-sne-visualization
# https://kaggle2.blob.core.windows.net/forum-message-attachments/66759/2166/benchmark.py?sv=2012-02-12&se=2015-03-20T17%3A58%3A28Z&sr=b&sp=r&sig=vOVQmc1iyLTnQepV7DbKqe%2BXUMvken2Df5xWjTRnk8Y%3D
# https://github.com/ottogroup/kaggle/blob/master/benchmark.py
# https://gist.github.com/doobwa/3cefcc92891c4f1571f8
# http://www.kaggle.com/c/otto-group-product-classification-challenge/forums/t/12882/guess-this-is-really-the-time-to-try-out-neural-nets
# http://www.r-bloggers.com/comparing-tree-based-classification-methods-via-the-kaggle-otto-competition/
  
# TODO:
# "Cluster similar products": Can kmeans or other cluster techniques be used? Tree?
# Transform counts to TF-IDF features. How to do in R? Not needed?
#    - Is this to find the RATIOS for how often a certain product code (event count??) appears
#      in the total dataset? Do in Python using your own code?
# Neural nets? h2o with rf type?
# Train 9 OVA models, and average predictions?

# TODO: Precision/recall:
# https://class.coursera.org/nlp/lecture/142
# http://en.wikipedia.org/wiki/Precision_and_recall
# http://www.cs.odu.edu/~mukka/cs495s13/Lecturenotes/Chapter5/recallprecision.pdf

# TIP for parallel processing (but not available on Windows?)
# package.install("doMC")
# library(doMC)
# registerDoMC(cores=4)

library(randomForest)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html
library(stats) # For using fft() Fast Fourier Transform
library(kernlab)
library(e1071)
library(foreach)
library(doParallel)
library(tree)
library(rpart)
library(rpart.plot)
library(caret)

set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
SetStandardOptions()

dataFolder <- "C:/coding/Kaggle/OttoGroupProductClassificationChallenge/Data/"
submissionsFolder <- "C:/coding/Kaggle/OttoGroupProductClassificationChallenge/Submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  if (file.exists(paste0(dataFolder, "train_tdm_basis_as_df.rda")) == T)
    load(paste0(dataFolder, "train_tdm_basis_as_df.rda"))
}

dim(train)
str(train)
summary(train)
pairs(train)
sapply(train, class)
describe(train) # library(psych)
View(train[1:500,])

head(train, n=1)
sapply(train, class)
unique(is.na(train))
table(is.na(train)) # No NA's

# IMPORTANT! Drop the id's in train and test
train <- train[,-1]
test <- test[-1]

CorrelationPlot(train[, 1:50]) # TODO: Some fairly high cors. Remove and retry? Check AIC for GLM
# 14 feat_39 feat_45  0.8241
#  5  feat_3 feat_46  0.7775
#  7  feat_8 feat_36  0.6067
CorrelationPlot(train[, 51:93]) # Or are these "false" correlations? Removing could worsen score?
#  2 feat_61 feat_80  0.5751
#  1 feat_52 feat_74  0.5547

# TODO: change target strings into a numerical factor?
unique(train$target) # Class_1 to Class_9

# TEST: Check row average
row.means <- rowMeans(train[, -ncol(train)])
df <- data.frame(the.row.means=row.means, the.class=train$target)
head(df)
barplot(tapply(df$the.row.means, df$the.class, mean), las=2, main="Class rowMeans")
barplot(tapply(df$the.row.means, df$the.class, sum), las=2, main="Class rowMeans")
table(df$the.class)

boxplot(train$feat_5 ~ train$target, col="wheat")
boxplot(train$feat_15 ~ train$target, col="wheat")
boxplot(train$feat_25 ~ train$target, col="wheat")
boxplot(train$feat_35 ~ train$target, col="wheat")
boxplot(train$feat_45 ~ train$target, col="wheat")
boxplot(train$feat_55 ~ train$target, col="wheat")
boxplot(train$feat_75 ~ train$target, col="wheat")
boxplot(train$feat_85 ~ train$target, col="wheat")

# Count the number of instances where the count variable > 0, per class:
df <- data.frame()
for (className in unique(as.character(train$target))) {
  cat("Checking class", className, "...\n")
  train.subset <- subset(train, target == className)
  result <- apply(X=train.subset[,2:93], 1, FUN=function(x) length(which(x > 0)))
  new.data <- data.frame(counts=as.integer(result), className=rep(className, length(result)))
  df <- rbind(df, new.data)
}
df
par(mfrow=c(1,2))
barplot(table(train$target), main="Class distribution", col="wheat", las=2)
boxplot(counts ~ className, data=df, col="cornflowerblue", las=2, main="Feature counts by class", ylab="Counts")
proportions <- round(tapply(df$counts, df$className, sum) / sum(df$counts), 2)
par(mfrow=c(1,1))

# TEST: Add the feature count (mean/median?) as a predictor:
# train$featureCountPerClass <- 0
# for (className in unique(as.character(train$target))) {
#   train$featureCountPerClass[train$target == className] <-
#     sum(df$counts[df$className == className]) / nrow(train[train$target == className, ])
# }


result <- apply(X=train[,2:93], 2, FUN=function(x) length(which(x > 0)))
barplot(result, col="orange", las=2, main="Feature 1 to 93, feature count > 0")
hist(result, col="powderblue", main="Feature 1 to 93, feature count > 0")

# Sum the counts in all feature cols with the number of rows for each class:
classes <- paste0("Class_", 1:9)
counts <- numeric(0)
for (counter in 1:9) {
  train.subset <- subset(train, target == classes[counter])
  counts <- c(counts, sum(train.subset[,1:93]) / nrow(train.subset))
}
names(counts) <- classes
barplot(counts, las=2, col="orange", main="Total count in feature cols, by class")


# Get all cells that have val > 0 and put colname in those cells, otherwise "None":
# TODO: Parallelize?
cl <- makeCluster(8)
registerDoParallel(cl)
strt <- Sys.time()
df <- train[, 2:94]
corpus <- foreach (counter = seq(1, nrow(df))) %do% {
#for (counter in 1:nrow(df)) {
  if (counter %% 1000 == 0) print(counter)
  temp <- character(0)
  for (counter2 in 1:ncol(df)) {
    if (df[counter,counter2] > 0) {
      #df[counter,counter2] <- names(df)[counter2]
      temp <- paste(temp, names(df)[counter2])
    } else {
      #df[counter,counter2] <- "None"
      temp <- paste(temp, "None")
    }
  }
  to.corpus <- temp
}
print(Sys.time() - strt)
corpus[[2]]
# TODO: Save the corpus to an .RDA file!
save(corpus, file=paste0(dataFolder, "train_tdm_basis.rda"))
corpus.df <- data.frame(matrix(unlist(corpus), nrow=length(corpus), byrow=T), stringsAsFactors=FALSE)
Id <- 1:nrow(corpus.df)
corpus.df <- cbind(Id, corpus.df)
names(corpus.df) <- c("Id","Corpus")
save(corpus.df, file=paste0(dataFolder, "train_tdm_basis_as_df.rda"))
write.csv(corpus.df, file=paste0(dataFolder, "corpus.csv"), row.names=F)
#stopCluster(cl)

# Test the Python TDM:
tdm <- read.csv(paste0(dataFolder, "pandas_tdm2.csv"), sep=",", header=F)
tdm_merged <- do.call(paste, c(tdm[1:10,3:96], sep = "")) # NOTE: Quick way to merge cols into a string
tdm_merged <- do.call(paste0, c(tdm[1:10,3:96])) # NOTE: Quick way to merge cols into a string

tdm <- read.csv(paste0(dataFolder, "pandas_tdm3.csv"), sep=",", header=F,
                colClasses=c("integer","integer","character"))
target <- train$target
names(target) <- "target"
tdm <- cbind(tdm[,3], target)
rownames(tdm) <- NULL
tdm.df <- as.data.frame(tdm)
head(tdm[,c(1,80:95)], n=1)
dim(tdm)
result <- CreateTrainAndValidationSets(tdm)
train.tdm.subset <- result[[1]]
validation.tdm.subset <- result[[2]]
model.rf <- randomForest(as.factor(target) ~ ., data=train.tdm.subset, ntrees=100)
model.gbm <- gbm(as.factor(target) ~ ., data=train.tdm.subset, n.trees=100,
                 distribution="multinomial")
pred <- predict(model.rf, newdata=validation.tdm.subset, type="response")
pred <- predict(model.gbm, newdata=validation.tdm.subset, type="response", n.trees=100)
pred.result <- table(pred == as.factor(validation.tdm.subset$target))
pred.result

# TODO: Possible to see some patterns by averaging, counting, etc.? See if there is a pattern with which features has counts
# image(t(as.matrix(subset(train, target=="Class_1"))))

# --------------------------------------------------------------------------------------------------------------------------
# Split train in train and vaidation sets
# TODO: For this dataset using a stratified split. Probably best because the of class imbalance
# (equal proportion of classes in train and test sets)
result <- CreateTrainAndValidationSets(train)
train.subset <- result[[1]]
validation.subset <- result[[2]]

library(caTools)
set.seed(1000)
split <- sample.split(train$target, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)

# --------------------------------------------------------------------------------------------------------------------------

strt <- Sys.time()
rf <- randomForest(as.factor(target) ~ ., data=train, ntrees=15)
print(Sys.time() - strt)

varImpPlot(rf, cex=.7, cex.lab=1, cex.main=1, col="blue", pch=19, main="RF importance plot")
# p <- predict(rf, validation.subset)
p <- predict(rf, test, type="prob") # NOTE: Important for probs array! as.matrix(p, ncol=9, byrow=T)
# TODO: partialPlot(titanic.survival.rf, pred.data=titanic.train.nomiss, x.var=sex, which.class=1)

pm <- as.data.frame(as.matrix(p, ncol=9, byrow=T))
id <- 1:nrow(test)
pm <- cbind(id, pm)
head(pm)
submission <- pm

# ----------------------------------------------------------------------------------------------------------------------
# TODO: How to do local validation trying different RF models (and other types of models)?
#       Validate prob array against a pure 0/1 outcome in train.subset for each class?

start.tree <- 10
result <- numeric(0)
strt <- Sys.time()
pos <- 1
max.tree <- 50
tree.interval <- 5

for (counter in seq(start.tree, max.tree, tree.interval)) {
  print(paste0("Doing iteration ", pos, ": ntrees=", counter))
  rf <- randomForest(as.factor(target) ~ ., data=train.subset, ntrees=counter)
  Sys.time()
  pred <- predict(rf, newdata=validation.subset, type="response")
  pred.result <- table(pred == as.factor(validation.subset$target))
  score <- as.numeric(1 - (pred.result[2] / (pred.result[1] + pred.result[2])))
  result[pos] <- score
  pos <- pos + 1
}

# Try parallel:
# cl <- makeCluster(8)
# registerDoParallel(cl)
strt <- Sys.time()

result <- for(counter in seq(start.tree, 50, 5)) {
# result <- foreach(counter = seq(start.tree, 50, 5)) %do% {
   print(paste0("Doing iteration ", counter))
   rf <- randomForest(as.factor(target) ~ ., data=train.subset, ntrees=counter)
   pred <- predict(rf, newdata=validation.subset, type="response")
   pred.result <- table(pred == as.factor(validation.subset$target))
   to.result <- as.numeric(1 - (pred.result[2] / (pred.result[1] + pred.result[2])))
}

print(Sys.time() - strt)
# Stop the cluster
# stopCluster(cl)

best.trees <- which(result == min(result))
best.trees.str <- paste(best.trees + start.tree, collapse = ", ") # TODO: Merge!

plot(result, pch=19, col="steelblue3",
     main=paste0("Random Forest (score=", round(score, 4), ",  trees=", best.trees.str, ")"),
     xlab="Trees", ylab="Error rate", xaxt="n")
axis(side=1, at=seq(0, max.tree - start.tree, 5) + 1, labels=seq(start.tree, max.tree, tree.interval), cex.axis=.7)
lines(result, col="steelblue3")
abline(v=best.trees, col="red")
print(Sys.time() - strt)

# ---------------------------------------------------------------------------------------------------------------------------

# Try a tree with rpart:
library(rpart.plot)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(as.factor(target) ~ ., data=train.subset, method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end
cp.value <- 0.01
fit <- rpart(as.factor(target) ~ ., data=train.subset, cp=cp.value, minbucket=5) # TODO: Try various minbucket!
summary(fit)
prp(fit, cex=.7, col="blue")
pred <- predict(fit, validation.subset, type="class")
barplot(table(pred))
result <- table(validation.subset$target, pred)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy
my.title <- paste0("Confusion Matrix (accuracy = ", round(accuracy, 2), ")")
ConfusionMatrix(data=result, labels=paste0("Class", 1:9), title=my.title)

# ---------------------------------------------------------------------------------------------------------------------------

# TEST: OVA (One-vs-All) approach: Train a model on one class at a time. Set class n to 1 and all other classes to 0,
# train 9 models and ensemble/average result? TODO: How to convert this binary result to a prob array for all classes??
# Or use this binary result as a kind of weight?

classes <- unique(train$target)
classes
dfClasses <- data.frame()
predicts <- list()
summaries <- list()
models <- list()
#omit.cols <- c(11, 14, 27, 28, 31, 32, 35, 36, 39, 46, 52, 61, 60, 80, 94)
omit.cols <- c(94)

for (counter in 1:9) {
  print(counter)
  train.temp <- train
  train.temp$ClassNo <- ifelse(train.temp$target == classes[counter], 1, 0)
  result <- CreateTrainAndValidationSets(train.temp)
  train.class <- result[[1]]
  validation.class <- result[[2]]
  
  # Keep the ids for train and validation, we'll need them to combine parts results into a total prediction dataset
  train.class.ids <- train.class$id
  validation.class.ids <- validation.class$id
  # Then remove the id cols
  train.class <- train.class[, -1]
  validation.class <- validation.class[, -1]
  
  # NOTE: Testing with removing one or more cols (high cor) from the train set. 94 is outcome (target) col.
  model.class <- glm(as.factor(ClassNo) ~ ., data=train.class[, -omit.cols], family=binomial(link="logit"))
  models[[counter]] <- model.class
  summaries[[counter]] <- summary(model.class)
  p <- predict(model.class, newdata=validation.class[, -omit.cols], type="response")
  class.ratio <- table(train.class$ClassNo)
  pred.ratio <- table(round(p))
  dfClassesNew <- data.frame(NotClass=class.ratio[1], Class=class.ratio[2],
                             ClassRatio=class.ratio[2]/class.ratio[1], PredRatio=pred.ratio[2]/pred.ratio[1])
  dfClasses <- rbind(dfClasses, dfClassesNew) 
}

dfClasses
summaries[[1]]

# Try all the models on test set and average results
preds <- list()
for (counter in 1:9) {
  print(counter)
  p <- predict(models[[counter]], newdata=test, type="response")
  preds[[counter]] <- ifelse(p >= 0.5, 1, 0)
}
preds.mat <- matrix(unlist(preds), ncol=9, byrow=T)
preds.mat.final <- rowSums(preds.mat) / 9

# Check the pred deviation:
score <- round(sum(sqrt((dfClasses$ClassRatio - dfClasses$PredRatio)^2)) , 2)
par(mfrow=c(1,2))
plot(dfClasses$ClassRatio, col="blue", type="o", main=paste0("Class prediction, score = ", score), xaxt="n",
     ylab="Real/Pred", xlab=NA, ylim=c(min(dfClasses$PredRatio)-0.1, max(dfClasses$PredRatio)+0.1))
axis(side=1, at=1:9, labels=paste0("Class_",1:9), las=2)
lines(dfClasses$PredRatio, col="red", type="o")
barplot(table(train.class$target), col="wheat", las=2, main="Classes")
par(mfrow=c(1,1))


# ---------------------------------------------------------------------------------------------------------------------------


# pred.result <- table(p == as.factor(validation.subset$target))

# Score (0 = best): 
1 - (pred.result[2] / (pred.result[1] + pred.result[2]))
# LB score?
pred.result[2] / (pred.result[1] + pred.result[2])

# p.index <- as.numeric(p) # Get 5 for class5, etc.
# zeroes <- rep(0, nrow(test))
# submission <- data.frame(id=1:nrow(test), Class_1=zeroes, Class_2=zeroes, Class_3=zeroes, Class_4=zeroes, Class_5=zeroes,
#                 Class_6=zeroes, Class_7=zeroes, Class_8=zeroes, Class_9=zeroes)
# head(submission)
# for (i in counter:length(p.index)) {
#   submission[i, p.index[i] + 1] <- 1
# }

# ---------------------------------------------------------------------------------------------------------------------------
# Deep learning with h2o...

package.install("h2o")
suppressMessages(library(h2o))

train$target <- as.factor(train$target)

library(caTools)
set.seed(1000)
split <- sample.split(train$target, SplitRatio=0.7)
train.subset <- subset(train, split==TRUE)
validation.subset <- subset(train, split==FALSE)

# TODO: Scale all feat_<n> cols?

#localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, max_mem_size='4g', nthreads=-1)
localH2O <- h2o.init()
#dat_h2o <- as.h2o(localH2O, train.subset, key='train')
#dat_h2o.test <- as.h2o(localH2O, validation.subset, key='test')
dat_h2o <- as.h2o(localH2O, train, key='train')
dat_h2o.test <- as.h2o(localH2O, test, key='test')

# TODO: model.dl gives extremely bad predictions! Parameter tuning/replacement?
# TIPS HERE, maybe NN is good after all (feed-forward NN):
# http://learn.h2o.ai/content/hands-on_training/deep_learning.html
# http://docs.h2o.ai/datascience/deeplearning.html
# http://0xdata.com/blog/2015/02/deep-learning-performance/
x.cols <- 1:93
y.col <- 94

model.dl <- 
  h2o.deeplearning(x=x.cols, # column numbers for predictors
                   y=y.col, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   classification=T,
                   activation="TanhWithDropout", # or 'TanhWithDrouput'
                   #autoencoder=T,
                   #input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=T,
                   fast_mode=T,
                   l1=1e-5,
                   # l2=1e-5, # TODO: How to set L1 or L2 regularization?
                   hidden=c(50, 50, 50), # three layers of 50 nodes
                   epochs=10) # max. no. of epochs (try epocs=0.1??)

model.dl <- 
  h2o.deeplearning(x=x.cols, # column numbers for predictors
                   y=y.col, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   classification=T,
                   activation="RectifierWithDropout", # or 'TanhWithDrouput'
                   #autoencoder=T,
                   #input_dropout_ratio=0.2, # % of inputs dropout
                   #hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=F,
                   fast_mode=T,
                   #l1=1e-5,
                   #l2=1e-5
                   hidden=c(50, 50, 50), # Was: three layers of 256 nodes
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
  h2o.gbm(x=x.cols, y=y.col, distribution = "multinomial", data=dat_h2o, key="gbm", n.trees=250, 
          interaction.depth=5, n.minobsinnode=10, shrinkage=0.1, n.bins=20,
          group_split=T, importance=FALSE, nfolds=0, holdout.fraction=0,
          balance.classes=T, max.after.balance.size=5, class.sampling.factors=NULL,
          grid.parallelism=1)
model.gbm

model.rf <-
  h2o.randomForest(x=x.cols, y=y.col, data=dat_h2o, key="rf", classification=TRUE, ntree=250, 
                   depth=20, mtries= -1, sample.rate=2/3, nbins=20, seed= -1, 
                   importance=FALSE, score.each.iteration=FALSE, nfolds=0, 
                   holdout.fraction=0, nodesize=1, balance.classes=T, # was F 
                   max.after.balance.size=5, class.sampling.factors=NULL, 
                   doGrpSplit=TRUE, verbose=FALSE, oobee=TRUE, stat.type="ENTROPY", 
                   type="fast")
model.rf

h2o_yhat_test.dl <- h2o.predict(model.dl, dat_h2o.test)
df_h2o_yhat_test.dl <- as.data.frame(h2o_yhat_test.dl)

h2o_yhat_test.gbm <- h2o.predict(model.gbm, dat_h2o.test)
df_h2o_yhat_test.gbm <- as.data.frame(h2o_yhat_test.gbm)
# h2o GBM with n.trees=250, balance.classes=T gives best result so far: 0.51478
# df_h2o_yhat_test.gbm[1:2,2:10]

h2o_yhat_test.rf <- h2o.predict(model.rf, dat_h2o.test)
df_h2o_yhat_test.rf <- as.data.frame(h2o_yhat_test.rf) 
# df_h2o_yhat_test.rf[1:2,2:10]

# Get accuracy:
result <- table(validation.subset$target, df_h2o_yhat_test.gbm$predict)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy
my.title <- paste0("Confusion Matrix (accuracy = ", round(accuracy, 2), ")")
ConfusionMatrix(data=result, labels=paste0("Class", 1:9), title=my.title)


# TODO: Ensemble models (average predictions)? Include Deep Learning model? And/or try all separate?

# Create submission data frame:
id <- 1:nrow(test)
submission.dl <- cbind(id, df_h2o_yhat_test.dl[,2:10])
submission.gbm <- cbind(id, df_h2o_yhat_test.gbm[,2:10])
submission.rf <- cbind(id, df_h2o_yhat_test.rf[,2:10])

# Add colnames:
names(submission.dl) <- c("id", paste0("Class_", 1:9))
names(submission.gbm) <- c("id", paste0("Class_", 1:9))
names(submission.rf) <- c("id", paste0("Class_", 1:9))

# TODO: Find score on local CV:
id.cv <- 1:nrow(validation.subset)
row.no <- 2
submission.gbm.cv <- cbind(id.cv, df_h2o_yhat_test.gbm[,2:10])
submission.gbm.cv[1:10, ]
which.max(submission.gbm.cv[row.no, 2:10])
max.pred.col <- as.integer(as.factor(validation.subset$target[row.no])) + 1
max.pred <- submission.gbm.cv[row.no, max.pred.col]
rest.pred <- 1 - max.pred
1 - (rest.pred / max.pred)

# ---------------------------------------------------------------------------------------------------------------------------
# Calculate log loss (TODO: Not working, returns -Inf)

LogLoss <- function(y, preds) {
  # https://www.kaggle.com/c/otto-group-product-classification-challenge/forums/t/12895/compute-score-before-submission/67251#post67251
  # Assuming you have nine columns in a df named 'preds' corresponding to the predicted probabilities and a 
  # vector of ground truth labels with numbers ranging from 1-9 (for each class):
  preds <- cbind(preds, y)
  rowTotals <- apply(preds, 1, function(x) log(x[x[10]]))
  return (sum(rowTotals) / length(rowTotals) * -1)
}
LogLoss(as.integer(as.factor(validation.class$target)), df_h2o_yhat_test.gbm[, 2:10])

# ---------------------------------------------------------------------------------------------------------------------------
# Create the submission file:

head(submission.dl)
head(submission.gbm)
head(submission.rf)
tail(submission.dl)

save(submission.rf, file=paste0(submissionsFolder, "submission.rda"))
KaggleSubmission(submission.dl, submissionsFolder, "h2o_DL")   
KaggleSubmission(submission.rf, submissionsFolder, "h2o_RF")   
KaggleSubmission(submission.gbm, submissionsFolder, "h2o_GBM")   

# ---------------------------------------------------------------------------------------------------------------------------
