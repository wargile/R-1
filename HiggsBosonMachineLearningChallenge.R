# Kaggle Higgs Boson Machine Learning Challenge

# Deadline: Sept. 15, 2014
# http://www.kaggle.com/c/higgs-boson/data
# http://higgsml.lal.in2p3.fr/files/2014/04/documentation_v1.8.pdf
# http://higgsml.lal.in2p3.fr/documentation/
# http://higgsml.lal.in2p3.fr/software/multiboost/
# http://higgsml.lal.in2p3.fr/software/hep-tmva-kit/
# http://higgsml.lal.in2p3.fr/software/starting-kit/
# http://www.kaggle.com/c/higgs-boson/forums/t/10255/leaderboard-score-3-39-using-random-forest-in-r/53325#post53325
# http://www.kaggle.com/c/higgs-boson/forums/t/10240/confusing-result/53230#post53230

# TIPS/SOLUTIONS:
# https://github.com/log0/higgs_boson/
# XGBoost is good (http://cran.r-project.org/web/packages/xgboost/index.html and Python),
# Also GradientBoostingClassifier (Python)
# http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.GradientBoostingClassifier.html


# Rank Order:
# The most signal-like event should have a rank order value of 550000.
# Other signal-like events should also have large integer values.
# The most background-like event should have a rank order of 1.
# Other background-like events should also have small integer values.
# ------------------------------------------------------------------------------------------------------------

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/HiggsBosonMachineLearningChallenge/data/"
codeFolder <- "C:/coding/Kaggle/HiggsBosonMachineLearningChallenge/code/"
submissionsFolder <- "C:/coding/Kaggle/HiggsBosonMachineLearningChallenge/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

head(train)
head(test)
dim(train)
dim(test)

classes <- sapply(train, class)
# Find all cols that are numeric, and round them? NOTE: Seems to give worse score!
# numeric.cols <- names(train)[which(classes == "numeric")]
# train[, numeric.cols ] <- round(train[, numeric.cols ])
# classes <- sapply(test, class)
# numeric.cols <- names(test)[which(classes == "numeric")]
# test[, numeric.cols ] <- round(test[, numeric.cols ])

# Look at missing values:
package.install("Amelia")
library(Amelia)
par(mfrow=c(1,2))
missmap(train[1:500, ], main = "Missingness Map Train", col = c("wheat", "cornflowerblue"))
missmap(test[1:500, ], main = "Missingness Map Test", col = c("wheat", "blue"))
par(mfrow=c(1,1))

sum(!complete.cases(train))
sum(!complete.cases(test))

library(e1071)
impute(train, what="median")
impute(test, what="median")

sum(!complete.cases(train))
sum(!complete.cases(test))

# Convert "s" and "b" Higgs Boson detection status in train data column Label to 0 and 1
train$Label[train$Label == "b"] <- 1
train$Label[train$Label == "s"] <- 0
train$Label <- as.integer(train$Label)

# Get rid of the Weight field in the training data: It is not a feature, and is not available in the test set
train$Weight <- NULL

# Create a nice plot (DER = first derivative?):
ggplot(data=subset(train, DER_mass_MMC > -999), aes(DER_mass_MMC, fill=Label)) +
  geom_histogram(alpha=0.5, position="identity", binwidth=.02) + scale_x_log10()
# Exploratory graph
plot(as.factor(Label) ~ as.factor(PRI_jet_num), data=train, col=as.integer(as.factor(Label)) + 1,
     cex.axis=.8, cex.lab=8, main="Label ~ PRI_jet_num")

# Check cor's (TODO: Observe a lot of high cor's , DER/PRI pairs etc.)
CorrelationPlot(train[, 2:31])

# Check var and get rid of zero-variance predictors, if exist
op <- par()
par(mar=c(10,5,3,1))
variance <- sapply(train[, 2:31], var)
plot(as.numeric(variance), pch=19, col="blue", xaxt="n", xlab="", ylab="Var", main="Variance")
axis(side=1, at=1:30, labels=names(train[2:31]), las=2, cex.axis=.7)
par <- op

# Check which predictors have a high number of -999 (not measurable) values. Exclude them from model?
op <- par()
par(mar=c(10,5,3,1))
not.measurable <- sapply(test[, 2:31], function(x) length(x[x == -999]))
not.measurable[not.measurable == 0]
plot(not.measurable, xaxt="n", bg="wheat", col="violetred", pch=21, xlab="",
     main="Predictors, number of -999 (not measurable) values")
axis(side=1, at=1:length(not.measurable), labels=names(not.measurable), las=2, cex.axis=.6)
# exlude.cols <- names(not.measurable)[not.measurable > 0]
par <- op

# TIP: Split models by number of jets
par(mfrow=c(1,2))
par(mar=c(5,5,3,1))
par(cex=.8)
nicecolors <- c("powderblue", "darkseagreen2", "lavenderblush3", "wheat", "cornflowerblue", "khaki3")
barplot(table(train$PRI_jet_num), col=nicecolors, main="Primary Jets (Train)")
barplot(table(test$PRI_jet_num), col=nicecolors, main="Primary Jets (Test)")
par(mfrow=c(1,1))

# ----------------------------------------------------------------------------------------------------------------
# Do glm
fit1 <- glm(as.factor(Label) ~ ., data=train, family=binomial(link="logit"))
fit2 <- glm(as.factor(Label) ~ DER_mass_MMC + DER_mass_transverse_met_lep + DER_mass_vis +
              DER_pt_h + DER_mass_jet_jet + DER_prodeta_jet_jet + DER_deltar_tau_lep +
              DER_pt_ratio_lep_tau + DER_met_phi_centrality + DER_lep_eta_centrality +
              PRI_met + PRI_met_sumet + PRI_jet_num, data=train, family=binomial(link="logit"))

summary(fit1)
pred1 <- predict(fit1, newdata=test, type="response")
pred.values <- pred1

# Create a submission file, use function in Tools.R
# Header: EventID, RankOrder, Class
eventId <- test$EventId
eventId[1:10]

# Fixing the RankOrder below is wrong, ignore....
rank.order <- data.frame(EventID=as.integer(eventId - min(eventId) + 1), RankOrder=pred.values)
rank.order <- rank.order[order(rank.order$RankOrder, decreasing=F), ]
rank.order$Index <- 1:length(eventId)
head(rank.order)

pred1 <- ifelse(pred1 < 0.5, "s", "b")
pred1[1:100]
# TODO: Set a 0.5 cutoff, and convert prediction result to "b" (0?) and "s" (1?) 


submission <- data.frame(EventID=eventId, RankOrder=1:length(eventId), Class=pred1)
#submission <- data.frame(EventID=eventId, RankOrder=rank.order$EventID, Class=pred1)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GLM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Do GBM
require(gbm)

# GBM model settings, these can be varied 
GBM_NTREES <- 1000 # 1000 used for best submission
GBM_SHRINKAGE <- 0.05 
GBM_DEPTH <- 6 # 6 used for best submission
GBM_MINOBS <- 50

# TODO: Create a better GBM model
# TODO: Set PRI_jet to factor
# TODO: Set distribution to bernoulli?
train$PRI_jet_num <- as.factor(train$PRI_jet_num)
deriv.cols <- which(substr(names(train), 1, 3) == "DER") * -1

# Build the GBM model (should this be Bernoulli dist??)
GBM_model <- gbm.fit(x=train[, -32], y=train$Label, distribution="gaussian", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=T) 
GBM_model <- gbm.fit(x=train[, c(-1,-31,-32,-33,deriv.cols)], y=train$Label, distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=T)
# After finding var importance, see below:
GBM_model <- gbm.fit(x=train[, -15], y=train$Label, distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=T) 
# Only cols without "not measurable" (-999) values:
measurable <- names(not.measurable[not.measurable == 0])
measurable <- measurable[c(-3,-6,-17,-19)] # Get rid of high cor cols too
names(train[measurable])
CorrelationPlot(train[, measurable])
GBM_model <- gbm.fit(x=train[, measurable], y=train$Label, distribution="bernoulli",
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=T) 

# List variable importance 
oldmar=par()$mar
par(mar=c(3, 10, 2, 1))
summary(GBM_model, GBM_NTREES, main="GBM variable importance", cex.axis=.8, cex.lab=.8,
        cex.main=1, cex.names=.7, las=1)
par(mar=oldmar)
oldmar=par()$mar
par(mar=c(10, 3.5, 2, 1))
influence <- relative.influence(GBM_model, GBM_NTREES, sort=T)
influence <- influence[influence > 1000]
barplot(influence, col="cornflowerblue", las=2, cex.axis=.7, cex.names=.7, cex.main=1, main="GBM variable importance")
par(mar=oldmar)
# Exclude cols that have low influence
# List the names in train that have influence > N in order to get only these cols
# TIP: Can also use intersect(names(train), names(influence))
keep.cols <- names(train)[(names(train) %in% names(influence))]
Label <- train$Label
train <- train[, keep.cols]
train <- cbind(train, Label)
names(train)

#axis(2, las=2)
#axis(1, las=1)

# Predict for the leaderboard data 
prediction <- predict.gbm(object=GBM_model, newdata=test, GBM_NTREES) 

# TODO: plogis on result?? plogis = exp(prediction) / (1 + exp(prediction))

# Put on correct scale and cap 
prediction <- expm1(prediction)
prediction
prediction <- pmin(15, prediction) 
prediction
prediction <- pmax(0, prediction) 
prediction

# Normalize prediction result between 0 and 1
pred.normalized <- Normalize(min(prediction), max(prediction), 0, 1, prediction)
pred.prob <- plogis(prediction)
# Same as:
pred.prob <- exp(prediction) / (1 + exp(prediction))

# Plot the submission distribution 
hist(prediction, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction", xlab="Prediction")
hist(pred.normalized, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction (normalized)", xlab="Prediction")

pred2 <- ifelse(pred.normalized < 0.5, "s", "b") # TODO: Try a different cutover value?
pred2[1:100]
pred2 <- ifelse(pred.prob < 0.5, "s", "b") # TODO: Try a different cutover value?
pred2[1:100]
# TODO: Set a 0.5 cutoff, and convert prediction result to "b" (0?) and "s" (1?) 

eventId <- test$EventId
eventId[1:10]

# Fixing the RankOrder
rank.order <- data.frame(EventID=as.integer(eventId - min(eventId) + 1), RankOrder=prediction)
rank.order <- rank.order[order(rank.order$RankOrder, decreasing=F), ]
rank.order$Index <- 1:length(eventId)
head(rank.order)

submission <- data.frame(EventID=eventId, RankOrder=1:length(eventId), Class=pred2)
submission <- data.frame(EventID=eventId, RankOrder=rank.order$EventID, Class=pred2)

# This creates the best entry so far: 2.94918
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GBM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Do Random forest and find importance
library(randomForest)

par(mar=c(5,4.5,3,1))
Sys.time()
pos <- 1
result <- integer()

# Take the cleaned train set from the gbm code:
tv <- CreateTrainAndValidationSets(train)
trainSubset <- tv$train
testSubset <- tv$validation
dim(trainSubset)
dim(testSubset)

# TODO: Need to weed down a lot of cols, or do smaller clusters and average models, way too big memory need here!
for (counter in seq(1, 25, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(as.factor(Label) ~ ., trainSubset, proximity=TRUE, keep.forest=TRUE, ntree=counter,
                               nodesize=10)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=testSubset, type="response")
  #prediction <- ifelse(prediction == 0, "b", "s")
  the.result <- (prediction == testSubset$Label)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()

best.ntrees <- which(result == min(result))
# Then train with best ntrees value
forestTrain1 <- randomForest(as.factor(Label) ~ ., train, proximity=TRUE, keep.forest=TRUE, ntree=best.ntrees)

# --------------------------------------------------------------------------------------------------------------
# Deep learning with h2o...
# http://cran.r-project.org/web/packages/h2o/h2o.pdf
# https://github.com/woobe/blenditbayes
# http://blenditbayes.blogspot.co.uk/
package.install("h2o")
suppressMessages(library(h2o))
library(mlbench) # For testing on datasets in example referenced at http://blenditbayes.blogspot.co.uk/

#cols <- c(which(names(train) %in% names(influence)[influence > 15]), 53)

localH2O <- h2o.init(ip="localhost", port=54321, startH2O=T, Xmx='2g')
dat_h2o <- as.h2o(localH2O, train, key='train')
test.names <- names(train[, -15])
dat_h2o.test <- as.h2o(localH2O, test[, test.names], key='test')

model <- 
  h2o.deeplearning(x=1:14, # column numbers for predictors
                   y=15, # column number for outcome variable
                   data=dat_h2o, # data in H2O format
                   activation="TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio=0.2, # % of inputs dropout
                   hidden_dropout_ratios=c(0.5, 0.5, 0.5), # % for nodes dropout
                   balance_classes=T,
                   # l2, # TODO: How to set L1 or L2 regularization?
                   hidden=c(50, 50, 50), # three layers of 50 nodes
                   epochs=100) # max. no. of epochs

h2o_yhat_test <- h2o.predict(model, dat_h2o.test)
df_yhat_test <- as.data.frame(h2o_yhat_test)
table(df_yhat_test$predict)
df_yhat_test$predict <- ifelse(df_yhat_test$predict == 1, "b", "s")
submission <- data.frame(EventID=eventId, RankOrder=1:length(eventId), Class=df_yhat_test$predict)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "h2o_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
