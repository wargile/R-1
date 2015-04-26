# Random Acts of Pizza

# Deadline: June 1, 2015
# http://www.kaggle.com/c/random-acts-of-pizza/data

# http://cs.stanford.edu/~althoff/raop-dataset/altruistic_requests_icwsm.pdf
# http://www.convertcsv.com/json-to-csv.htm
# Using SnowballC stemmer library: http://www.r-bloggers.com/r-stem-pre-processed-text-blocks/

# TODO: Try "The Analytics Edge" Week 5 Twitter recitation code on this corpusPizza!

# --------------------------------------------------------------------------------------------------------------------------

library(rjson)

SetStandardOptions()

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/RandomActsOfPizza/data/"
codeFolder <- "C:/coding/Kaggle/RandomActsOfPizza/code/"
submissionsFolder <- "C:/coding/Kaggle/RandomActsOfPizza/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- fromJSON(file=paste0(dataFolder, "train.json"), method="C", unexpected.escape="error")
  test <- fromJSON(file=paste0(dataFolder, "test.json"), method="C", unexpected.escape="error")
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

# Normalize values between min and max
Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  # Find the normalized (0-1 range) value of curval
  normalized <- (curval - minval) / (maxval - minval)
  normalized
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

# http://stackoverflow.com/questions/16947643/getting-imported-json-data-into-a-data-frame-in-r
train <- lapply(train, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
do.call("rbind", train)

tmp <- lapply(train, function(u) 
  lapply(u, function(x) if(is.null(x)) NA else x)
)
tmp <- lapply(tmp, as.data.frame)
tmp <- do.call(rbind, tmp)
tmp

ncols <- length(sapply(train[1], unlist))
colnames <- rownames(sapply(train[1], unlist))

library(plyr)
do.call("rbind.fill", lapply(train, as.data.frame))
# Seems all of these solutions have problems with certain row(s)...

# NOTE: I did json->csv conversion here:
# http://www.convertcsv.com/json-to-csv.htm
train <- read.csv(paste0(dataFolder, "convertedcsv_train.csv"), header=T, sep="#", stringsAsFactors=F)
test <- read.csv(paste0(dataFolder, "convertedcsv_test.csv"), header=T, sep="#", stringsAsFactors=F)

head(train)
head(test)
dim(train)
dim(test)
sapply(train, class)

View(train[, c("requester_received_pizza", "request_text_edit_aware"), ])

# TEST: Check if the unix time of day timstamps have significance (season, month, hour). NOTE: Worsens score for GLM!
timestamp.train <- as.POSIXct(train$unix_timestamp_of_request, origin="1970-01-01") # CEST - Central European summertime
#timestamp.utc <- as.POSIXct(train$unix_timestamp_of_request_utc, origin="1970-01-01")
train$timestamp.hour <- as.factor(substr(timestamp.train, 12, 13))
train$timestamp.day <- as.factor(substr(timestamp.train, 9, 10))
train$timestamp.month <- as.factor(substr(timestamp.train, 6, 7))

timestamp.test <- as.POSIXct(test$unix_timestamp_of_request, origin="1970-01-01") # CEST - Central European summertime
test$timestamp.hour <- as.factor(substr(timestamp.test, 12, 13))
test$timestamp.day <- as.factor(substr(timestamp.test, 9, 10))
test$timestamp.month <- as.factor(substr(timestamp.test, 6, 7))

# TODO: Does requester_subreddits_at_request have any influence? Create a "score" col for these?
View(train[train$requester_received_pizza=="true", c(23, 24)])

# Check if the text length have any influence - add a col on train and test set for lengths
textlengths <- nchar(train$request_text_edit_aware) # NOTE: IMPORTANT FEATURE!
textlengths_title <- nchar(train$request_title) # NOTE: IMPORTANT FEATURE?
train$textlengths <- textlengths
train$textlengths_title <- textlengths_title
median(textlengths)
train$requester_received_pizza.num <- 0
train$requester_received_pizza.num <- ifelse(train$requester_received_pizza == "false", 0, 1)
cor(train$textlengths, as.numeric(as.factor(train$requester_received_pizza)))
cor(train$textlengths, train$requester_received_pizza.num) # 0.12 -- well, not very good....
var(train$textlengths, as.numeric(as.factor(train$requester_received_pizza)))
var(train$textlengths_title, as.numeric(as.factor(train$requester_received_pizza)))
hist(textlengths, col="orange", main=paste0("Train: Length of request text. Median=", median(textlengths)), breaks=200)
hist(nchar(train$request_title), col="orange", main=paste0("Train: Length of request title. Median=",
                                                           median(nchar(train$request_title))), breaks=50)

textlengths <- nchar(test$request_text_edit_aware)
textlengths_title <- nchar(test$request_title)
test$textlengths <- textlengths
test$textlengths_title <- textlengths_title
median(textlengths)

hist(train$textlengths[train$requester_received_pizza == "true"], col="orange",
     main=paste0("Test: Length of request text. Median=", median(textlengths)), breaks=200)
hist(train$textlengths[train$requester_received_pizza == "false"], col="orange",
     main=paste0("Test: Length of request text. Median=", median(textlengths)), breaks=200)

hist(textlengths, col="orange", main=paste0("Test: Length of request text. Median=",
                                            median(textlengths)), breaks=200)
hist(nchar(train$request_title), col="orange", main=paste0("Train: Length of request title. Median=",
                                                           median(nchar(train$request_title))), breaks=50)
par(mfrow=c(1,3))
oldmar=par()$mar
par(mar=c(2.6,2,2,.5))
boxplot(train$textlengths_title ~ train$requester_received_pizza, col=c("red", "green"),
        main="Length request_text_title", xlab="", ylab="", cex.axis=.7, cex.lab=.8, cex.main=.8)
boxplot(train$textlengths ~ train$requester_received_pizza, col=c("red", "green"),
        main="Length request_text", xlab="", ylab="", cex.axis=.7, cex.lab=.8, cex.main=.8)
boxplot(as.numeric(train$requester_number_of_comments_at_request) ~ train$requester_received_pizza, col=c("red", "green"),
        main="Number_of_comments_at_request", xlab="", ylab="", cex.axis=.7, cex.lab=.8, cex.main=.8)
par(mar=oldmar)
par(mfrow=c(1,1))


# TIP: Build a dictionary of "good/polite" words:
# recipocrate, please, lost, job, unemployed, unemployment, mortage, hard times, rent, give back, pay back, exchange, night,
# appreciated, paid/payed forward, work, money, died, hospital, sick, passed away, sympathy, old, return the favor, ...
# etc. and create score dict/col, use grepl to just get subsets of strings (to cover many variants of a phrase/word), etc.
# TODO: Create a dict in Python, count all words where pizza was delivered, and all words where not. Compare dict results.
CreatePizzaScore <- function(data) {
  dfpizza <- data.frame(y=data$requester_received_pizza, x=tolower(data$request_text_edit_aware))
  dfpizza$x <- gsub("\"", "", dfpizza$x) # Get rid of extra qoute chars  
  dfpizza$x <- gsub("\n", " ", dfpizza$x) # Get rid of newlines, replace with space
  dfpizza <- dfpizza[which(dfpizza$y == "true"), ]
  dfpizza$y <- NULL
  dfpizza$x[which(nchar(dfpizza$x[1:nrow(dfpizza)]) == 0)] <- "*"
  head(dfpizza)
  write.table(dfpizza, file=paste0(dataFolder, "Pizza.csv"), row.names=F, quote=T, sep="#", eol="\n")
}

CreatePizzaTexts <- function(data, train.or.test) {
  dfpizza <- data.frame(id=data$request_id, text=tolower(data$request_text_edit_aware))
  dfpizza$text <- gsub("\"", "", dfpizza$text) # Get rid of extra qoute chars  
  dfpizza$text <- gsub("\n", " ", dfpizza$text) # Get rid of newlines, replace with space
  dfpizza$text[which(nchar(dfpizza$text[1:nrow(dfpizza)]) == 0)] <- "*"
  head(dfpizza)
  write.table(dfpizza, file=paste0(dataFolder, paste0("PizzaTexts_", train.or.test, ".csv")),
              row.names=F, quote=T, sep="#", eol="\n")
}

# Create a Python dictionary with scores of words from the text
CreatePizzaScore(train)
# Export the cleaned-up texts from train and test to score
CreatePizzaTexts(train, "train")
CreatePizzaTexts(test, "test")

goodwords <- list()
word.regex <- "(\\w)(\\w)*'?(\\w)?"

for (counter in 1:nrow(dfpizza)) {
  #words <- strsplit(dfpizza$x[counter], " ")[[1]]
  words <- unlist(strsplit(dfpizza$x[counter], "\\s+", fixed=F))
  #words <- gsub("?{10}", "", words, fixed=F)
  for (wordcounter in 1:length(words)) {
    if (nchar(words[wordcounter]) > 0) {
      if (!is.null(goodwords[[words[wordcounter]]])) {
        goodwords[[words[wordcounter]]] <- goodwords[[words[wordcounter]]] + 1 
      } else {
        goodwords[[words[wordcounter]]] <- 1
      }
    }
  }
}
goodwords

word <- character(0)
frequency <- integer(0)
for (name in names(goodwords)) {
  print(name) # Word
  print(goodwords[[name]]) # Frequency
  word <- c(word, as.character(name))
  frequency <- c(frequency, as.integer(goodwords[[name]]))
}
dfwords <- data.frame(word=word, frequency=frequency)
dfwords


train$requester_received_pizza[1:50]
train$requester_received_pizza[train$requester_received_pizza == "false"] <- 0
train$requester_received_pizza[train$requester_received_pizza == "true"] <- 1
train$requester_received_pizza <- as.integer(train$requester_received_pizza)

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

# Get rid of useless cols
# Find cols in train not in test and vice versa
names(train)[which(!(names(train) %in% names(test)))] # Quite a few...
names(test)[which(!(names(test) %in% names(train)))] # None
names(train)[which((names(train) %in% names(test)))] # Find intersecting col names

train$giver_username_if_known <- NULL
train$request_id <- NULL
train$request_text <- NULL
train$request_text_edit_aware <- NULL
train$request_title <- NULL
train$train$request_text <- NULL # Could be useful, but must be fixed
train$requester_user_flair <- NULL
train$requester_username <- NULL
train$unix_timestamp_of_request <- NULL
train$requester_days_since_first_post_on_raop_at_retrieval <-
  as.integer(train$requester_days_since_first_post_on_raop_at_retrieval)

test$giver_username_if_known <- NULL
test$request_id <- NULL
test$request_text <- NULL
test$request_text_edit_aware <- NULL
test$request_title <- NULL
test$train$request_text <- NULL # Could be useful, but must be fixed
test$requester_user_flair <- NULL
test$requester_username <- NULL
test$unix_timestamp_of_request <- NULL

# TODO: Check for image links in the request text - can indicate that users proved their needs by photo evidence
train$evidence_given <- 0 # Create new col
for (counter in 1:nrow(train)) {
  result <- regexpr("http", train$request_text_edit_aware[counter], fixed=T)
  train$evidence_given[counter] <- ifelse(attr(result, "match.length") > 0, 1, 0)
}
test$evidence_given <- 0 # Create new col
for (counter in 1:nrow(test)) {
  result <- regexpr("http", test$request_text_edit_aware[counter], fixed=T)
  test$evidence_given[counter] <- ifelse(attr(result, "match.length") > 0, 1, 0)
}

plot(table(train$evidence_given, train$requester_received_pizza), col=c("red", "green"),
     main="Photo evidence given", xlab="", ylab="", cex.axis=.8)


# Check the UNIX timestamps and add col(s): Find the hour of day when request was made (does not seem to improve score!)
GetTimestamps <- function(data) { 
  timestamps.fixed <- as.POSIXct(data, origin="1970-01-01")
  hours <- substr(timestamps.fixed, 12, 13)
  return (as.factor(hours))
}
GetWeekday <- function(data) { 
  #weekday <- lapply(data, function(x) as.POSIXlt(x, origin="1970-01-01")$wday)
  weekday <- as.POSIXlt(data, origin="1970-01-01")$wday
  return (as.factor(weekday))
}
train$timestamp_of_request_utc.hour <- GetTimestamps(train$unix_timestamp_of_request_utc)
test$timestamp_of_request_utc.hour <- GetTimestamps(test$unix_timestamp_of_request_utc)
train$timestamp_of_request_utc.weekday <- GetWeekday(train$unix_timestamp_of_request_utc)
test$timestamp_of_request_utc.weekday <- GetWeekday(test$unix_timestamp_of_request_utc)

# ----------------------------------------------------------------------------------------------------------------------
# TEST START: Build a feature engineered set for train and test:

DoFeatureEngineering <- function(data.in) {
  data <- data.frame(timestamp_of_request_utc.hour=rep(NA, nrow(data.in)))
  data$evidence.given <- 0

  for (counter in 1:nrow(data.in)) {
    result <- regexpr("http", data.in$request_text_edit_aware[counter], fixed=T)
    data$evidence.given[counter] <- ifelse(attr(result, "match.length") > 0, 1, 0)
  }
  
  data$timestamp_of_request_utc.hour <- GetTimestamps(data.in$unix_timestamp_of_request_utc)
  data$timestamp_of_request_utc.weekday <- GetWeekday(data.in$unix_timestamp_of_request_utc)
  data$title.length <- nchar(data.in$request_title)
  data$request_text_edit_aware.length <- nchar(data.in$request_text_edit_aware)
  data$requester_days_since_first_post_on_raop_at_request <- data.in$requester_account_age_in_days_at_request
  data$requester_days_since_first_post_on_raop_at_request <- data.in$requester_days_since_first_post_on_raop_at_request
  data$requester_number_of_comments_at_request <- data.in$requester_number_of_comments_at_request       
  data$requester_number_of_comments_in_raop_at_request <- data.in$requester_number_of_comments_in_raop_at_request 
  data$requester_number_of_posts_at_request <- data.in$requester_number_of_posts_at_request    
  data$requester_number_of_posts_on_raop_at_request <- data.in$requester_number_of_posts_on_raop_at_request
  data$requester_number_of_subreddits_at_request <- data.in$requester_number_of_subreddits_at_request
  data$requester_upvotes_minus_downvotes_at_request <- data.in$requester_upvotes_minus_downvotes_at_request
  data$requester_upvotes_plus_downvotes_at_request <- data.in$requester_upvotes_plus_downvotes_at_request
  return (data)  
}

train.fe <- DoFeatureEngineering(train)
train.fe$requester_received_pizza <- ifelse(train$requester_received_pizza == "false", "0", "1")
test.fe <- DoFeatureEngineering(test)

table(train.fe$evidence.given)
table(test.fe$evidence.given)

# TODO: Add in term frequency for most common tems from TDM? This result seems weird!
par(mar=c(4,3,2,1))
par(mfrow=c(2,1))
barplot(colSums(labeledTermsTrain[, -ncol(labeledTermsTrain)]), las=2, col="wheat", main="TDM frequency")
barplot(colSums(labeledTermsTest), las=2, col="powderblue")
par(mfrow=c(1,1))
par(mar=c(3,3,2,1))

pizzaCART2 <- rpart(as.factor(requester_received_pizza) ~ ., data=train.fe, na.action=na.omit,
                    minbucket=10, cp=0.003, method="class")
prp(pizzaCART2, cex=.6, col="blue", main="Requester received pizza")
# Make predictions on the train set
predTrain = predict(pizzaCART2)
# Make predictions on the test set
predTest = predict(pizzaCART2, newdata=test.fe)
predTrain[1:10]
predTest[1:10]
#predTrain.prob = predTrain[,2]
#predTest.prob = predTest[,2]
# Compute accuracy
result2 <- table(train.fe$requester_received_pizza, predTrain[,2] > 0.5)
result2
score2 <- sum(diag(result2)) / sum(result2) 
score2
par(mfrow=c(2,1))
plot(sort(predTrain[,2]), type="l", col="blue", main="Prediction train")
plot(sort(predTest[,2]), type="l", col="red", main="Prediction test")
par(mfrow=c(1,1))

# TEST END: Build a feature engineered set for train and test:
# ----------------------------------------------------------------------------------------------------------------------

# GLM:
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                     -1.64e+00   9.74e-02  -16.88  < 2e-16 ***
# requester_number_of_comments_at_request         -1.42e-04   3.23e-04   -0.44   0.6607    
# requester_number_of_comments_in_raop_at_request  7.91e-02   1.69e-02    4.68  2.9e-06 ***
# requester_number_of_posts_at_request            -1.07e-04   1.08e-03   -0.10   0.9214    
# requester_number_of_posts_on_raop_at_request     6.46e-01   1.27e-01    5.09  3.5e-07 ***
# requester_number_of_subreddits_at_request        3.21e-03   3.01e-03    1.06   0.2872    
# requester_upvotes_minus_downvotes_at_request     9.34e-06   3.67e-05    0.25   0.7994    
# requester_upvotes_plus_downvotes_at_request     -1.41e-06   4.95e-06   -0.28   0.7757    
# textlengths                                      6.78e-04   9.76e-05    6.95  3.7e-12 ***
# textlengths_title                                9.21e-04   1.05e-03    0.88   0.3792    
# evidence_given                                   3.75e-01   1.36e-01    2.77   0.0057 ** 


# ----------------------------------------------------------------------------------------------------------------
# Do glm

# "given_username_if_known",
# "post_was_edited",
# "request_id",
# "request_text",
# "request_text_edit_aware",
# "request_title",
# "requester_account_age_in_days_at_request", (need int)
# "requester_account_age_in_days_at_retrieval", (need int)
# "requester_days_since_first_post_on_raop_at_request" (need int),
# "requester_days_since_first_post_on_raop_at_retrieval" (need int),
# "requester_subreddits_at_request" (need to count words put in sep col, have col for that already!)
# "requester_user_flair",
# "requester_user_name",
# "unix_timestamp_of_request", (need conversion?, skip UTC?)
# "unix_timestamp_of_request_utc" (need conversion?)

# -1,-4,-5,-7,-8,-9,-11,-12,-13,-24,-29,-30,-31,-32
skip.cols <- c(-1,-4,-5,-7,-8,-9,-10,-11,-12,-13,-24,-29,-30,-31,-32)
skip.cols.not.in.test <- c(-2,-3,-6,-15,-17,-19,-21,-26,-28)
all.skip.cols <- sort(c(skip.cols, skip.cols.not.in.test), decreasing=T)

# Skipping cols:
#[1] "number_of_downvotes_of_request_at_retrieval"          "number_of_upvotes_of_request_at_retrieval"           
#[3] "post_was_edited"                                      "request_number_of_comments_at_retrieval"             
#[5] "request_text"                                         "requester_account_age_in_days_at_retrieval"          
#[7] "requester_days_since_first_post_on_raop_at_retrieval" "requester_number_of_comments_at_retrieval"           
#[9] "requester_number_of_comments_in_raop_at_retrieval"    "requester_number_of_posts_at_retrieval"              
#[11] "requester_number_of_posts_on_raop_at_retrieval"       "requester_received_pizza"                            
#[13] "requester_upvotes_minus_downvotes_at_retrieval"       "requester_upvotes_plus_downvotes_at_retrieval"       
#[15] "requester_user_flair"                        

fit1 <- glm(as.factor(requester_received_pizza) ~ ., data=train[, all.skip.cols], family=binomial(link="logit"))
fit2 <- glm(as.factor(requester_received_pizza) ~ textlengths + requester_number_of_posts_on_raop_at_request +
              requester_number_of_comments_in_raop_at_request + timestamp_of_request_utc.hour +
              timestamp_of_request_utc.weekday, data=train, family=binomial(link="logit"))
summary(fit1)
summary(fit2)
pred1 <- predict(fit1, newdata=test, type="response")
# Same as: plogis(predict(fit1, newdata=test, type="link"))
min(pred1)
max(pred1)
pred2 <- predict(fit2, newdata=test, type="response")
min(pred2)
max(pred2)
# Get RMSE(?) score when splitting train set in train and validation sets
# y.pred <- round(pred.values)
# mean((y.pred - train$requester_received_pizza)^2)
# sum(abs(y.pred - requester_received_pizza))

submission <- data.frame(request_id=test$request_id, requester_received_pizza=pred1)
submission <- data.frame(request_id=test$request_id, requester_received_pizza=pred2)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GLM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)
# GLM creates the best entry so far: 0.64450

# ---------------------------------------------------------------------------------------------------------------
# Do GBM
require(gbm)

# GBM model settings, these can be varied 
GBM_NTREES = 2000 # N used for best submission
GBM_SHRINKAGE = 0.05 
GBM_DEPTH = 6 # N used for best submission
GBM_MINOBS = 50

# Build the GBM model
train.gbm <- train
train.gbm$requester_received_pizza[train.gbm$requester_received_pizza == "false"] <- 0
train.gbm$requester_received_pizza[train.gbm$requester_received_pizza == "true"] <- 1

GBM_model <- gbm.fit(x=train.gbm[, c(-23, all.skip.cols)], y=train.gbm$requester_received_pizza,
                     distribution="bernoulli", 
                     n.trees=GBM_NTREES, shrinkage=GBM_SHRINKAGE, interaction.depth=GBM_DEPTH,
                     n.minobsinnode=GBM_MINOBS, verbose=T) 

# List variable importance 
oldmar=par()$mar
par(mar=c(3,15,2,1))
summary(GBM_model, GBM_NTREES, main="GBM variable importance", cex.axis=.7, cex.lab=.7,
        cex.main=1, cex.names=.6, las=1)
par(mar=oldmar)
oldmar=par()$mar
par(mar=c(15,3.5,2,1))
influence <- relative.influence(GBM_model, GBM_NTREES, sort=T)
influence <- influence[influence> 0]
barplot(influence, col="cornflowerblue", las=2, cex.axis=.7, cex.names=.6, cex.main=1, main="GBM variable importance")
par(mar=oldmar)

#axis(2, las=2)
#axis(1, las=1)

# Predict for the leaderboard data (TODO: got to find which cols to skip here!)
prediction <- predict.gbm(object=GBM_model, newdata=test[, names(train[, c(-23, all.skip.cols)])], GBM_NTREES) 
p <- plogis(prediction)
p[1:100]
min(p)
max(p)

# Put on correct scale and cap 
prediction <- expm1(prediction)
prediction
prediction <- pmin(15, prediction) 
prediction
prediction <- pmax(0, prediction) 
prediction

# Normalize prediction result between 0 and 1
pred.normalized <- Normalize(min(prediction), max(prediction), 0, 1, prediction)

# Plot the submission distribution 
hist(prediction, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction", xlab="Prediction")
hist(pred.normalized, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction (normalized)", xlab="Prediction")
hist(p, breaks=40, col="cornflowerblue", cex.axis=.7, cex.main=1,
     main="GBM prediction (normalized plogis)", xlab="Prediction")

options(digits=15)
submission <- data.frame(request_id=test$request_id, requester_received_pizza=p)
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "GBM_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)


# ---------------------------------------------------------------------------------------------------------------
# Do Random forest and find importance
library(randomForest)

par(mar=c(5,4.5,3,1))

train.rf <- train
train.rf$requester_received_pizza[train.rf$requester_received_pizza == "false"] <- 0
train.rf$requester_received_pizza[train.rf$requester_received_pizza == "true"] <- 1

rows <- dim(train.rf)[1]
rows <- sample(1:rows, rows / 2, replace=T)
trainSubset <- train.rf[rows, all.skip.cols]
testSubset <- train.rf[-rows, all.skip.cols]

pos <- 1
result <- integer()

for (counter in seq(1, 100, 1)) {
  Sys.time()
  forestTrain1 <- randomForest(requester_received_pizza ~ ., trainSubset,
                               proximity=TRUE, keep.forest=TRUE, ntree=counter)
  Sys.time()
  prediction <- predict(forestTrain1, newdata=testSubset[, -6], type="response")
  #prediction <- ifelse(prediction == "false", 0, 1)
  the.result <- (prediction == testSubset$requester_received_pizza)
  table(the.result)
  result[pos] <- (1 - (length(the.result[the.result == T]) / nrow(testSubset)))
  pos <- pos + 1
}

plot(result, pch=19, col="steelblue3", main="Random Forest Error Rate", cex.axis=.8)
lines(result, col="steelblue3")
abline(v=which(result == min(result)), col="red")
Sys.time()

forestTrain1 <- randomForest(train.rf$requester_received_pizza ~ ., train.rf[, c(-23, all.skip.cols)],
                             proximity=TRUE, keep.forest=TRUE, ntree=41)
prediction <- predict(forestTrain1, newdata=test, type="response")

min(prediction)
max(prediction)
options(digits=15)
#prediction <- ifelse(prediction == "false", 0, 1)
submission <- data.frame(request_id=test$request_id, requester_received_pizza=prediction) # TODO: plogis transform?
head(submission)

write.csv(submission, file=paste0(submissionsFolder, "RF_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------------------------------
names(train)
train$requester_received_pizza[1:10]

# TODO: Do some test plots and cor
train2 <- train
train2$requester_received_pizza[which(train2$requester_received_pizza == "false")] <- 0
train2$requester_received_pizza[which(train2$requester_received_pizza == "true")] <- 1

cor(as.integer(train2$requester_received_pizza),
    train2$requester_number_of_comments_in_raop_at_retrieval)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Create a corpusPizza on the request_text variable, and split in train and validation subsets:

library(tm)
# Split in train and validation sets:
corpusPizza = Corpus(VectorSource(train$request_text))
corpusPizza[[1]]
# Pre-process data
corpusPizza = tm_map(corpusPizza, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function
corpusPizza = tm_map(corpusPizza, PlainTextDocument)
corpusPizza = tm_map(corpusPizza, removePunctuation)
corpusPizza = tm_map(corpusPizza, removeWords, stopwords("english"))
corpusPizza = tm_map(corpusPizza, stemDocument)
corpusPizza[[1]]
# Create matrix
dtmPizza = DocumentTermMatrix(corpusPizza)
dtmPizza
str(dtmPizza) # Lots of terms...
# Remove sparse terms (leave about 700  terms or so?)
dtmSparse = removeSparseTerms(dtmPizza, 0.98)
dtmSparse
# Create data frame
labeledTerms = as.data.frame(as.matrix(dtmSparse))
rownames(labeledTerms) <- NULL
head(labeledTerms, n=1)
# Add in the outcome variable
table(train$requester_received_pizza)
labeledTerms$requester_received_pizza <- ifelse(train$requester_received_pizza == "false", 0, 1)
table(labeledTerms$requester_received_pizza)
str(labeledTerms)
# Split the data
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$requester_received_pizza, 0.7)
train2 = subset(labeledTerms, spl == TRUE)
test2 = subset(labeledTerms, spl == FALSE)
# Build a CART model
library(rpart)
library(rpart.plot)
pizzaCART = rpart(as.factor(requester_received_pizza) ~ ., data=train2, method="class", na.action=na.omit)
# NOTE: cp parameter seems to be important here!
pizzaCART = rpart(requester_received_pizza ~ ., data=train2, na.action=na.omit, minbucket=5, cp=0.001)
prp(pizzaCART, cex=.6, col="blue", main="Requester received pizza")
# Make predictions on the train set
pred = predict(pizzaCART)
# Make predictions on the test set
pred = predict(pizzaCART, newdata=test2)
pred[1:10]
pred.prob = pred[,2]
# Compute accuracy
result <- table(train2$requester_received_pizza, pred >= 0.5)
result <- table(test2$requester_received_pizza, pred >= 0.5)
result
score <- sum(diag(result)) / sum(result) 
score

# Create a corpusPizza on the request_text variable, model on train set and predict on test set:

# TODO:
# -First combine the request_text from text and train into ONE data frame
# -Then create corpus
# -Then split the combined corpus into test and train again
# -Then add outcome var to train set

library(tm)
corpusPizzaTrain = Corpus(VectorSource(c(train$request_text, test$request_text))) # NOTE: Combined train and test
corpusPizzaTrain = tm_map(corpusPizzaTrain, tolower)
corpusPizzaTrain = tm_map(corpusPizzaTrain, PlainTextDocument)
corpusPizzaTrain = tm_map(corpusPizzaTrain, removePunctuation)
corpusPizzaTrain = tm_map(corpusPizzaTrain, removeWords, stopwords("english"))
corpusPizzaTrain = tm_map(corpusPizzaTrain, stemDocument)
corpusPizzaTrain[[1]]
# Create matrix
dtmPizzaTrain = DocumentTermMatrix(corpusPizzaTrain)
dtmPizzaTrain
# Remove sparse terms (leave about 700  terms or so? TODO: Tune this and validate on subsets!)
dtmSparseTrain = removeSparseTerms(dtmPizzaTrain, 0.9) # Was 0.98->0.993, go opposite way??
dtmSparseTrain
# Create data frame
labeledTermsCombined = as.data.frame(as.matrix(dtmSparseTrain))
dim(labeledTermsCombined)
rownames(labeledTermsCombined) <- NULL
head(labeledTermsCombined, n=1)
# Split the combined corpus into train and test (TODO: Is this split correct????)
labeledTermsTrain <- labeledTermsCombined[1:nrow(train),]
labeledTermsTest <- labeledTermsCombined[(nrow(train)+1):nrow(labeledTermsCombined),]
dim(labeledTermsTrain)
dim(labeledTermsTest)

# Add in the outcome variable to the train set
labeledTermsTrain$requester_received_pizza <- ifelse(train$requester_received_pizza == "false", 0, 1)
table(labeledTermsTrain$requester_received_pizza)
str(labeledTermsTrain)

# Try a good ol' GLM on this corpus too. Not so good here...
pizzaGLM2 <- glm(as.factor(requester_received_pizza) ~ ., data=labeledTermsTrain, na.action=na.omit,
                 family=binomial(link="logit"))
summary(pizzaGLM2)

# Build a CART model
library(rpart)
library(rpart.plot)
# NOTE: cp parameter seems to be important here!
pizzaCART2 = rpart(requester_received_pizza ~ ., data=labeledTermsTrain, na.action=na.omit, minbucket=5, cp=0.001)
prp(pizzaCART2, cex=.6, col="blue", main="Requester received pizza")
# Make predictions on the train set
predTrain = predict(pizzaCART2)
predTrainLog = predict(pizzaGLM2)
plot(sort(plogis(predGLMTrain)))
# TODO: Make predictions on the combined train and test set
predTrainCombined = predict(pizzaCART2, newdata=labeledTermsCombined)
predTrainLogCombined = predict(pizzaGLM2, newdata=labeledTermsCombined)
# Make predictions on the test set
predTest = predict(pizzaCART2, newdata=labeledTermsTest)
predTestLog = predict(pizzaGLM2, newdata=labeledTermsTest)
predTrain[1:10]
predTrainCombined[1:10]
predTest[1:10]
#predTrain.prob = predTrain[,2]
#predTest.prob = predTest[,2]
# Compute accuracy
result2 <- table(labeledTermsTrain$requester_received_pizza, predTrain[,2] > 0.5)
result2
score2 <- sum(diag(result2)) / sum(result2) 
score2
par(mfrow=c(2,1))
plot(sort(predTrain[,2]), type="l", col="blue", main="Prediction train")
plot(sort(predTest[,2]), type="l", col="red", main="Prediction test")
par(mfrow=c(1,1))

# Create submission file
submission <- data.frame(request_id=test$request_id, requester_received_pizza=predTest[,2])
head(submission)
write.csv(submission, file=paste0(submissionsFolder, "rpart_benchmark_",
                                  format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------
# TODO: Try a h2o RF on the cleaned up train and test corpuses
