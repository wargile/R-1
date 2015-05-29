# The Analytics Edge 2015 - Final Exam
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/24da2473ff184b05ac5abadcfe90f4fe/925c60eee73a48b7a118d41be4bf91c5/
# ------------------------------------------------------------------------------------

# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")

set.seed(1000)

source("tools.R")
library(caret)
library(randomForest)
library(gbm)
library(stats)
library(ROCR)
library(readr)
library(caTools)
library(wordcloud)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

SetStandardOptions()

# ------------------------------------------------------------------------------------------------------------------------------------
# EXAM PART 1): FORECASTING AIRLINE DELAYS
# ----------------------------------------
# On any given day, more than 87,000 flights take place in the United States alone. About one-third of these flights are
# commercial flights, operated by companies like United, American Airlines, and JetBlue. While about 80% of commercial
# flights take-off and land as scheduled, the other 20% suffer from delays due to various reasons. A certain number of
# delays are unavoidable, due to unexpected events, but some delays could hopefully be avoided if the factors causing
# delays were better understood and addressed.
#
# In this problem, we'll use a dataset of 9,381 flights that occured in June through August of 2014 between the three busiest
# US airports -- Atlanta (ATL), Los Angeles (LAX), and Chicago (ORD) -- to predict flight delays. The dataset AirlineDelay.csv
# includes the following 23 variables:
# Flight = the origin-destination pair (LAX-ORD, ATL-LAX, etc.)
# Carrier = the carrier operating the flight (American Airlines, Delta Air Lines, etc.)
# Month = the month of the flight (June, July, or August)
# DayOfWeek = the day of the week of the flight (Monday, Tuesday, etc.)
# NumPrevFlights = the number of previous flights taken by this aircraft in the same day
# PrevFlightGap = the amount of time between when this flight's aircraft is scheduled to arrive at the airport and when it's scheduled to depart for this flight
# HistoricallyLate = the proportion of time this flight has been late historically
# InsufficientHistory = whether or not we have enough data to determine the historical record of the flight (equal to 1 if we don't have at least 3 records, equal to 0 if we do)
# OriginInVolume = the amount of incoming traffic volume at the origin airport, normalized by the typical volume during the flight's time and day of the week
# OriginOutVolume = the amount of outgoing traffic volume at the origin airport, normalized by the typical volume during the flight's time and day of the week
# DestInVolume = the amount of incoming traffic volume at the destination airport, normalized by the typical volume during the flight's time and day of the week
# DestOutVolume = the amount of outgoing traffic volume at the destination airport, normalized by the typical volume during the flight's time and day of the week
# OriginPrecip = the amount of rain at the origin over the course of the day, in tenths of millimeters
# OriginAvgWind = average daily wind speed at the origin, in miles per hour
# OriginWindGust = fastest wind speed during the day at the origin, in miles per hour
# OriginFog = whether or not there was fog at some point during the day at the origin (1 if there was, 0 if there wasn't)
# OriginThunder = whether or not there was thunder at some point during the day at the origin (1 if there was, 0 if there wasn't)
# DestPrecip = the amount of rain at the destination over the course of the day, in tenths of millimeters
# DestAvgWind = average daily wind speed at the destination, in miles per hour
# DestWindGust = fastest wind speed during the day at the destination, in miles per hour
# DestFog = whether or not there was fog at some point during the day at the destination (1 if there was, 0 if there wasn't)
# DestThunder = whether or not there was thunder at some point during the day at the destination (1 if there was, 0 if there wasn't)
# TotalDelay = the amount of time the aircraft was delayed, in minutes (this is our dependent variable)

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/FinalExam/"

# PROBLEM 1 - LOADING THE DATA. Answer: 1) Rows in train: 6566  2) Rows in test: 2815
Airlines <- read.csv(paste0(folder, "AirlineDelay.csv"))
str(Airlines)
dim(Airlines)
summary(Airlines)
View(Airlines)
names(Airlines)
cor(Airlines)
CorrelationPlot(Airlines)
set.seed(15071)
spl = sample(nrow(Airlines), 0.7 * nrow(Airlines))
AirlinesTrain = Airlines[spl,]
AirlinesTest = Airlines[-spl,]
nrow(AirlinesTrain)
nrow(AirlinesTest)

par(mar=c(6,3,2,1))
plot(sort(Airlines$TotalDelay))
with(Airlines, plot(TotalDelay ~ Carrier, main="Total Delay", col="orange", las=2, xlab=""))
with(Airlines, barplot(tapply(HistoricallyLate, Carrier, mean), las=2, main="Airlines", col=2:length(unique(Carrier))))
par(mar=c(3,3,2,1))

# PROBLEM 2 - METHOD OF SPLITTING THE DATA. Answer: See below.
# In this class, we have frequently used the sample.split function to randomly split our data. Why do we use a different approach here?
# Answer: The sample.split function is typically used to split data with a categorical dependent variable, and we have a
# continuous dependent variable.

# PROBLEM 3 - A LINEAR REGRESSION MODEL. Answer: Multiple R squared = 0.0948
lm.fit <- lm(TotalDelay ~., data=AirlinesTrain)
summary(lm.fit)

# PROBLEM 4 - CHECKING FOR SIGNIFICANCE. Answer: Significant vars: See model

# PROBLEM 5 - CORRELATIONS. Answer: 1) cor: -0.6521  2) cor: 0.51 
with(AirlinesTrain, cor(NumPrevFlights, PrevFlightGap))
with(AirlinesTrain, cor(OriginAvgWind, OriginWindGust))

# PROBLEM 6 - IMPORTANCE OF CORRELATIONS. Answer: See below.
# Why is it imporant to check for correlations between independent variables? 
# Answer: Having highly correlated independent variables in a regression model can affect the interpretation of the coefficients

# PROBLEM 7 - COEFFICIENTS. Answer: Coeff = 47.91364
# In the model with all of the available independent variables, what is the coefficient for HistoricallyLate?

# PROBLEM 8 - UNDERSTANDING THE COEFFICIENTS. Answer: See below.
# The coefficient for NumPrevFlights is 1.56. What is the interpretation of this coefficient?
# (REMEMBER: In regression with multiple independent variables, the coefficient tells you how much the dependent variable is
# expected to increase when that independent variable increases by one, holding all the other independent variables constant.)
# Answer: For an increase of 1 in the number of previous flights, the prediction of the total delay increases by approximately 1.56.

# PROBLEM 9 - UNDERSTANDING THE MODEL. Answer: 1) 6.99  2) 0.9114
# 1) In the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted
#    total delay given that one flight is on Thursday and the other is on Sunday?
DayOfWeekThursday <- 1.57150
DayOfWeekSunday <- -5.41836
abs(DayOfWeekThursday - DayOfWeekSunday)
# 2) In the linear regression model, given two flights that are otherwise identical, what is the absolute difference in predicted
#    total delay given that one flight is on Saturday and the other is on Sunday?
DayOfWeekSaturday <- -4.50694
abs(DayOfWeekSaturday - DayOfWeekSunday)
# TIP: Look at ?contrasts, ?C, ?contr.sum, and the contrasts part of ?options. type="terms" on prediction. 
# http://www.r-bloggers.com/interpreting-interaction-coefficient-in-r-part1-lm/
  
# PROBLEM 10 - PREDICTIONS ON THE TEST SET. Answer: 1) SSE = 4744764   2) SST = 5234023  3) R2 = 0.0934
predict.lm <- predict(lm.fit, AirlinesTest)
SSE <- sum((AirlinesTest$TotalDelay - predict.lm)^2) 
SSE
# HINT Remember to use the mean total delay on the training set as the "baseline model".
SST <- sum((AirlinesTest$TotalDelay - mean(AirlinesTrain$TotalDelay))^2) 
SST
R2 <- 1 - (SSE / SST)
R2 # OK! Compare to output from summary(model) 

# PROBLEM 11 - EVALUATING THE MODEL. Answer: See below.
# Given what you have seen about this model (the R-squared on the training and test sets, the significance of the coefficients,
# etc.), which of the following are true? Select all that apply.
# Answer: Since our R-squared values are low, we can conclude that our independent variables only explain a small amount of the
# variation in the dependent variable.

# PROBLEM 12 - A CLASSIFICATION PROBLEM. Answer: See below.
# Let's turn this problem into a multi-class classification problem by creating a new dependent variable:
Airlines$DelayClass = factor(ifelse(Airlines$TotalDelay == 0, "No Delay",
                                    ifelse(Airlines$TotalDelay >= 30, "Major Delay", "Minor Delay")))
Airlines$TotalDelay = NULL # Remove the original dependent var
table(Airlines$DelayClass)
set.seed(15071)
spl = sample.split(Airlines$DelayClass, SplitRatio=0.7)
AirlinesTrain <- subset(Airlines, spl==T)
AirlinesTest <- subset(Airlines, spl==F)
nrow(AirlinesTrain)
nrow(AirlinesTest)
# 1) How many flights in the dataset Airlines had no delay? Answer: 4688
# 2) How many flights in the dataset Airlines had a minor delay? 3096
# 3) How many flights in the dataset Airlines had a major delay? 1597

# PROBLEM 13 - A CART MODEL. Answer: 1 split
# How many split are in the resulting tree?
fit.rpart <- rpart(DelayClass ~., data=AirlinesTrain)
summary(fit.rpart)
prp(fit.rpart)

# PROBLEM 14 - UNDERSTANDING THE MODEL. Answer: Major Delay
# The CART model you just built never predicts one of the three outcomes. Which one?
summary(fit.rpart)

# PROBLEM 15 - TRAINING SET ACCURACY. Answer: 0.5261
predict.rpart.train <- predict(fit.rpart, AirlinesTrain, type="class")
result.train <- table(AirlinesTrain$DelayClass, predict.rpart.train)
result.train
accuracy.train <- sum(diag(result.train)) / nrow(AirlinesTrain)
accuracy.train

# PROBLEM 16 - A BASELINE MODEL. Answer: Baseline Accuracy = 0.4998
# What is the accuracy on the training set of a baseline model that predicts the most frequent outcome (No Delay) for all observations?
result.train <- table(AirlinesTrain$DelayClass)
result.train
accuracy.train <- result.train[3] / nrow(AirlinesTrain)
accuracy.train

# PROBLEM 17 - TESTING SET ACCURACY. Answer: Accuracy on test set: 0.5167
# Make predictions on the testing set, and then create a confusion matrix. What is the overall accuracy of the model on the testing set?
predict.rpart.test <- predict(fit.rpart, AirlinesTest, type="class")
result.test <- table(AirlinesTest$DelayClass, predict.rpart.test)
result.test
accuracy.test <- sum(diag(result.test)) / nrow(AirlinesTest)
accuracy.test

# PROBLEM 18 - UNDERSTANDING THE MODEL. Answer: See below.
# What can you conclude from the CART model? Select all that apply.
# Answer: 1) Out of the independent variables in our dataset, the best predictor of future delays is historical delays.
#         2) While delays are hard to predict, using historical data can be helpful.


# ------------------------------------------------------------------------------------------------------------------------------------
# EXAM PART 2): PREDICTING SALES ON EBAY
# --------------------------------------

# To determine whether analytics can be used to help make this choice, we will look at whether data from previous auctions on eBay,
# a major online auction and shopping site, can be used to predict whether a new item will be sold at some target price. We will
# limit our attention to Christian Louboutin shoes, using data from nearly 4,000 auctions from late 2014. In this analysis, the
# dependent variable will be the binary outcome variable sold, which takes value 1 if the item was sold and 0 if it was not sold.
# We also include saleprice, which is the price the shoe sold at (NA for shoes that did not sell). For each item, the file ebay.csv
# contains the following independent variables:
# biddable: Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0)
# startprice: The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0)
# condition: The condition of the shoe (New with box, New with defects, New without box, or Pre-owned)
# size: The size of the shoe (converted to US shoe sizes)
# heel: The size of the heel (Flat, Low, Medium, High)
# style: The style of the shoe (Open Toe, Platform, Pump, Slingback, Stiletto, or Other/Missing)
# color: The color of the shoe (Beige, Black, Brown, Red, or Other/Missing)
# material: The material of the shoe (Leather, Patent Leather, Satin, Snakeskin, Suede, or Other/Missing)
# snippit: A short snippit of text describing the shoe
# description: A long text description describing the shoe

eBay <- read.csv(paste0(folder, "ebay.csv"))

# PROBLEM 1 - LOADING THE DATA. Answer: 0.2105
# What proportion of all shoes were sold?
str(eBay)
summary(eBay)
names(eBay)
CorrelationPlot(eBay[, -2])
table(eBay$sold)[2] / nrow(eBay)

# PROBLEM 2 - MISSING VALUES. Answer: size
# Which of the numerical variables has at least one missing value?
table(is.na(eBay$biddable))
table(is.na(eBay$sold))
table(is.na(eBay$startprice))
table(is.na(eBay$size)) # NA

# PROBLEM 3 - MOST COMMON SHOE SIZE. Answer: Size 8
barplot(table(eBay$size), main="eBay Shoesize", col="orange")

# PROBLEM 4 - CONVERTING VARIABLES TO FACTORS. Answer: Logistic regression (glm)?? randomForest is correct(??)
# Which of the following methods requires the dependent variable be stored as a factor variable when training a model
# for classification?
eBay$sold <- as.factor(eBay$sold)
eBay$condition <- as.factor(eBay$condition)
eBay$heel <- as.factor(eBay$heel)
eBay$style <- as.factor(eBay$style)
eBay$color <- as.factor(eBay$color)
eBay$material <- as.factor(eBay$material)

# PROBLEM 5 - SPLITTING INTO A TRAINING AND TESTING SET. Answer: It balances the dependent variable between
# the training and testing sets
# Why do we use the sample.split() function to split into a training and testing set?
set.seed(144)
library(caTools)
spl <- sample.split(eBay$sold, 0.7)
eBay.train <- subset(eBay, spl==T)
eBay.test <- subset(eBay, spl==F)

# PROBLEM 6 - TRAINING A LOGISTIC REGRESSION MODEL. Answer: The item having a high starting price
fit.glm <- glm(sold ~ biddable+startprice+condition+heel+style+color+material, data=eBay.train, family=binomial)
summary(fit.glm)
# Which of the following characteristics of a shoe are statistically significantly (p < 0.05, aka at least a * in the
# regression summary) associated with a lower chance of an item being sold?

# PROBLEM 7 - PREDICTING USING A LOGISTIC REGRESSION MODEL. Answer: 0.2492
# Consider a shoe that is not for auction (biddable=0), that has start price $100, that is in condition "Pre-owned",
# that has "High" heels, that has style "Open Toe", that has color "Black", and that has material "Satin". What is the
# predicted probability that this shoe will be sold according to the logistic regression model?
intercept <- 0.599079
startprice <- -0.004442
conditionPreOwned <- -0.495298
heelHigh <- 0.122426
styleOpenToe <- 0 # NOTE: "open toe" is default level for factor var style
colorBlack <- 0.222655
materialSatin <- -1.107810
1 / (1 + exp(-1)^(intercept + (100 * startprice) + conditionPreOwned + heelHigh + styleOpenToe + colorBlack + materialSatin))

# PROBLEM 8 - INTERPRETING MODEL COEFFICIENTS. Answer: See below.
# What is the meaning of the coefficient labeled "styleStiletto" in the logistic regression summary output?
summary(fit.glm)
#                            Estimate Std. Error z value Pr(>|z|)    
# styleStiletto              0.832541   0.260679    3.19  0.00140 ** 
# Answer: Stilettos are predicted to have 129.9% higher odds of being sold than an otherwise identical open-toed shoe.
(exp(0.832541) * 100) - 100 # ??

# PROBLEM 9 - OBTAINING TEST SET PREDICTIONS. Answer: 22 + 58 = 80
# On how many test set observations does your logistic regression model make a different prediction than the prediction
# the naive baseline model would make? (Remember that the naive baseline model we use in this class always predicts the
# most frequent outcome in the training set for all observations in the test set.)
predict.glm <- predict(fit.glm, eBay.test, type="response") # IMPORTANT: Use type="response" here!
table(eBay.test$sold) # Baseline, most frequent outcome (not sold)
result <- table(eBay.test$sold, predict.glm > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy # Accuracy on test set: 0.8209

# PROBLEM 10 - COMPUTING TEST-SET AUC. Answer: AUC on test set: 0.7444
pred <- prediction(predict.glm, eBay.test$sold)
as.numeric(performance(pred, "auc")@y.values)

# PROBLEM 11 - COMPUTING TEST-SET AUC. Answer: See below.
# What is the meaning of the AUC?
# Answer: The proportion of the time the model can differentiate between a randomly selected shoe that was sold
# and a randomly selected shoe that was not sold.

# PROBLEM 12 - ROC CURVES. Answer: 0
# Which logistic regression threshold is associated with the upper-right corner of the ROC plot
# (true positive rate 1 and false positive rate 1)?

# PROBLEM 13 - ROC CURVES. Answer: At 0.16
perfROCR <- performance(pred, "tpr", "fpr")
par(mar=c(3,3,2,2))
plot(perfROCR, colorize=TRUE, main="ROCR on eBay.test$sold", lwd=3)
par(mar=c(3,3,2,1))
# At roughly which logistic regression cutoff does the model achieve a true positive rate of 80% and a false
# positive rate of 50%?

# PROBLEM 14 - CROSS-VALIDATION TO SELECT PARAMETERS. Answer: See below.
# Which of the following best describes how 10-fold cross-validation works when selecting between 3 different parameter values?
# Answer: 30 models are trained on subsets of the training set and evaluated on a portion of the training set
# Explanation: In 10-fold cross validation, the model with each parameter setting will be trained on 10 90% subsets of
# the training set. Hence, a total of 30 models will be trained. The models are evaluated in each case on the last 10%
# of the training set (not on the testing set).

# PROBLEM 15 - CROSS-VALIDATION FOR A CART MODEL. Answer: cp = 0.007
# What cp value maximizes the cross-validation accuracy?
set.seed(144)
numFolds <- trainControl(method="cv", number=10)
cpGrid <- expand.grid(.cp=seq(0.001, 0.05, 0.001))
train(sold ~ biddable+startprice+condition+heel+style+color+material, data=eBay.train,
      method="rpart", trControl=numFolds, tuneGrid=cpGrid) # Get cp param at end

# PROBLEM 16 - TRAIN CART MODEL. Answer: startprice
# What variable is used most frequently as a split in the tree?
eBay.CART <- rpart(sold ~ biddable+startprice+condition+heel+style+color+material,
                   data=eBay.train, method="class", cp=0.007)
prp(eBay.CART ,col="blue", cex=.7)
summary(eBay.CART)

# PROBLEM 17 - BUILDING A CORPUS FROM ITEM DESCRIPTIONS. Answer: 10770 terms in DTM
# Build a document-term matrix called "dtm" from the preprocessed corpus. How many unique word stems are in dtm?
library(tm)
# Create corpus
corpus <- Corpus(VectorSource(eBay$description))
# Pre-process data, lowercase:
corpus <- tm_map(corpus, tolower)
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before
# continuing (it converts corpus to a Plain Text Document):
corpus <- tm_map(corpus, PlainTextDocument)
# Remove punctuation:
corpus <- tm_map(corpus, removePunctuation)
# Remove stopwords:
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Stem the document:
corpus <- tm_map(corpus, stemDocument)
# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm

# PROBLEM 18 - REMOVING SPARSE TERMS. Answer: 144 unique terms in spdtm
# Remove all terms that don't appear in at least 10% of documents in the corpus, storing the result in a new document
# term matrix called spdtm. How many unique terms are in spdtm?
threshold <- 0.9
# Remove sparse terms
dtm <- removeSparseTerms(dtm, threshold)
dtm

# PROBLEM 19 - EVALUATING WORD FREQUENCIES IN A CORPUS. Answer: "ship" is the most frequent term 
# Convert spdtm to a data frame called descriptionText. Which word stem appears the most frequently across all descriptions?
# Create data frame
descriptionText <- as.data.frame(as.matrix(dtm))
barplot(sort(colSums(descriptionText), decreasing=T)[1:10], main="Most frequent terms in DTM", col="wheat")
# colnames(descriptionText) <- make.names(colnames(descriptionText))

# PROBLEM 20 - ADDING DATA FROM ORIGINAL DATA FRAME. Answer: Variables in testText: 152 (153 is the correct answer??)
names(descriptionText) <- paste0("D", names(descriptionText))
descriptionText$sold <- eBay$sold
descriptionText$biddable <- eBay$biddable
descriptionText$startprice <- eBay$startprice
descriptionText$condition <- eBay$condition
descriptionText$heel <- eBay$heel
descriptionText$style <- eBay$style
descriptionText$color <- eBay$color
descriptionText$material <- eBay$material
trainText <- subset(descriptionText, spl==T)
testText <- subset(descriptionText, spl==F)
dim(testText)

# PROBLEM 21 - TRAINING ANOTHER LOGISTIC REGRESSION MODEL. Answer: 13
# Using trainText, train a logistic regression model called glmText to predict the dependent variable using all other
# variables in the data frame:
glm.fit <- glm(sold ~., data=trainText, family=binomial)
summary(glm.fit)
# How many of the word frequencies from the description text (variables beginning with the letter "D") are significant at
# or below the p=0.05 level? (This is the total number of independent variables beginning with the letter "D" that have at
# least one star.)

# PROBLEM 22 - TEST SET AUC OF NEW LOGISTIC REGRESSION MODEL. Answer: 1) 0.8191  2) 0.7338
# What is the training-set AUC of the new logistic regression model?
pred.train <- predict(glm.fit, trainText)
prediction.train <- prediction(pred.train, trainText$sold)
as.numeric(performance(prediction.train, "auc")@y.values)
# What is the test-set AUC of the new logistic regression model?
pred.test <- predict(glm.fit, testText)
prediction.test <- prediction(pred.test, testText$sold)
as.numeric(performance(prediction.test, "auc")@y.values)

# PROBLEM 23 - ASSESSING OVERFITTING OF NEW MODEL. Answer: glmText is overfitted, and removing variables would improve its test-set performance.
# Explanation: glmText has more variables than the base logistic regression model, but it exhibits worse test-set performance
# (AUC of 0.734 vs. 0.744). Therefore, it is overfitted and removing variables would improve the test-set performance.


# ------------------------------------------------------------------------------------------------------------------------------------
# EXAM PART 3): UNDERSTANDING CUSTOMERS OF HUBWAY
# -----------------------------------------------
# Data from: http://www.thehubway.com/

# In Unit 6, we saw how clustering can be used for market segmentation, the idea of dividing a broad target market of customers
# into smaller, more similar groups, and then designing a marketing strategy specifically for each group. In this problem,
# we'll see how the same idea can be applied using data from Hubway, a bike-sharing program in the Boston, Massachusetts area.
# Registered users of Hubway can check-out a bicycle from one of 140 stations located throughout the Metro-Boston area, and
# return the bike to any of the 140 stations. This enables users to take bikes on one-way trips throughout the city. Users pay
# a membership fee, which includes unlimited trips up to 30 minutes in duration at no additional cost. Trips longer than 30
# minutes cost additional "overtime" fees. 

# In this problem, we'll use the dataset HubwayTrips.csv, which contains data from trips taken by registered users of Hubway
# from June 2012 through September 2012. The dataset contains the following seven variables:
# Duration = the time of the trip, in seconds
# Morning = whether or not the trip started in the morning, between the hours of 6:00am and 12:00pm (1 if yes, 0 if no)
# Afternoon = whether or not the trip started in the afternoon, between the hours of 12:00pm and 6:00pm (1 if yes, 0 if no)
# Evening = whether or not the trip started in the evening, between the hours of 6:00pm and 12:00am (1 if yes, 0 if no)
# Weekday = whether or not the trip started on Monday, Tuesday, Wednesday, Thursday, or Friday (1 if yes, 0 if no)
# Male = whether or not the user was male (1 if yes, 0 if no)
# Age = the age of the user, in years

HubwayTrips <- read.csv(paste0(folder, "HubwayTrips.csv"))
str(HubwayTrips)
CorrelationPlot(HubwayTrips)

# PROBLEM 1 - READING IN THE DATA. Answer: 185190
# How many observations are in this dataset?
nrow(HubwayTrips)

# PROBLEM 2 - AVERAGE DURATION. Answer: 1) 721.6  2) 700.1  3) 826.2
# 1) What is the average duration (in seconds) of all trips in this dataset?
mean(HubwayTrips$Duration)
# 2) What is the average duration (in seconds) of trips taken on the weekdays?
tapply(HubwayTrips$Duration, HubwayTrips$Weekday, mean)
# 3) What is the average duration (in seconds) of trips taken on the weekends?
tapply(HubwayTrips$Duration, HubwayTrips$Weekday, mean)

# PROBLEM 3 - TIME OF DAY. Answer: 1) 60399  2) 74021  3) 46264
# 1) How many trips were taken in the morning?
table(HubwayTrips$Morning)
# 2) How many trips were taken in the afternoon?
table(HubwayTrips$Afternoon)
# 3) How many trips were taken in the evening?
table(HubwayTrips$Evening)

# PROBLEM 4 - GENDER DISTRIBUTION. Answer: 0.7371
# In this dataset, what proportion of trips are taken by male users?
table(HubwayTrips$Male)[2] / nrow(HubwayTrips)

# PROBLEM 5 - IMPORTANCE OF NORMALIZING. Answer: Duration
# When clustering data, it is often important to normalize the variables so that they are all on the same scale.
# If you clustered this dataset without normalizing, which variable would you expect to dominate in the distance calculations?
summary(HubwayTrips) # Duration has largest scale (min=180, max=85040)

# PROBLEM 6 - NORMALIZING THE DATA. Answer: 1) 67.47  2) 3.877
library(caret)
preproc <- preProcess(HubwayTrips)
HubwayNorm <- predict(preproc, HubwayTrips)
# (Remember that for each variable, the normalization process subtracts the mean and divides by the standard deviation.
# We learned how to do this in Unit 6.) In your normalized dataset, all of the variables should have mean 0 and standard
# deviation 1.
# 1) What is the maximum value of Duration in the normalized dataset?
max(HubwayNorm$Duration)
# 2) What is the maximum value of Age in the normalized dataset?
max(HubwayNorm$Age)

# PROBLEM 7 - HIERARCHICAL CLUSTERING. Answer: See below
# Why is hclust not good for this dataset?
# Answer: We might have too many observations in our dataset for Hierarchical clustering to handle
# (NOTE: hclust handles categorical vars OK!)

# PROBLEM 8 - K-MEANS CLUSTERING. Answer: 1) 9720 (cluster 5)  2) 36409 (cluster 2)
set.seed(5000)
# Run the k-means clustering algorithm on your normalized dataset, selecting 10 clusters:
kmeans.clusters <- kmeans(HubwayNorm, centers=10)
summary(kmeans.clusters)
myClusters <- kmeans.clusters$cluster
sort(table(myClusters))
# 1) How many observations are in the smallest cluster?
# 2) How many observations are in the largest cluster?

# PROBLEM 9 - UNDERSTANDING THE CLUSTERS. Answer: Cluster 10
# Which cluster best fits the description "trips taken by female users on weekday evenings"?
clusterList.km <- lapply(1:10, function(x) subset(HubwayTrips, kmeans.clusters$cluster == x))
kmeans.clusters$centers

# PROBLEM 10 - UNDERSTANDING THE CLUSTERS. Answer: Cluster 8
# Which cluster best fits the description "leisurely (longer than average) afternoon trips taken on the weekends"?

# PROBLEM 11 - UNDERSTANDING THE CLUSTERS. Answer: Cluster 4
# Which cluster best fits the description "morning trips taken by older male users"?

# PROBLEM 12 - RANDOM BEHAVIOR. Answer: See below.
# 1) If we ran k-means clustering a second time without making any additional calls to set.seed, we would expect:
#    Answer: Different results from the first k-means clustering
# 2) If we ran k-means clustering a second time, again running the command set.seed(5000) right before doing the clustering,
#    we would expect:
#    Answer: Identical results to the first k-means clustering
# 3) If we ran k-means clustering a second time, running the command set.seed(4000) right before doing the clustering,
#    we would expect:
#    Answer: Different results from the first k-means clustering

# PROBLEM 13 - THE NUMBER OF CLUSTERS. Answer: Decrease the number of clusters
# Suppose the marketing department at Hubway decided that the 10 clusters were too specific, and they wanted more general
# clusters to describe the Hubway user base. Would they want to increase or decrease the number of clusters?

# PROBLEM 14 - INCREASING THE NUMBER OF CLUSTERS. Answer: 1) 99 (cluster 20)  2) 25225 (cluster 11)
set.seed(8000)
# Run the k-means clustering algorithm on your normalized dataset, selecting 20 clusters:
kmeans.clusters <- kmeans(HubwayNorm, centers=20)
myClusters <- kmeans.clusters$cluster
sort(table(myClusters))
# 1) How many observations are in the smallest cluster?
# 2) How many observations are in the largest cluster?

# PROBLEM 15 - DESCRIBING THE CLUSTERS. Answer: Cluster 7 and 13 (not 1,7,12,13??)
# Which clusters can be described as "shorter than average trips that occur on weekday evenings"? Select all that apply.
clusterList.km <- lapply(1:20, function(x) subset(HubwayTrips, kmeans.clusters$cluster == x))
kmeans.clusters$centers
mean.duration <- mean(kmeans.clusters$centers[, "Duration"]) # Mean of Duration
mean.duration
mean.weekday <- mean(kmeans.clusters$centers[, "Weekday"]) # Mean of Weekday
mean.weekday
mean.evening <- mean(kmeans.clusters$centers[, "Evening"]) # Mean of Evening
mean.evening
rows <- which((kmeans.clusters$centers[, "Duration"] < mean.duration) & (kmeans.clusters$centers[, "Weekday"] > mean.weekday) &
                (kmeans.clusters$centers[, "Evening"] > mean.evening))
kmeans.clusters$centers[rows, ]

# PROBLEM 16 - UNDERSTANDING CENTROIDS. Answer: See below.
# Why do we typically use cluster centroids to describe the clusters?
# Answer: The cluster centroid shows average behavior in a single cluster - it does not describe every single observation in
# that cluster or tell us how the cluster compares to other clusters.

# PROBLEM 17 - USING A VISUALIZATION. Answer: See below.
# Which of the following visualizations could be used to observe the distribution of Age, broken down by cluster?
boxplot(HubwayTrips$Age ~ kmeans.clusters$cluster, main="Age by cluster", col="orange")
# Answer: 1) A box plot of the variable Age, subdivided by cluster
# Answer: 4) ggplot with Age on the x-axis and the cluster number on the y-axis, plotting with geom_point()



# ------------------------------------------------------------------------------------------------------------------------------------
# EXAM PART 4): OPTIMAL PRODUCTION SCHEDULING
# -------------------------------------------

# http://en.wikipedia.org/wiki/Die_casting

# Falcon Die Casting Company (FDC) is an automotive parts manufacturer based in the United States. FDC uses an innovative
# method of high volume die casting, a metal casting process that is characterized by forcing molten metal under high
# pressure into a mold cavity. Due to the strength of their method, FDC recently received a long-term contract from a major
# automobile manufacturer to produce the five key die cast items used in most of their automobiles.

# This problem is based on the case study "Production Scheduling at Falcon Die Casting"
# by B. Madhu Rao and Jeroen Belien, INFORMS Transactions on Education 15(1), p.154-155, 2014.

# See: DieCasting.ods in the exam folder for the LibreOffice Calc spreadsheep with problem desctiptions, etc.

# PROBLEM 1 - THE DECISION VARIABLES. Answer: 20
# As mentioned above, the production manager needs to decide the number of non-overtime and overtime hours to use each machine
# to manufacture each part (for a single week). These are the decisions for our optimization problem.
# How many decision variables will there be in your optimization formulation? (HINT: Remember that not all parts can be
# produced on all machines, so you might not need a decision variable for each part/machine pair.)
# Explanation: You should have two decision variables (the number of non-overtime and overtime hours) for each feasible
# part/machine pair. There are 10 different part/machine possibilities (non-empty cells in the table in the introduction),
# so there are 20 decision variables.

# PROBLEM 2 - THE DECISION VARIABLES. Answer: Continuous variables limited to non-negative values
# What type of decision variables should be used in the optimization formulation?

# PROBLEM 3 - THE OBJECTIVE. Answer: 10
# The objective of the production manager is to minimize the total number of overtime machine-hours that need to be used.
# How many of the decision variables defined in Problem 1 have to do with the overtime shifts?
# Explanation: One of the decision variables for each machine/part pair indicates the number of overtime hours used by that
# machine to produce that part. You will want to minimize the sum of these decision variables.

# PROBLEM 4 - DEMAND CONSTRAINTS. Answer: See below.
# 1) The production manager needs to be sure that the weekly demand is met for each part. If we decide to produce Part 1 on
# Machine 1 for 20 non-overtime hours and 4 overtime hours and to produce Part 1 on Machine 2 for 32 non-overtime hours and
# 0 overtime hours, how many units of Part 1 will we produce?
M1 <- (20 * 40) + (4 * 40) # 960
M2 <- (32 * 35) + (0 * 35) # 1120
M1 + M2
# Answer: 1) 2080 units
# Denote the total number of units of Part 1 produced by N. In our optimization formulation, we need to include a constraint
# that N is at least how much?
# Answer: 2) 2450 units (the demand for part 1)
# Explanation for 1): Machine 1 produces Part 1 at 40 units/hour, so over 24 hours it will produce 40*24=960 units.
# Machine 2 produces Part 1 at 35 units/hour, so over 32 hours it will produce 35*32=1120 units. In total we will produce
# 960+1120=2080 units of Part 1. 

# PROBLEM 5 - CAPACITY CONSTRAINTS. Answer: 10
# No machine can work for more than 80 non-overtime hours or 40 overtime hours. How many total constraints do we need to add
# to the model to ensure this requirement is met?
# Explanation:
# For each of the 5 machines, we need one constraint to limit the total non-overtime hours to be no more than 80 and another
# constraint to limit the total overtime hours to be no more than 40. This is a total of 2*5=10 constraints.

# PROBLEM 6 - SOLVING THE MODEL. Answer: 0
# How many overtime machine-hours are used to produce the parts?
# Explanation: The optimal objective function value is 0, meaning that no overtime machine-hours are used to produce the parts.

# PROBLEM 7 - ROBUSTNESS TO MACHINE FAILURE. Answer: Machine 1, 2, 3 and 5
# Explanation: This can be determined by re-solving the model five times, each time forcing one of the machines to have 0 hours.

# PROBLEM 8 - QUALITY CONTROL. Answer: See below.
# Unfortunately, the FDC discovered that a significant percentage of the parts being produced are not at a quality level demanded by
# the customer. FDC's industrial engineers developed "yield factors", which are quite accurate in predicting the proportion of parts
# that meet customer quality specifications. The yield factors are as follows:
# Part 1 = 60%, Part 2 = 55%, Part 3 = 75%, Part 4 = 65%, Part 5 = 60%
# So, for example, only 60% of the Part 1 units produced meet the necessary quality standards, regardless of the machine that produced
# those units. FDC needs to ensure that they are producing enough high quality parts to meet the weekly customer demand.
# Adjust your optimization formulation to account for this requirement by requiring the number of high-quality parts produced to
# exceed the demand. You should still minimize the number of overtime machine-hours that are used.
# How many overtime machine-hours are now used in the optimal solution?
# Answer: 1) 44.157
# Which machines are used at their full overtime capacity (all 40 overtime hours) in the optimal solution? Select all that apply.
# Answer 2) Machine 4
# Explanation: In the five demand constraints, the number of products produced should be multiplied by the appropriate yield factors.

# PROBLEM 9 - SENSITIVITY ANALYSIS. Answer: Yes
# FDC's automotive partner proposes a 100 unit increase in the demand for Part 3, in exchange for compensation that FDC thinks is
# worth using up to 10 machine-hours of overtime (they would accept this proposal if it cost them less than 10 machines hours of
# overtime to produce the extra parts). Should FDC accept this proposal? Use your model from Problem 8 (the model that takes into
# account yield factors) when making the decision.
# Explanation: From changing the demand and re-solving the model, we see that the number of machine-hours of overtime increases by
# 2.8, less than the 10 machine-hours of value that the company derives from the proposal. Therefore the proposal should be accepted
# by FDC.
