# The Analytics Edge week 3 - Logistic Regression
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/logistic-regression/
# -----------------------------------------------------------------------------------------

library(scales)

SetStandardOptions()
# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 3/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# MODELING AN EXPERT: AN INTRODUCTION TO LOGISTIC REGRESSION
# ---------------------------------------------------------------

# Healthcare quality assessment: Identify poor healthcare (claims data for doctors, hospitals(?) and pharmacies) using analytics.
# Medical claims are generated when a patient visits a doctor. Includes diagnosis, procedures, doctor/hospital, costs.
# Pharmacy claims: Drug quantity/cost, prescribing doctor.
# Not 100% accurate. Under-reporting is common. electronically available, standardized (use well-established codes).
# Claims for hospital visits can be vague.

# Claims samlple:
# * Large health insurance claims database
# * Randomly selected 131 diabetes patients
# * Ages range from 35 to 55
# * Costs $10,000 - $20,000
# September 1, 2003 - August 31, 2005 

# Expert review (wrote comments on each claim), and expert assessment (rated quality on two-point scale, binary: poor 1 / good 0)

# Variable extraction:
# Dependent Variable: Quality of care
# Independent Variables: Diabetes treatment, Patient demographics, Healthcare utilization, Providers, Claims, Prescriptions

# Poor Care = 1
# Good Care = 0
# P(y = 0) = 1 - P(y = 1)
# Use the Logistic Response Function: 1 / (1 + e^-1(B0 + B1x1 + B2x2 + (..) + Bkxk)) (a.k.a. Sigmoid??)
# Positive values are predictive of class 1, negative of class 0.
B0 <- .0321
B1 <- 2.5
B2 <- -2.75
x1 <- rnorm(1000)
x2 <- rnorm(1000)

sigmoid <- 1 / (1 + exp((B0 + (B1 * x1) + (B2 * x2))*-1))
par(mfrow=c(2,1))
plot(sigmoid, ylim=c(-0.1, 1.1), col="blue", main="Logit function (Sigmoid)", ylab="P(y=1)")
abline(h=c(0,1), col="red")
plot(sort(sigmoid), type="h", ylim=c(-0.1, 1.1), col="gray", main="Logit function (Sigmoid)", ylab="P(y=1)")
abline(h=c(0,1), col="red")
par(mfrow=c(1,1))

# Odds: P(y = 1) / P(y = 0)
# Odds > 1 if y = 1 is more likely
# Odds < 1 if y = 0 is more likely
# Odds = e^(B0 + B1x1....)
# Taking the log on each side (log cancels out the e/exp() on the right side) we get: log(Odds) = B0 + B1x1....
# This is called the "Logit", and looks like Linear Regression
# The bigger the Logit is, the bigger P(y = 1)
# A positive Beta-value increases the logit, which increases the odds of 1.
# A negative Beta-value decreases the logit, which decreases the odds of 1.

quality <- read.csv(paste0(folder, "quality.csv"), header=T)
str(quality)
pairs(quality, col="blue")
with(quality, plot(Narcotics ~ OfficeVisits, pch=16, col=c("Red","Green"), main="Healthcare Quality",
                   xlab="Number of office visits", ylab="Number of narcotics prescribed"))
table(quality$PoorCare)

# Create a baseline model: For Logistic Regression, we just measure the distribution of the outcome variable.
good.care <- as.integer(table(quality$PoorCare)[1])
total.care <- nrow(quality)
baseline <- good.care / total.care
baseline

# Randomly split training set into a train and test set:
package.install("caTools")
library(caTools)
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio=0.75) # Here we make sure that both train and test set
# receives 75% of records with good care outcome, since that is the approximate ratio (see baseline) in our original train set
# Output from sample.split: TRUE if observation should go in training set, FALSE if observation should go in test set
qualityTrain <- subset(quality, split == T)
qualityTest <- subset(quality, split == F)
nrow(qualityTrain)
nrow(qualityTest)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
# Higher values in coeffs OfficeVisits and Narcotics (both are positive here) is indicative of poor healthcare, as we saw
# in the scatterplot data. AIC (Akaike information criterion )is like R2 in lm. A measure of the quality of the model.
# It shows: Number of variables used compared to the number of obervations.
# (TODO: How to calculate AIC? http://en.wikipedia.org/wiki/Akaike_information_criterion)
k <- length(QualityLog$coefficients) + 1
n <- nrow(qualityTrain)
# TODO: First calculate AIC: AIC = 2k + n Log(RSS/n) (RSS = residual sum of squares)
AIC <- (2 * k) + n * (log(2 * pi) + 1 + log((sum(QualityLog$residuals^2) / n))) # TODO: Not correct!
logLik(QualityLog)
AIC(QualityLog)
# Example: -2*as.numeric(logLik(lm_mtcars))+2*(length(lm_mtcars$coefficients)+1)
# Example: nrow(mtcars)*(log(2*pi)+1+log((sum(lm_mtcars$residuals^2)/nrow(mtcars))))+((length(lm_mtcars$coefficients)+1)*2)
# http://stats.stackexchange.com/questions/87345/calculating-aic-by-hand-in-r
AICc <- AIC + (2*k*(k+1))/(n-k-1)
AICc

predictTrain <- predict(QualityLog, type="response") # "response" gives probs
summary(predictTrain) # Close to 0-1 span
tapply(predictTrain, qualityTrain$PoorCare, mean) # Average prediction for each of the true outcomes (0/1)
# Remember: Poor care = 1, good care = 0.
# True poor cases (1): ~0.44. It seems we're predicting a higher prob. for the actual poor cases

# Select a threshold:
# We can use a THRESHOLD value (t) to predict accuracy in a logistic model
# If P(PoorCare = 1) >= t, predict poor quality
# If P(PoorCare = 1) < t, predict good quality
# Use a confusion matrix (actuals on rows, predictions on cols):
#          | Predicted=0          | Predicted=1          |
# ---------+----------------------+----------------------+
# Actual=0 | True Negatives (TN)  | False Positives (FP) |  
# Actual=1 | False Negatives (FN) | True Positives (TP)  |

# Sensitivity: TP / (TP + FN) (a.k.a. "The true positive rate", that is, all the positives in the dataset)
# Specificity: TN / (TN + FP) (a.k.a. "The true negative rate", the actual good care cases (0) that we classified correctly)
# A model with a HIGHER threshold will have a LOWER sensitivity and a HIGHER specificity
# A model with a LOWER threshold will have a HIGHER sensitivity and a LOWER specificity
table(qualityTrain$PoorCare, predictTrain > 0.5) # Set 0.5 as threshold
ConfusionMatrix(table(qualityTrain$PoorCare, predictTrain > 0.5), labels=c("0","1"))
sensitivity <- 8 / (17 + 8) # TP / (TP + FN)
specificity <- 73 / (73 + 1) # TN / (TN + FP)
table(qualityTrain$PoorCare, predictTrain > 0.7) # Set 0.7 as threshold. Sensitivity goes down, specificity up
table(qualityTrain$PoorCare, predictTrain > 0.2) # Set 0.2 as threshold. Sensitivity goes up, specificity down

# Predict accuracy of predictions using ROC (Receiver Operator Characteristic) curve:

# True positive rate (TPR, sensitivity) on the y-axis: Proportion of PoorCare (1) caught
# False positive rate (FPR, 1-specificity) on the x-axis: Proportion of GoodCare labeled as PoorCare.
# ROC Plot: Lower left corner (0,0): Threshold value of 1. Catches NO PoorCare cases. False Positive Rate = 0.
# ROC Plot: Upper right corner (1,1): Threshold value of 0. Catches ALL PoorCare cases (sensitivity=1). BUT it labels all
# of the GoodCare cases as PoorCare cases too! Meaning you have a False Positive Rate of 1
# The threshold DECREASES as you move from (0,0) to (1,1)

# The ROC curve captures all thresholds simultaneously: High threshold (close to 1): High specificity, low sensitivity.
# Low threshold (close to 0): Low specificity, high sensitivity.

# Choose best threshold for best trade-off: 
# - Cost of failing to detect positives
# - Costs of raising false alarms

library(ROCR)
par(mar=c(3,3,2,2))
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7), main="ROCR plot of predictTrain")
par(mar=c(3,3,2,1))
# How to assess the strength of our model with ROCR:

# TODO! Go through video 105-106 again!

# Compute outcome measures:
# N = number of observations
# Overall accuracy = (TN + TP) / N
# Overall error rate = (FP + FN) / N 
# Sensitivity = TP / (TP + FN)
# False Negative Error Rate = FN /(TP + FN) 
# Specificity = TN / (TN + FP)
# False Positive Error Rate = FP / (TN + FP)

predictTest= predict(QualityLog, type="response", newdata=qualityTest)
table(qualityTest$PoorCare, predictTest > 0.3) # Set 0.3 as threshold

# THE FRAMINGHAM HEART STUDY
# --------------------------
# Study started in 1948 in Framingham, MA. Appropriate size, stable population, and cooperative doctors and residents.
# https://www.framinghamheartstudy.org/
# https://www.framinghamheartstudy.org/about-fhs/history.php
# http://en.wikipedia.org/wiki/Framingham_Heart_Study

# CHD (Coronary Heart Disease) is a disease of the blood vessels supplying the heart
# CHD has declined 60% since 1960. Heart disease has been the leading cause of death
framingham <- read.csv(paste0(folder, "framingham.csv"), header=T) # https://biolincc.nhlbi.nih.gov/static/studies/teaching/framdoc.pdf
str(framingham)
pairs(framingham, col="blue", main="Framingham heart study")
with(framingham, plot(heartRate ~ log(glucose), col="blue", main="heartRate ~ glucose"))
with(framingham, plot(diabetes ~ glucose, col="blue", main="diabetes ~ glucose"))
# Watch out for multicollinearity:
# 1 currentSmoker cigsPerDay  0.7739
# 2  prevalentHyp      sysBP  0.6977
# 3  prevalentHyp      diaBP  0.6177
# 4      diabetes    glucose  0.6148
# 5         sysBP      diaBP  0.7867

# Create a "Framingham Risk Score" logistic regression model and predict whether a patient experienced CHD
# (Coronary Heart Disease) within 10 years of the first examination:

# Demographic risk factors: Male: Sex of patient 0/1, Age: age in years at first examination,
# Education: some HS (1), HS diploma (GED) (2), some college/vocational school (3), college (4)
# Behavioral risk factors: Smoker/cigs.
# Medical risk factors: On BloodPressureMeds, PreviousStroke, Hypertensive, Diabetes (all current)
# Risk factors from first examination:
#   totChol: Total cholesterol (mg/dL) 
#   sysBP: Systolic blood pressure 
#   diaBP: Diastolic blood pressure 
#   BMI: Body Mass Index, weight (kg)/height (m)^2
#   heartRate: Heart rate (beats/minute) 
#   glucose: Blood glucose level (mg/dL) 
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio=0.65)
train <- subset(framingham, split==T)
test <- subset(framingham, split==F)
framinghamLog <- glm(TenYearCHD ~., data=train, family=binomial)
summary(framinghamLog)
# male             0.574130   0.135500    4.24  2.3e-05 ***
# age              0.062342   0.008251    7.56  4.2e-14 ***
# sysBP            0.016346   0.004693    3.48   0.0005 ***
# cigsPerDay       0.021545   0.007830    2.75   0.0059 **
# glucose          0.006731   0.002773    2.43   0.0152 *
# NOTE: All these coeffs positive, meaning that these are contributing factors to a higher risk of CHD
predictTest <- predict(framinghamLog, newdata=test, type="response")
predResult <- table(test$TenYearCHD, predictTest > 0.5)
# NOTE: True predictions are ALWAYS on the diagonal down no matter the outcome value!
# So:
correct.preds <- predResult[1] + predResult[4] 
correct.preds / sum(predResult)
# Baseline model: Get the TRUE negative cases: Sum the ROW (actuals) with value 0:
correct.negative.cases <- predResult[1] + predResult[3]
correct.negative.cases / sum(predResult) # This is the baseline prediction
# So, NOTE: Our model barely beats the baseline accuracy.
library(ROCR)
ROCRpred <- prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Out-of-sample AUC~0.74: The model can differentiate between low- and high-risk patients

# External validation: Test on other populations. The original study was on a caucasian middle-class cohort
# Re-tested 1998 with different races: Black - fairly close prediction, Asian: Framingham study consistently overpredicted
# the CHD risk. Model can be adjusted by scaling down the risk.

# Overall inpact:
# http://cvdrisk.nhlbi.nih.gov/calculator.asp
# More than 2400 studies use Framingham data
# New developments/techniques: Analyse generations to get family history of CHD, genome-wide association study to
# verify if genetics are risk factors, more diverse cohorts, social network analysis of participants, etc.
# The study also paved the way for Clinical Decision Rules: Patient and disease characteristics and test results


# ELECTION FORECASTING: PREDICTING THE WINNER BEFORE ANY VOTES ARE CAST
# ---------------------------------------------------------------------
# Polling data from: RealClearPolitics.com
polling <- read.csv(paste0(folder, "pollingdata.csv"))
str(polling)
summary(polling)
# Instances represent a state in a given election
# State: Name of state 
# Year: Election year (2004, 2008, 2012) 
# Dependent variable: Republican: 1 if Republican won state, 0 if Democrat won 
# Independent variables: 
# Rasmussen, SurveyUSA: Polled R% - Polled D% 
# DiffCount: Polls with R winner - Polls with D winner 
# PropR: Polls with R winner / # polls
head(polling)
pairs(polling, col="blue", main="US Polls")
table(polling$Year) # We do not have data for all states in 2012
# Impute data:
package.install("mice") # Multiple Imputation by Chained Equations package  
library(mice)
table(is.na(polling))
with(polling, tapply(Year, State, is.na))
simple <- polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
str(simple)
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling) # Imputation is fixed!

Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
table(Train$Republican)
sign(polling$Rasmussen) # 1: R won: -1, D won: -1, Tie: 0 
table(sign(Train$Rasmussen)) # 1: R won: -1, D won: -1, Tie: 0 
table(Train$Republican, sign(Train$Rasmussen)) # 1: R won: -1, D won: -1, Tie: 0 
cor(polling[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
CorrelationPlot(polling[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
plot(Republican ~ PropR, col=Republican+2, data=polling, pch=16)
mod1 <- glm(Republican ~ PropR, data=Train, family=binomial)
summary(mod1)
pred1 <- predict(mod1, Train, type="response") # Get probs
table(Train$Republican, pred1 >= 0.5)
mod2 <- glm(Republican ~ DiffCount + SurveyUSA, data=Train, family=binomial)
summary(mod2)
pred2 <- predict(mod2, Train, type="response") # Get probs
table(Train$Republican, pred2 >= 0.5)

table(Test$Republican, sign(Test$Rasmussen)) # 1: R won: -1, D won: -1, Tie: 0 
TestPrediction <- predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)
subset(Test, TestPrediction >= 0.5 & Republican == 0)

# ----------------------------------------------------------------------------------------------------------------------------
# UNIT 3-1 Modeling The Expert: Quick questions:
# ----------------------------------------------
# 1) Which of the following dependent variables are categorical? 1,3,4,6 (OK)
#    Which of the following dependent variables are binary? 3,6 (OK)

# 2) What is the value of the Logit for this observation? Recall that the Logit is log(Odds). Answer: - 1 (OK)
#    Recall that log(Odds) is the same as the lm model (B0 + B1x1 etc.)
#    What is the value of the Odds for this observation? 0.3679 (OK)
#    What is the value of P(y = 1) for this observation? (ERROR! TODO!) 1 - 0.3679 is wrong!
B0 <- -1.5; B1 <- 3; B2 <- -0.5; x1 <- 1; x2 <- 5
logOdds <- B0 + (B1 * x1) + (B2 * x2)
logOdds
odds <- exp(B0 + (B1 * x1) + (B2 * x2))
odds <- exp(logOdds)
odds
# P(Y = 1) is the logit function: 1 - (1 + e^-x):
sigmoid <- 1 / (1 + exp(1)^(-logOdds))
sigmoid
1 / (1 + exp(1)^(logOdds*-1))


# 3) Coeff for StartedOnCombination=T: 1.9523
quality = read.csv("quality.csv")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
#QualityLog3 <- glm(PoorCare ~ as.integer(StartedOnCombination) + ProviderCount, data=qualityTrain, family=binomial)
QualityLog3 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog3)


# 4) 1) Sensitivity: 0.8   2) Specificity: 0.6 3) We increased the threshold value (predicted fewer true outcomes (1))
# 0  15   10
# 1	  5	 20
sensitivity <- 20 / (5 + 20) # TP / (TP + FN)
specificity <- 15 / (15 + 10) # TN / (TN + FP)
sensitivity
specificity
# 0  20   5
# 1  10	 15
table(qualityTrain$PoorCare, predictTrain > 0.5) # Set threshold DOWN, predict MORE correct true outcomes (1)
table(qualityTrain$PoorCare, predictTrain > 0.4)
table(qualityTrain$PoorCare, predictTrain > 0.2) # Set threshold UP, predict FEWER correct true outcomes (1)
table(qualityTrain$PoorCare, predictTrain > 0.3)


# 5) Answer: 1) 0.7 (35% of patients correctly identified, but with low FPR) 2) 0.3 (identifies around 50% correct, with low FPR)
# ROC curve

# 6) # Answer: Good care (Wrong!) See explanation:
# The coefficient value is positive, meaning that positive values of the variable make the outcome of 1 more likely.
# This corresponds to Poor Care.
tapply(quality$PoorCare, quality$StartedOnCombination, mean)
table(quality$PoorCare, quality$StartedOnCombination)

# 7) AUC value on test set: 0.7995
library(ROCR)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# ----------------------------------------------------------------------------------------------------------------------------
# UNIT 3-2 The Framingham Heart Study: Quick questions:
# -----------------------------------------------------

# 1) Why was the city of Framingham, Massachusetts selected for this study?
# Answer: Stable pop., appropr. size, cooperative doctors and residents

# 2) Answer: 1) "Risk factors" are independent vars. 2) Risk factors should be collected/decided BEFORE data are collected. 

# 3) 1) Specificity: 0.05556  Sensitivity: 0.9944
#   FALSE TRUE
# 0  1069	  6
# 1	  187	 11
sensitivity <- 11 / (187 + 11) # TP / (TP + FN)
specificity <- 1069 / (1069 + 6) # TN / (TN + FP)
sensitivity
specificity

# 4) Answer: External validation: Obesity CA versus all US, and Boston marathon runners versus all marathon runners

# 5) Variable most dramatically affected by behavioral intervention: Number of cigarettes per day (cigsPerDay)

# ------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK UNIT 3

# 1) POPULARITY OF MUSIC RECORDS

# Sources for songs data:
# http://en.wikipedia.org/wiki/Billboard_Hot_100
# http://www.billboard.com/
# http://echonest.com/
folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 3/Assignment/"
songs <- read.csv(paste0(folder, "songs.csv"), header=T)
str(songs)
#pairs(songs[,1:10], col="blue")
CorrelationPlot(songs)
#          var1          var2 cor.val
# 1     loudness        energy  0.7420
# 2     loudness  timbre_0_max  0.9060
# 3       energy  timbre_0_max  0.5648
# 4 timbre_3_max timbre_10_min -0.5075
par(mfrow=c(2,2))
with(songs, plot(energy ~ loudness, col="blue", main="energy ~ loudness"))
with(songs, plot(loudness ~ timbre_0_max, col="blue", main="loudness ~ timbre_0_max"))
with(songs, plot(energy ~ timbre_0_max, col="blue", main="energy ~ timbre_0_max"))
with(songs, plot(timbre_3_max ~ timbre_10_min, col="blue", main="timbre_3_max ~ timbre_10_min"))
par(mfrow=c(1,1))

par(mfrow=c(3,2))
hist(songs$tempo, col="wheat")
hist(songs$key, col="wheat")
hist(songs$timesignature, col="wheat")
hist(songs$loudness, col="wheat")
hist(songs$pitch, col="wheat")
hist(songs$energy, col="wheat")
par(mfrow=c(1,1))

with(songs, boxplot(energy ~ Top10, col="wheat", main="Top10 by energy"))
with(songs, boxplot(loudness ~ Top10, col="wheat", main="Top10 by loudness"))
with(songs, boxplot(tempo ~ Top10, col="wheat", main="Top10 by tempo"))

# PROBLEM 1.1 - UNDERSTANDING THE DATA. Answer: 373
# How many observations (songs) are from the year 2010?
nrow(songs[songs$year == 2010, ])

# PROBLEM 1.2 - UNDERSTANDING THE DATA. Answer: 18
nrow(songs[songs$artistname == "Michael Jackson", ])

# PROBLEM 1.3 - UNDERSTANDING THE DATA. Answer:
#4329 You Rock My World     1
#6207 You Are Not Alone     1
#6210    Black or White     1
#6218 Remember the Time     1
#6915     In The Closet     1
songs[(songs$artistname == "Michael Jackson"),c("Top10","songtitle")]
songs[(songs$artistname == "Michael Jackson") & (songs$Top10 == 1), c("songtitle", "Top10")]

# PROBLEM 1.4 - UNDERSTANDING THE DATA. Answer: 1) 0 1 3 4 5 7   2) 4
# 1) find timesignature discrete values
levels(as.factor(songs$timesignature))
table(songs$timesignature)
sort(unique(songs$timesignature))
# 2) Most frequenct timesignature
table(songs$timesignature)
MyMode(songs$timesignature)

# PROBLEM 1.5 - UNDERSTANDING THE DATA. Answer: Wanna Be Startin' Somethin'
songs$songtitle[which.max(songs$tempo)]
songs[which.max(songs$tempo), ]

# PROBLEM 2.1 - CREATING OUR PREDICTION MODEL. Answer: 7201 rows in train set
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)
nrow(SongsTrain)
nrow(SongsTest)

# PROBLEM 2.2 - CREATING OUR PREDICTION MODEL. Answer: AIC = 4827
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[, !(names(SongsTrain) %in% nonvars)]
SongsTest = SongsTest[, !(names(SongsTest) %in% nonvars)]
model1 <- glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(model1)

# PROBLEM 2.3 - CREATING OUR PREDICTION MODEL. Answer: See below
# The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
with(songs, plot(Top10 ~ timesignature_confidence))

# PROBLEM 2.4 - CREATING OUR PREDICTION MODEL. Answer: See below
# Mainstream listeners tend to prefer less complex songs 

# PROBLEM 2.5 - CREATING OUR PREDICTION MODEL. Answer: See below
# 1) Mainstream listeners prefer songs with heavy instrumentation
# 2) By inspecting the coefficient of the variable "energy", we do NOT draw the same conclusions as above
with(songs, plot(Top10 ~ loudness))
with(songs, plot(Top10 ~ energy))
# Look at cor between energy and loudness: 
with(songs, cor(energy, loudness)) # Quite high. By removing loudness (see 3.1 below) we get a positive (and correct) coeff for energy!

# PROBLEM 3.1 - BEWARE OF MULTICOLLINEARITY ISSUES!. Answer: 0.7399
with(SongsTrain, cor(loudness, energy))
CorrelationPlot(SongsTrain)

# PROBLEM 3.2 - BEWARE OF MULTICOLLINEARITY ISSUES!. Answer: See below
# Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial) # NOTE: -loudness removes loudness var
summary(SongsLog2)

# PROBLEM 3.3 - BEWARE OF MULTICOLLINEARITY ISSUES!. Answer: Yes
# NOTE: Like model1, but without "Energy"
# Do we make the same observation about the popularity of heavy instrumentation as we did with Model 2? Yes
nonvars = c("year", "songtitle", "artistname", "songID", "artistID", "energy")
SongsTrain3 <- subset(songs, year <= 2009)
SongsTest3 <- subset(songs, year == 2010)
SongsTrain3 = SongsTrain3[, !(names(SongsTrain3) %in% nonvars)]
SongsTest3 = SongsTest3[, !(names(SongsTest3) %in% nonvars)]
SongsLog3 = glm(Top10 ~ ., data=SongsTrain3, family=binomial)
summary(SongsLog3)

# PROBLEM 4.1 - VALIDATING OUR MODEL. Answer: Accuracy = 0.8794
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a
# threshold of 0.45? (Compute the accuracy as a number between 0 and 1.)
model3 <- glm(Top10 ~ ., data=SongsTrain3, family=binomial)
model3 <- glm(Top10 ~ . -energy, data=SongsTrain, family=binomial) # ??
summary(model3)
p <- predict(model3, newdata=SongsTest3, type="response") # Remember "response" param for correct result!
result <- table(SongsTest3$Top10, p > 0.45) # NOTE: Remember: When it says "using a threshold of N" it's: p > N, and NOT: p >= N
result
sum(diag(result)) / sum(result) # Accuracy

# PROBLEM 4.2 - VALIDATING OUR MODEL. Answer: Accuracy on model predicting most frequent outcome on test set (0): 0.8418
table(SongsTest3$Top10)[1] / nrow(SongsTest3)

# PROBLEM 4.3 - VALIDATING OUR MODEL. Answer: 19 songs correctly predicted as Top10 (actual/predicted: 1),
# 5 songs wrongly predicted as Top10 (actual: 0, predicted: 1)
p <- predict(model3, newdata=SongsTest3, type="response") # Remember "response" param for correct result!
result <- table(SongsTest3$Top10, p > 0.45) # NOTE: Remember: When it says "using a threshold of N" it's: p > N, and NOT: p >= N
result

# PROBLEM 4.4 - VALIDATING OUR MODEL. Answer: 1) Sensitivity: 0.322 2) Specificity: 0.9841
result
sensitivity <-  19 / (19 + 40)
specificity <- 309 / (309 + 5)
# Sensitivity: TP / (TP + FN)
# Specificity: TN / (TN + FP)
# A model with a HIGHER threshold will have a LOWER sensitivity and a HIGHER specificity
# A model with a LOWER threshold will have a HIGHER sensitivity and a LOWER specificity

# PROBLEM 4.5 - VALIDATING OUR MODEL. Answer: See below
# What conclusions can you make about our model?
# Model 3 favors specificity over sensitivity.
# Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely.
# So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does
# predict to be Top 10 hits.
# ----------------------------------------------------------------------------------------
# 2) PREDICTING PAROLE VIOLATORS

# PROBLEM 1.1 - LOADING THE DATASET. Answer: 675 parolees
parole <- read.csv(paste0(folder, "parole.csv"))
str(parole)
summary(parole)
table(parole$state)
table(parole$crime)

# PROBLEM 1.2 - LOADING THE DATASET. Answer: 78 violated their parole (violator = 1)
table(parole$violator)

# PROBLEM 2.1 - PREPARING THE DATASET. Answer: Unordered factors with at least 3 levels: State, Crime

# PROBLEM 2.2 - PREPARING THE DATASET. Answer: The output of the factor variables with summary() becomes like the
# output of the table() command
parole$crime <- as.factor(parole$crime)
parole$state <- as.factor(parole$state)
summary(parole)

# PROBLEM 3.1 - SPLITTING INTO A TRAINING AND TESTING SET. Answer: ~70 percent for train, ~30 percent for test
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
nrow(train)
nrow(test)
train.percent <- (nrow(train)/(nrow(train)+nrow(test))) * 100
test.percent <- (nrow(test)/(nrow(train)+nrow(test))) * 100

# PROBLEM 3.2 - SPLITTING INTO A TRAINING AND TESTING SET. Answer: 1) Same 2) Different 3) Different
# set.seed(N) makes the split different. If re-running without changing seed, exactly same result from sample.split()

# PROBLEM 4.1 - BUILDING A LOGISTIC REGRESSION MODEL. Answer: significant: race, state4, multiple.offenses
model1 <- glm(violator ~ ., data=train, family=binomial)
summary(model1)

# PROBLEM 4.2 - BUILDING A LOGISTIC REGRESSION MODE. Answer: See below
# What can we say based on the coefficient of the multiple.offenses variable?
# The following two properties might be useful to you when answering this question:
# 1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c
#    for a unit increase in the variable.
# 2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit
#    increase in the variable.
coeff <- model1$coeff["multiple.offenses"]
exp(coeff) # To get the odds value from the logit result that summary() gives
# Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator
# than a parolee who did not commit multiple offenses but is otherwise identical.

# PROBLEM 4.3 - BUILDING A LOGISTIC REGRESSION MODEL. Answer: 1)  2) TODO...
# (Intercept)       -4.241157   1.293885   -3.28    0.001 ** 
# male               0.386990   0.437961    0.88    0.377    
# race               0.886719   0.395066    2.24    0.025 *  
# age               -0.000176   0.016085   -0.01    0.991    
# state2             0.443301   0.481662    0.92    0.357    
# state3             0.834980   0.556270    1.50    0.133    
# state4            -3.396788   0.611586   -5.55  2.8e-08 ***
# time.served       -0.123887   0.120423   -1.03    0.304    
# max.sentence       0.080295   0.055375    1.45    0.147    
# multiple.offenses  1.611992   0.385305    4.18  2.9e-05 ***
# crime2             0.683714   0.500355    1.37    0.172    
# crime3            -0.278105   0.432836   -0.64    0.521    
# crime4            -0.011763   0.571304   -0.02    0.984
# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland,
# served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny.
intercept <- -4.241157
male <- 0.386990
aged50 <- -0.000176 * 50 # Aged 50 at release
maryland <- 0 # Maryland is in group "Other state", does not have coeff 
time.served <- -0.123887  * 3 # Served 3 months
max.sentence <- 0.080295 * 12
multiple.offenses <- 1.611992 # TODO: But he did NOT commit multiple offenses. Skip this? Or subtract??
crime2 <- 0.683714 # Commited a larceny (crime category 2)
# TODO: Look at video again for formulas here:
probs <- 1 / (1 + exp(1)^((intercept + male + aged50 + maryland + time.served + max.sentence + crime2) * -1))
probs
MyOdds(probs)

# PROBLEM 5.1 - EVALUATING THE MODEL ON THE TESTING SET. Answer: Max prediction for parole violation: 0.9073
pred <- predict(model1, newdata=test, type="response")
max(pred)

# PROBLEM 5.2 - EVALUATING THE MODEL ON THE TESTING SET. Answer 1) Sensitivity: 0.5217  2) Specificity: 0.933  3) Accuracy: 0.8861
result <- table(test$violator, pred > 0.5)
result
# Use a confusion matrix (actuals on rows, predictions on cols):
#          | Predicted=0          | Predicted=1          |
# ---------+----------------------+----------------------+
# Actual=0 | True Negatives (TN)  | False Positives (FP) |  
# Actual=1 | False Negatives (FN) | True Positives (TP)  |

# Sensitivity: TP / (TP + FN) (a.k.a. "The true positive rate", that is, all the positives in the dataset)
# Specificity: TN / (TN + FP) (a.k.a. "The true negative rate", the actual 0's that we classified correctly)
# A model with a HIGHER threshold will have a LOWER sensitivity and a HIGHER specificity
# A model with a LOWER threshold will have a HIGHER sensitivity and a LOWER specificity
sensitivity <- result[4] / (result[4] + result[2])
sensitivity
specificity <- result[1] / (result[1] + result[3])
specificity
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 5.3 - EVALUATING THE MODEL ON THE TESTING SET. Answer: 0.8861
# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
# Solution: Just find the most frequent outcome (non-violator), and do (non-violator / nrow(test))
nrow(test[test$violator==0,]) / nrow(test)
# Could also do this the usual way we find accuracy:
result <- table(test$violator, rep(0, nrow(test)))
sum(diag(result)) / sum(result)

# PROBLEM 5.4 - EVALUATING THE MODEL ON THE TESTING SET. Answer: See below
# Answer: The board assigns more cost to a false negative than a false positive, and should therefore use a
# logistic regression cutoff less than 0.5 (higher sensitivity, detecting a better true positive rate). 
# Sensitivity: TP / (TP + FN) (a.k.a. "The true positive rate", that is, all the positives in the dataset)
library(ROCR)
# TODO: Predict on the SimpleModel mentioned above!
pred <- predict(SimpleModel, newdata=test, type="response")
ROCRpred <- prediction(pred, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 5.5 - EVALUATING THE MODEL ON THE TESTING SET. Answer: See below.
# Answer: The model is likely of value to the board, and using a different logistic regression cutoff is
# likely to improve the model's value.  
result1 <- table(test$violator, pred > 0.9)
result2 <- table(test$violator, pred > 0.5)
result3 <- table(test$violator, pred > 0.1)
result1
result2
result3 # Lower cutoff values -> better sensitivity (better result in predicting true violators (1))

# PROBLEM 5.6 - EVALUATING THE MODEL ON THE TESTING SET. Answer: AUC on test set = 0.8946
# Using the ROCR package, what is the AUC value for the model?
model1 <- glm(violator ~ ., data=train, family=binomial)
predROC <- predict(model1, newdata=test, type="response")
ROCRpred <- prediction(predROC, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 5.7 - EVALUATING THE MODEL ON THE TESTING SET. Answer: See below.
# Describe the meaning of AUC in this context.
# Answer: The probability the model can correctly differentiate between a randomly selected parole violator and a
# randomly selected parole non-violator.
# Explanation: The AUC deals with differentiating between a randomly selected positive and negative example.
# It is independent of the regression cutoff selected.

# PROBLEM 6.1 - IDENTIFYING BIAS IN OBSERVATIONAL DATA. Answer: See below.
# How could we improve our dataset to best address selection bias?
# We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole
# or they completed their term. 

# ---------------------------------------------------------------------------
# 3) PREDICTING LOAN REPAYMENT
# Data from: https://www.lendingclub.com/info/download-data.action

# PROBLEM 1.1 - PREPARING THE DATASET. Answer: Proportion of loans not fully paid: 0.1601
loans <- read.csv(paste0(folder, "loans.csv"))
str(loans)
summary(loans)
pairs(loans, col="blue")
pay.status <- table(loans$not.fully.paid)
pay.status[2] / sum(pay.status)

# PROBLEM 1.2 - PREPARING THE DATASET. Answer: NA's in: log.annual.inc, days.with.cr.line, revol.util,
# ing.last.6mhts, delinq.2yrs, pub.rec,
colSums(is.na(loans))
colSums(is.na(loans)) == 0

# PROBLEM 1.3 - PREPARING THE DATASET. Answer: We want to be able to predict risk for all borrowers,
# instead of just the ones with all data reported.
# Best reason for imputing.
missing.vals <- subset(loans, !complete.cases(loans))
dim(missing.vals)
View(missing.vals)

# PROBLEM 1.4 - PREPARING THE DATASET. Answer: See below
# What best describes the process we just used to handle missing values?
# Answer: We predicted missing variable values using the available independent variables for each observation.
loans <- read.csv(paste0(folder, "loans_imputed.csv"))
str(loans)

# PROBLEM 2.1 - PREDICTION MODELS. Answer: Check summary (p < 0.05)
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
nrow(train)
nrow(test)
model2 <- glm(not.fully.paid ~ ., data=train, family=binomial)
summary(model2)

# PROBLEM 2.2 - PREDICTION MODELS. Answer: 1) TODO... 2) 1.094 
fico <- -9.31e-03
intercept <- 9.05e+00
x <- c(700, 710)
Logit.A <- MyLogit(1 / (1 + exp(1)^((intercept + (fico*x[1])) * -1)))
Logit.B <- MyLogit(1 / (1 + exp(1)^((intercept + (fico*x[2])) * -1)))
# Same as:
LogOdds.A <- (intercept + (fico*x[1]))
LogOdds.B <- (intercept + (fico*x[2]))
Odds.A <- exp(intercept + (fico*x[1])) # Note: Can just do exp(), don't need exp(1)^()
Odds.B <- exp(intercept + (fico*x[2]))
Logit.A - Logit.B
LogOdds.A - LogOdds.B
Odds.A - Odds.B
Prob.A - Prob.B
exp(Logit.A)/exp(Logit.B)

# PROBLEM 2.3 - PREDICTION MODELS. Answer: 1) Accuracy on test set = 0.8393  2) Baseline model accuracy: 0.8399
p <- predict(model2, newdata=test, type="response")
# Add to test set for later use:
test$predicted.risk <- p
result <- table(test$not.fully.paid, p > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy
baseline.model <- table(test$not.fully.paid)
baseline.model
baseline.accuracy <- baseline.model[1] / nrow(test)
baseline.accuracy

# PROBLEM 2.4 - PREDICTION MODELS. Answer: AUC on test set: 0.6801
library(ROCR)
ROCRpred <- prediction(p, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 3.1 - A "SMART BASELINE". Answer: int.rate is correlated with other risk-related variables, and therefore 
# does not incrementally improve the model when those other variables are included. 
model3 <- glm(not.fully.paid ~ int.rate, data=loans, family=binomial)
summary(model3)
CorrelationPlot(train)

# PROBLEM 3.2 - A "SMART BASELINE". Answer: 1) max prob on test set: 0.4345 2) 0 predictions at cutoff 0.5
pred <- predict(model3, newdata=test, type="response")
max(pred)
table(test$not.fully.paid, pred >= 0.4345)

# PROBLEM 3.3 - A "SMART BASELINE". Answer: AUC = 0.6239
library(ROCR)
ROCRpred <- prediction(pred, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# PROBLEM 4.1 - COMPUTING THE PROFITABILITY OF AN INVESTMENT. Answer: A $11.97 payback after 3 years
# To compute interest revenue, consider a $c investment in a loan that has an annual interest rate r over a period of t years.
# Using continuous compounding of interest, this investment pays back c * exp(rt) dollars by the end of the t years,
# where exp(rt) is e raised to the r*t power.
c <- 10 # A 10 dollar investment
r <- 0.06 # A 6% interest rate
t <- 3 # 3 years time for paying back the loan
c * exp(r*t)

# PROBLEM 4.2 - COMPUTING THE PROFITABILITY OF AN INVESTMENT. The profit is: (c * exp(rt)) - c
# Explanation: A person's profit is what they get minus what they paid for it. In this case, the investor gets
# c * exp(rt) but paid c, yielding a profit of c * exp(rt) - c.

# PROBLEM 4.3 - COMPUTING THE PROFITABILITY OF AN INVESTMENT. Answer: Profit = -c (no money was paid back, full investment lost)

# PROBLEM 5.1 - A SIMPLE INVESTMENT STRATEGY. Answer: Max profit for 10 years on loans fully paid back is: 8.895
# Assume a $1 investment (aka c=1)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] <- -1
max(test$profit[test$not.fully.paid == 0] * 10)

# PROBLEM 6.1 - AN INVESTMENT STRATEGY BASED ON RISK. Answer: 1) Mean profit is: 0.2251  2) Proportion not fully paid: 0.2517
# First, use the subset() function to build a data frame called highInterest consisting of the test set loans
# with an interest rate of at least 15%:
highInterest <- subset(test, int.rate >= 0.15)
plot(sort(highInterest$int.rate))
hist(highInterest$int.rate)
mean(highInterest$profit) # NOTE: Should include ALL loans here, including the "not fully paid back" ones!
table(highInterest$not.fully.paid)[2] / nrow(highInterest)

# PROBLEM 6.2 - AN INVESTMENT STRATEGY BASED ON RISK. Answer: 1) Total profit: 32.28  2) Loans not paid back in full: 18
# Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in
# increasing order and selecting the 100th element of this sorted list:
loans <- read.csv(paste0(folder, "loans_imputed.csv"))
cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk < cutoff)
nrow(selectedLoans) # NOTE: Gives correct answer on 99 loans! ("< cutoff", not "<= cutoff" as it should be!)
table(selectedLoans$not.fully.paid)
# We have now seen how analytics can be used to select a subset of the high-interest loans that were paid back at only
# a slightly lower rate than average, resulting in a significant increase in the profit from our investor's $100 investment.
# Although the logistic regression models developed in this problem did not have large AUC values, we see that they still
# provided the edge needed to improve the profitability of an investment portfolio.
