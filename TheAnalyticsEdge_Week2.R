# The Analytics Edge week 2 - Linear Regression
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/linear-regression/
# ---------------------------------------------------------------------------------------

library(scales)

SetStandardOptions()
# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")


folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 2/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# THE STATISTICAL SOMMELIER: AN INTRODUCTION TO LINEAR REGRESSION
# ---------------------------------------------------------------
# Linear regression and SSE (sum of squared errors)
n <- 255
x <- (((1:n) / 10)) + 15
x
y <- (x^2) + (rnorm(n) * 100)
df <- data.frame(Avg.Growing.Season.Temp=x, Price=y)
df
model <- lm(Price ~ Avg.Growing.Season.Temp, data=df)
summary(model)
SSE <- sum((df$Price - model$fitted)^2)
SSE
RMSE <- sqrt(sum((df$Price - model$fitted)^2) / n) # ROOT MEAN square error. NOTE: divided by n for MEAN, and sqrt for ROOT
RMSE # or use: SSE/N
null.model <- lm(Price ~ I(rep(0, n)), data=df) # Choose a random value for a straight horiz. line
summary(null.model)
SST <- sum((df$Price - null.model$fitted)^2) # NOTE: null.model$fitted = mean(y.test)
SST
R2 <- 1 - (SSE / SST)
R2 # OK! Compare to output from summary(model) 
summary(model)
plot(df, col="blue", xlab="Temperature", main=paste0("Wine price by growing temperature (R2 = ", round(R2, 3), ")"))

package.install("R330")
library(R330)
data(wine.df)
boxcoxplot(price~temp+h.rain+w.rain+year, data=wine.df)
names(wine.df) #h.rain = Harvest Rain, w.rain = Winter.Rain
model <- lm(price ~., data=wine.df)
summary(model)
plot(log(price) ~ temp, data=wine.df, col="blue", main="Price versus temperature")
model <- lm(price ~ log(temp), data=wine.df)
model <- lm(price ~ temp, data=wine.df)
plot(price ~ temp, data=wine.df, col="blue", main="Price versus temperature")
abline(model, col="red")
summary(model)
SSE <- sum((wine.df$price - model$fitted)^2))
# Same as:
SSE <- sum(model$residuals^2)
SST <- sum((wine.df$price - mean(wine.df$price))^2) # NOTE: null.model$fitted = mean(y)
R2 <- 1 - (SSE/SST)
R2
# Quality of wine dataset (Ashenfelter 1990)
# http://www.liquidasset.com/winedata.html
wine <- read.csv(paste0(folder, "wine.csv"))
str(wine)
summary(wine)
head(wine)
wineTest <- read.csv(paste0(folder, "wine_test.csv"))
str(wineTest)
summary(wineTest)
head(wineTest)
model1 <- lm(Price ~ AGST, data=wine) # AGST = Average growing season temp
summary(model1)
plot(Price ~ AGST, data=wine, col="blue", main="Price versus temperature")
abline(model1, col="red")
# NOTE: Adjusted R2 DECREASES if adding an independent var (predictor) that does not help the model. Good way to find out if a var
# is good to include or not in the model!
SSE1 <- sum(model1$residuals^2)
SSE1
model2 <- lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE2 <- sum(model2$residuals^2)
SSE2
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE3 <- sum(model3$residuals^2)
SSE3

# Understanding the model:

# If a predictor's coefficient is very close to zero, it should probably be removed, as it does not help the model much.
# The standard error gives a measure of how much the coefficient/predictor is likely to vary from its estimated value
# The t-value is ("the estimate" / "the standard error"). The bigger the t-value is, the more likely it is that the
# coefficient (predictor) is significant.
# The "Pr" column gives a value for how plausible it is that the coefficient is actually zero given the data we
# have used to build the model. A small value here is significant. Big t-value -> small P-value. And vice versa.
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
# Age and FrancePop are not significant, so remove them
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4) # NOTE: R2.adj has increased! The model4 is just as strong, if not stronger, than model3

# Correlation and multicollinearity:

# NOTE: Age is now suddenly also more significant. This is due to multicollinearity. Age and FrancePop are highly 
# correlated.
cor(wine$Age, wine$FrancePop)
plot(wine$Age ~ wine$FrancePop, col="blue", pch=22, bg="gray")
CorrelationPlot(wine)
model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
# Removing age too causes R2 to decrease

# Making predictions:

model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4) # NOTE: R2.adj has increased! The model4 is just as strong, if not stronger, than model3
predictTest <- predict(model4, newdata=wineTest)
predictTest
str(wineTest)
SSE <- sum((wineTest$Price - predictTest)^2)
SSE
SST <- sum((wineTest$Price - mean(wine$Price))^2)
SST
R2 <- 1 - SSE/SST
R2
# NOTE: This test set is very small. It is best to increase the size of the test set to be more confident about
# the out-of-sample accuracy of our model. NOTE: Out-of-sample R2 can sometimes be nagative too amd does not always
# increase as we add predictors.
summary(lm(Price ~ AGST, data=wine, subset=wine_test$Price)) # Not sure how they found R2 with just two rows...
summary(lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine_test, subset=wine_test$Price))
anova(model4)

# Comparing the models to the experts:
# Conclusion: Ashenfelter's linear regression predicted the good Bordeaux wine years/price very well.
# A quantitative approach gave better estimates than a qualitative (wine experts opinion) approach in this case.

# -----------------------------------------------------------------------------------------------------------------------------------
# Quick question 2:
x <- 0:2
y <- 3*x + 2
y
sum(y - x)
summary(lm(y ~ I(rep(0,3))))
summary(lm(y ~ x))

plot(x, y, type="o", asp=1, ylim=c(0,8), xlim=c(-1,3), pch=19, col="blue")
abline(h=y, v=x, col="lightgray")
# Baseline prediction = 2 (Error!) (or 12? 2 + 5 + 5) (Error!) 4 (mean of y (2,5,5)) CORRECT! Baseline prediction = mean(y)
# R2: 0 (Error!) 6?
sum((c(2,2,8) - mean(c(2,5,5)))^2) # SST
n = 3
SSE <- sum((c(2,5,5) - c(2,2,8))^2) # Prediction: (0,2) (1,2) (1,8)
SSE # 18 (OK)
x <- c(0,1,1)
y
SST <- sum((c(2,2,8) - mean(c(2,5,5)))^2)
SST # 24?
R2 <- 1 - (SSE / SST)
R2 # 0.25 ??

x <- c(2,2,8)
y <- c(2,5,5)
model <- lm(y ~ x)
summary(model)

# Quick question 3: Can R^2 decrease as we add new predictors? Answer: No, but the retiurn diminishes for each new var added
# "year"   "price"  "temp"   "h.rain" "w.rain"
model <- lm(price ~ temp, data=wine.df)
summary(model)
model <- lm(price ~ temp + h.rain, data=wine.df)
summary(model)
model <- lm(price ~ temp + h.rain + year, data=wine.df)
summary(model)
model <- lm(price ~ temp + h.rain + year + w.rain, data=wine.df)
summary(model)

# Quick question 4: Answer: 1) 0.318 2) -4.97e-03 3) 7.87e+00
model <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model)
par(mfrow=c(1,2))
plot(Price ~ HarvestRain, data=wine, col="blue", main="Price versus harvest rain", xlim=c(0, max(HarvestRain)))
abline(model$coeff[1], model$coeff[2], col="red")
plot(Price ~ WinterRain, data=wine, col="red", main="Price versus winter rain", xlim=c(0, max(WinterRain)))
abline(model$coeff[1], model$coeff[3], col="green3")
par(mfrow=c(1,1))

# Quick question 5: Answer: 1) Yes (0.0052) 2) No (0.9139)
model <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model)

# Quick question 6: Answer: -0.2754
cor(wine$WinterRain, wine$HarvestRain)

# Quick question 7: Correct answer: -7, 0, 2.4

# --------------------------------------------------------------------------------------------------------------------
# MONEYBALL: THE POWER OF SPORTS ANALYTICS (Movie (2002) about a Oakland Ca. baseball team, "The Athletics"/"The A's")
# http://www.imdb.com/title/tt1210166/
# http://en.wikipedia.org/wiki/Baseball
# Data from: http://www.baseball-reference.com/
baseball <- read.csv(paste0(folder, "baseball.csv"))
dim(baseball)
str(baseball) # Team-Year pairs for 1962-2012 for all seasons with 162 games (G). RS=runs scored, RA=runs allowed, W=wins
summary(baseball)
names(baseball)

# Nice way to use ifelse(...) in points coloring:
plot(moneyball$W, moneyball$Team, col=ifelse(moneyball$W > 90, 'red','black'), pch=16,
     main="Team by Wins")

baseball2 <- na.omit(baseball)
# How many games do a team need to win to make it to the playoffs?
# How does a team win games? They score more runds than their opponents.
# The A's calculated that they needed to score 135 more runs than they allowed during the regular season
# to expect to win 95 games:
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
# Create a "Run Difference" column:
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)
with(moneyball, plot(W ~ RD, col="blue", main="Wins ~ Runs difference", ylab="Wins", xlab="Runs difference"))
abline(v=135, h=95, col="red") # RD needed for 95 wins
winsReg <- lm(W ~ RD, data=moneyball)
summary(winsReg)
abline(v=0, h=winsReg$coeff[1], col="gray") # model y-intercept
abline(winsReg, col="green3") # RD needed for 95 wins
# So: Wins = y-intercept + (slope * RD)
# We need Wins >= 95. So: 80.8814 + (0.1058 * RD) >= 95
# Rearranging (divide both sides by 0.1058, subtract 80.8814 on both sided), we get:
# RD >= 95 - 80.8814 / 0.1058
RD <- (95 - 80.8814) / 0.1058
RD # So, if the RD > 133.4 we predict that the team will win at least 95 games
# How does a team score more runs? On-Base Percentage (OBP, percentage of time an opponent gets on base, including walks)
# and Slugging Percentage (SLG, How far an opponent gets around the bases on his turn, measures the power of a hitter)
# are significant predictors. They are more important than Batting Average (BA):
CorrelationPlot(na.omit(moneyball))
with(moneyball, plot(RS ~ OBP, col="blue", main="Wins ~ OBP", ylab="Wins", xlab="OBP"))
with(moneyball, cor(RS, OBP))
with(moneyball, cor(RS, SLG))
with(moneyball, cor(RS, BA))
RunsReg <- lm(RS ~ OBP + SLG + BA, data=moneyball) # NOTE: Highly correlated predictors
summary(RunsReg)
# NOTE: Coeff of BA is negative, that implies that teams with a lower batting average will score more runs.
# This is wrong, and indicates multicollinearity. so let's move BA:
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball) # NOTE: Highly correlated predictors
summary(RunsReg) # R2 = 0.91. Both predictors are significant
# Using opponents OBP and opponents SLG:
Runs.Allowed <- RunsReg$coeff[1] + (RunsReg$coeff[2] * moneyball$OBP) + ((RunsReg$coeff[2] * moneyball$SLG))
plot(Runs.Allowed)
# Predicting runs scored in 2002: A's had 24 batters on their roster at the beginning of the 2002 season.
# Using the 2001 statistics for these players: Team OBP is 0.339. Team SLG is 0.430
Team.OBP <- 0.339
Team.SLG <- 0.430
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
Runs.Scored <- as.numeric(RunsReg$coeff[1] + (RunsReg$coeff[2] * Team.OBP) + (RunsReg$coeff[3] * Team.SLG))
Runs.Scored # 805
Team.OOBP <- 0.307
Team.OSLG <- 0.373
RunsReg <- lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsReg)
Runs.Allowed <- as.numeric(RunsReg$coeff[1] + (RunsReg$coeff[2] * Team.OOBP) + (RunsReg$coeff[3] * Team.OSLG))
Runs.Allowed # ~622
# Remember, this was our model to predict Wins based on RunsDifference (Runs.Scored - Runs.Allowed)
winsReg <- lm(W ~ RD, data=moneyball)
summary(winsReg)
# So to predict wins, we use: Wins <- as.numeric(winsReg$coeff[1] + (winsReg$coeff[2] * (Runs.Scored - Runs.Allowed)))
Wins <- as.numeric(winsReg$coeff[1] + (winsReg$coeff[2] * (round(Runs.Scored) - round(Runs.Allowed))))
80.8814 + 0.1058 * (805-622)
Wins # 100
# Winning the world series: Is playoff performance predictable?
# Using data 1994-2001 (8 teams in the playoffs)
# Correlation between winning the World Series and regular season wins is 0.03 (very weak).
# (NOTE: Using Logistic Regression in Week 3 to predict this)

# Where is baseball now?
# "Sabermetrics" is now the term for the analysis that the A's championed / Moneyball techniques.
# Baseball Prospectus website (baseballprospectus.com)
# Value Over Replacement Player (VORP)
# Defense Independent Pitching Statistics (DIPS)
# Other sport analytics sites: http://www.82games.com/, http://www.socceranalysts.com/, http://www.soccermetrics.net/,
# http://www.cricmetric.com/, http://www.impactindexcricket.com/, http://www.hockeyanalytics.com/,  http://www.lighthousehockey.com/

# ------------------------------------------------------------------------------------
# Quick question 1: 91.36 wins needed
80.8814 + (0.1058 * (713 - 614)) # Wins: 91.36
# Try to predict this result:
RD.new <- data.frame(RD=(713 - 614))
winsReg <- lm(W ~ RD, data=moneyball)
summary(winsReg)
prediction <- predict(winsReg, newdata=RD.new)
prediction # Very close! 91.35 versus 91.36
RD <- (95 - 80.8814) / 0.1058
RD # So, if the RD > 133.4 we predict that the team will win at least 95 games

# Question 2: 1) 688.7 2) 588.2
OBP <- 0.311
SLG <- 0.405
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball) # NOTE: Highly correlated predictors
summary(RunsReg)
Runs.Scored <- as.numeric(RunsReg$coeff[1] + (RunsReg$coeff[2] * OBP) + (RunsReg$coeff[3] * SLG))
Runs.Scored <- -804.6 + (2737.8 * OBP) + (1584.9 * SLG)
Runs.Scored # 688.7
OOBP <- 0.297
OSLG <- 0.370
RunsReg <- lm(RA ~ OOBP + OSLG, data=moneyball) # NOTE: Use opponents scores OOSLG and OOBP!
summary(RunsReg)
Runs.Allowed <- as.numeric(RunsReg$coeff[1] + (RunsReg$coeff[2] * OOBP) + (RunsReg$coeff[3] * OSLG))
Runs.Allowed <- -837.38 + (2913.60 * OOBP) + (1514.29 * OSLG)
Runs.Allowed # 588.2

# Quick question 3: Answer: Giambi and Pena
# Player Name  OBP	SLG	Salary
# Eric Chavez	0.338	0.540	$1,400,000
# Jeremy Giambi	0.391	0.450	$1,065,000
# Frank Menechino	0.369	0.374	$295,000
# Greg Myers	0.313	0.447	$800,000
# Carlos Pena	0.361	0.500	$300,000
player.stats <- data.frame(name=c("Eric Chavez", "Jeremy Giambi", "Frank Menechino", "Greg Myers", "Carlos Pena"),
                           OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
                           SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
                           Salary=c(1400000, 1065000, 295000, 800000, 300000))
player.stats
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball) # NOTE: Highly correlated predictors
summary(RunsReg)
Runs.Scored <- as.numeric(RunsReg$coeff[1] + (RunsReg$coeff[2] * player.stats$OBP) + (RunsReg$coeff[3] * player.stats$SLG))
player.stats$Runs.Scored <- Runs.Scored
player.stats

# Quick question 4: Answer: 1) 0.3477  2) -0.6557
teamRank <- c(1, 2, 3,3, 4,4,4,4, 5,5)
wins2012 <- c(94, 88, 95,88, 93,94,98,97, 93,94) 
wins2013 <- c(97, 97, 92,93, 92,96,94,96, 92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)
par(mfrow=c(1,2))
plot(wins2012 ~ teamRank, type="o", main="Wins 2012 by Team Rank", col="blue", bg="cyan", pch=21)
plot(wins2013 ~teamRank, type="o", main="Wins 2013 by Team Rank", col="blue", bg="cyan", pch=21)
par(mfrow=c(1,1))

# Quick question 5: Answer: Predicting how many home runs the Oakland A's will hit next year

# --------------------------------------------------------------------------------------------------------------------
# PLAYING MONEYBALL IN THE NBA (National Basketball Association):
# Data from: http://www.basketball-reference.com/
NBA <-  read.csv(paste0(folder, "NBA_train.csv"))
str(NBA)
# SeasonEnd: The year the season ended
# Team: Name of team
# Playoffs: Binary var, team made it to the playoffs yes/no
# W: Regular season wins
# PTS: Points scored during the regular season
# oppPTS: Opponents points scored during the regular season
# FG/A: Field goals, (/A: field goals Attempted, see also below)
# X2P/A: Two-pointers (NOTE: X added by R since org. var name was 2P/2PA)
# XP3/A: Three-pointers
# FR/A: Free throws
# ORB/DRB: Offensive/defensive rebounds
# AST: Assists
# STL: Steals
# BLK: Blocks
# TOV: Turnovers
par(mar=c(10.5,3,2,1))
with(NBA, plot(W ~ Team, las=2, col=2:8, main="Regular season wins by Team", ylab="Wins", xlab=NA))
par(mar=c(3,3,2,1))
table(NBA$W, NBA$Playoffs)
# Add a var for diff points.scored and points.allowed
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS
plot(NBA$W ~ NBA$PTSdiff, col="blue", main="Wins versus difference in ponts scored/allowed")
WinsReg <- lm(W ~ PTSdiff, data=NBA)
summary(WinsReg)
abline(WinsReg, col="red")
W <- WinsReg$coeff[1] + (WinsReg$coeff[2] * NBA$PTSdiff)
W
# The points difference should be >= 42 (see intercept value):
# W = 41 + (0.326 * PTSdiff) = 30.67. Rewrite this as: 
PTSdiff <- (42 - WinsReg$coeff[1]) / WinsReg$coeff[2] # PTSdiff >= (42 - 41) / 0.0326
# So we need to score at least 31 points to win at least 42 games
PointsReg <- lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK, data=NBA)
summary(PointsReg)
PointsReg$residuals
SSE <- sum(PointsReg$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(NBA))
RMSE # ~184 points off, not bad
mean(NBA$PTS)
# Remove some predictors from the model:
PointsReg2 <- lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK, data=NBA) # Remove TOV
summary(PointsReg2)
PointsReg3 <- lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+STL+BLK, data=NBA) # Remove DRB
summary(PointsReg3)
PointsReg4 <- lm(PTS ~ X2PA+X3PA+FTA+AST+ORB+STL, data=NBA) # Remove BLK
summary(PointsReg4)
SSE <- sum(PointsReg4$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(NBA))
RMSE # ~Still 184 points off, not bad. We've essentially narrowed/simplified the model without losing prediction accuracy
# Making predictions:
NBA_test <-  read.csv(paste0(folder, "NBA_test.csv"))
str(NBA_test)
PointsPredictions <- predict(PointsReg4, NBA_test)
# Get Out-of-sample R2:
SSE <- sum((PointsPredictions - NBA_test$PTS)^2)
SSE
SST <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)
SST
RMSE <- sqrt(SSE/nrow(NBA_test))
RMSE # ~196
R2 <- 1 - (SSE/SST)
R2

# ------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------
# Homework:

# 1) CLIMATE CHANGE
# -----------------
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/

# Data from: http://www.cru.uea.ac.uk/cru/data/temperature/
#            http://www.esrl.noaa.gov/gmd/ccgg/data-products.html

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 2/Assignment/"
data <- read.csv(paste0(folder, "climate_change.csv"), header=T)
dim(data)
str(data)
summary(data)
names(data)
sapply(data, class)
pairs(data, col="blue")
plot(Temp ~ CFC.12, data=data, pch=21, bg="cyan")

train <- subset(data, Year <= "2006")
test <- subset(data, Year > "2006")
dim(train)
dim(test)

# PROBLEM 1.1 - CREATING OUR FIRST MODEL. Answer: 0.751
model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model1)
result <- predict(model1, newdata=test, type="response")

# PROBLEM 1.2 - CREATING OUR FIRST MODEL. Answer: MEI CO2 CFC.11 CFC.12 TSI Aerosols

# PROBLEM 2.1 - UNDERSTANDING THE MODEL. Answer: All of the gas concentration variables reflect human
# development - N2O and CFC.11 are correlated with other variables in the data set.
# N2O: nitrous oxide, CFC-11: trichlorofluoromethane 
summary(model1)
pairs(train, col="blue", main="Train")
with(train, plot(Temp ~ CFC.11, main="Temp ~ CFC.11", pch=16, col="gray"))
with(train, plot(Temp ~ N2O, main="Temp ~ N2O", pch=16, col="gray"))
CorrelationPlot(train)

# PROBLEM 2.2 - UNDERSTANDING THE MODEL. Answer 1: CO2, CH4, CFC.12  Anser 2: CH4, CFC.12
result <- CorrelationPlot(data, 0.7)
result[result$var1 == "N2O" | result$var2 == "N2O", ]
result <- cor(data)
result[result$var1 == "CFC.11" | result$var2 == "CFC.11", ]

# PROBLEM 3 - SIMPLIFYING THE MODEL. Answer 1: 2.53e-02  Answer 2: 0.726
model2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train)
summary(model2)

# PROBLEM 4 - AUTOMATICALLY BUILDING THE MODEL. Answer 1: 0.751 (Correct??)  Answer 2: CH4 (Correct??)
# http://en.wikipedia.org/wiki/Akaike_information_criterion
# http://stat.ethz.ch/R-manual/R-devel/library/stats/html/step.html
# TODO: Use full model or the simplified model above as step() input?
model3 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model3)
simplified.model3 <- step(model3)
summary(simplified.model3)

# PROBLEM 5 - TESTING ON UNSEEN DATA. Answer: 0.6286
result <- predict(simplified.model3, newdata=test, type="response")
summary(result)
# Calculate R^2 manually. Find SSE/SST on test data and prediction
SSE <- sum((result - test$Temp)^2)
SSE
SST <- sum((mean(train$Temp) - test$Temp)^2)
SST
RMSE <- sqrt(SSE/nrow(test))
RMSE # ~0.09
R2 <- 1 - (SSE/SST)
R2 # 0.6286


# 2) READING TEST SCORES
# ----------------------
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/f8d71d64418146f18a066d7f0379678c/60d93a44280348d7a0a16663f92af0f7/
# Data from: http://nces.ed.gov/pubsearch/pubsinfo.asp?pubid=2011038
# In this homework assignment, we will predict the reading scores (readingScore variable) of students from the United States
# of America on the 2009 PISA exam.

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 2/Assignment/"
pisa.train <- read.csv(paste0(folder, "pisa2009train.csv"), header=T)
pisa.test <- read.csv(paste0(folder, "pisa2009test.csv"), header=T)
str(pisa.train)
summary(pisa.train)
pairs(na.omit(pisa.train), col="blue") # Odd display...
CorrelationPlot(na.omit(pisa.train))
names(pisa.train)

# PROBLEM 1.1 - DATASET SIZE. Answer: 3663
nrow(pisa.train)

# PROBLEM 1.2 - SUMMARIZING THE DATASET. Answer: 1) 483.5 (males reading score)  2) 512.9 (females reading score)
tapply(pisa.train$readingScore, pisa.train$male, mean, na.rm=T)
# Same as: mean(pisa.train$readingScore[pisa.train$male == 1])

# PROBLEM 1.3 - LOCATING MISSING VALUES. Answer: All except: grade, male, publicSchool, urban, readingScore
table(sapply(pisa.train, is.na))
sapply(pisa.train, function(x) unique(is.na(x)))
sapply(pisa.train, function(x) table((is.na(x))))

# PROBLEM 1.4 - REMOVING MISSING VALUES. Answer: 1) 2414  2) 990
pisa.train <- na.omit(pisa.train)
pisa.test <- na.omit(pisa.test)
nrow(pisa.train)
nrow(pisa.test)

# PROBLEM 2.1 - FACTOR VARIABLES. Answer: 1) raceeth  2) grade
unique(as.factor(pisa.test$grade))
unique(pisa.test$raceeth)
unique(as.factor(pisa.test$male))

# PROBLEM 2.2 - UNORDERED FACTORS IN REGRESSION MODELS. Answer: All except ref.level "White"
# To include unordered factors in a linear regression model, we define one level as the "reference level"
# and add a binary variable for each of the remaining levels.
# ***TODO:....
pisa.train$raceeth <- relevel(pisa.train$raceeth, "White")
pisa.testraceeth <- relevel(pisa.test$raceeth, "White")
LinReg <- lm(readingScore ~ ., data=pisa.train)
summary(LinReg)

# PROBLEM 2.3 - EXAMPLE UNORDERED FACTORS. Answer: 1) Asian: "Asian"=1, else 0.  2) White: All 0.


# PROBLEM 3.1 - BUILDING A MODEL. Answer: 0.325
pisa.train$raceeth <- relevel(pisa.train$raceeth, "White")
pisa.testraceeth <- relevel(pisa.test$raceeth, "White")
levels(pisa.train$raceeth)
LinReg <- lm(readingScore ~ ., data=pisa.train)
summary(LinReg)

# PROBLEM 3.2 - COMPUTING THE ROOT-MEAN SQUARED ERROR OF THE MODEL. Answer: 73.37
SSE <- sum(LinReg$residuals^2)
SSE
SST <- sum((mean(pisa.train$readingScore) - pisa.train$readingScore)^2)
SST
R2 <- 1 - (SSE/SST)
R2
RMSE <- sqrt(SSE/nrow(pisa.train))
RMSE

# PROBLEM 3.3 - COMPARING PREDICTIONS FOR SIMILAR STUDENTS. Answer: 59.09
summary(LinReg)
unique(pisa.train$grade)
as.factor(pisa.test$grade)
str(pisa.train) # Note that grade 9 is first
# Coefficient for grade is 29.54271. Grade 9 is the "reference level". So from 9 to 11, it is 29.54271 * 2.
grade11 <- 29.54271 + (29.54271 * 2) # Grade 9 (29.54271) + Grade 10 (29.54271) + Grade 11 (29.54271)
grade9 <- 29.54271
grade11 - grade9

# PROBLEM 3.4 - INTERPRETING MODEL COEFFICIENTS. Correct answer: Predicted difference in the reading score
# between an Asian student and a white student who is otherwise identical 
# http://www.theanalysisfactor.com/interpreting-regression-coefficients/

# PROBLEM 3.5 - IDENTIFYING VARIABLES LACKING STATISTICAL SIGNIFICANCE. Answer: See summary(LinReg)
summary(LinReg)

# PROBLEM 4.1 - PREDICTING ON UNSEEN DATA. Answer: 284.5
predict.result <- predict(LinReg, pisa.test)
max(predict.result) - min(predict.result)

# PROBLEM 4.2 - TEST SET SSE AND RMSE. Answer: 1) SSE: 5762082 2) RMSE: 76.29
SSE <- sum((pisa.test$readingScore - predict.result)^2)
SSE
RMSE <- sqrt(SSE/nrow(pisa.test))
RMSE
SST <- sum((pisa.test$readingScore - mean(pisa.train$readingScore))^2)
SST
R2 <- 1 - SSE/SST
R2

# PROBLEM 4.3 - BASELINE PREDICTION AND TEST-SET SSE. Answer: 1) 518  2) 7802354
SST <- sum((pisa.test$readingScore - mean(pisa.train$readingScore))^2)
SST
predicted.test.score <- mean(predict(LinReg, pisa.train))
predicted.test.score
# Predicted test score = average score based on baseline model.
# Here, baseline very specifically refers to the baseline type model (the one that is a flat,
# horizontal line) which is related to the R-Square calculations that were introduced in this Unit
# http://stats.stackexchange.com/questions/35617/how-to-correct-for-correlation-at-baseline-between-predictor-and-dv

# PROBLEM 4.4 - TEST-SET R-SQUARED. Answer: 0.2615
R2 <- 1 - SSE/SST
R2


# 3) DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA
# ----------------------------------------------------
# We would like to estimate influenza-like illness (ILI) activity using Google web search logs.
# Google Flu Trends: https://www.google.org/flutrends/us/#US
# Google Search Queries: http://www.google.com/trends/
# ILI Data: http://www.cdc.gov/flu/weekly/fluactivitysurv.htm

# PROBLEM 1.1 - UNDERSTANDING THE DATA (NOTE: 6 points possible!). Answer: 1) 2009-10-18  2) 2009-10-18
folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 2/Assignment/"
FluTrain <- read.csv(paste0(folder, "FluTrain.csv"), header=T, stringsAsFactors=F)
str(FluTrain)
pairs(FluTrain, main="Flu queries", col="blue")
cor(FluTrain[,c(2,3)])
FluTrain$Date <- as.Date(FluTrain$Week)
head(FluTrain)
sapply(FluTrain, class)
CorrelationPlot(FluTrain[,c(2,3)])
# Looking at the time period 2004-2011, which week corresponds to the highest percentage of
# ILI-related physician visits? Select the day of the month corresponding to the start of this week.
FluTrain[which(FluTrain$ILI == max(FluTrain$ILI)), ] # 2009-10-18
FluTrain[which(FluTrain$Queries == max(FluTrain$Queries)), ] # 2009-10-18

# PROBLEM 1.2 - UNDERSTANDING THE DATA. Answer: Most of the ILI values are small, with a relatively small
# number of much larger values (in statistics, this sort of data is called "skew right").
hist(FluTrain$ILI, col="wheat")
hist(FluTrain$Queries, col="wheat")

# PROBLEM 1.3 - UNDERSTANDING THE DATA. Answer: There is a positive, linear relationship between log(ILI) and Queries
par(mfrow=c(1,2))
with(FluTrain, plot(ILI ~ Queries, col="darkcyan", main="(ILI) ~ (Queries)", pch=16))
with(FluTrain, plot(log(ILI) ~ Queries, col="violetred2", main="log(ILI) ~ Queries", pch=16))
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
abline(FluTrend1, col="blue")
par(mfrow=c(1,1))

# PROBLEM 2.1 - LINEAR REGRESSION MODEL. Answer: log(ILI) = intercept + coefficient x Queries, where the coefficient is positive
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# PROBLEM 2.2 - LINEAR REGRESSION MODEL. Answer: R2 = 0.709
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)

# PROBLEM 2.3 - LINEAR REGRESSION MODEL. Answer: R2 and cor relationship is: cor(log(ILI), Queries)^2
with(FluTrain, cor(log(ILI), Queries))
R.squared <- 0.709
with(FluTrain, cor(log(ILI), Queries)^2) # NOTE: R2 = cor(x,y)^2: Only valid for a single-variable model
with(FluTrain, log(1 / cor(log(ILI), Queries)))
with(FluTrain, exp(-0.5 * cor(log(ILI), Queries)))

# PROBLEM 3.1 - PERFORMANCE ON THE TEST SET. Answer: Row 11 is: predicted: 2.187 actual: 2.293
FluTest <- read.csv(paste0(folder, "FluTest.csv"), header=T, stringsAsFactors=F)
str(FluTest)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest)) # NOTE: Need exp() ion predict because of log() on y.
FluTest[which(as.Date(FluTest$Week) == "2012-03-11"), ] # row 11
PredTest1[11]

# PROBLEM 3.2 - PERFORMANCE ON THE TEST SET. Answer: 0.04623
# NOTE: The relative error is calculated as: (Observed ILI - Estimated ILI) / Observed ILI
observed.ILI <- 2.293
estimated.ILI <- 2.187
(observed.ILI - estimated.ILI) / observed.ILI

# PROBLEM 3.3 - PERFORMANCE ON THE TEST SET. Answer: RMSE = 0.7491
SSE <- sum((FluTest$ILI - PredTest1)^2)
SSE
RMSE <- sqrt(SSE / nrow(FluTest))
RMSE

# PROBLEM 4.1 - TRAINING A TIME SERIES MODEL. Answer: NA's in ILILag2 variable: 2
library(zoo)
par(mfrow=c(2,1))
with(FluTrain, plot(ILI ~ as.Date(Week), col="cornflowerblue", pch=16, main="ILI by Week"))
with(FluTrain, plot(log(ILI) ~ as.Date(Week), col="violetred3", pch=16, main="log(ILI) by Week"))
par(mfrow=c(1,1))
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)
# In these commands, the value of -2 passed to lag means to return 2 observations before the current one;
# a positive value would have returned future observations. The parameter na.pad=TRUE means to add missing
# values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier.
table(is.na(FluTrain$ILILag2))

# PROBLEM 4.2 - TRAINING A TIME SERIES MODEL. Answer: There is a strong positive relationship between log(ILILag2) and log(ILI)
with(FluTrain, plot(log(ILI) ~ ILILag2, col="violetred3", pch=16, main="ILI by ILILag2"))

# PROBLEM 4.3 - TRAINING A TIME SERIES MODEL. Answer: 1) Significant: Intercept, Queries, log(ILILag2)  2) R2: 0.906
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrend2)

# PROBLEM 4.4 - TRAINING A TIME SERIES MODEL. Answer: FluTrend2 is a stronger model than FluTrend1 on the training set
# Better R2, better significance levels

# PROBLEM 5.1 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET. Answer: 2
ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)
table(is.na(FluTest$ILILag2))

# PROBLEM 5.2 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET.
# Answer: 1) The ILI value of the second-to-last observation in the FluTrain data frame.
# Answer: 2) The ILI value of the last observation in the FluTrain data frame.
# 1) Which value should be used to fill in the ILILag2 variable for the first observation in FluTest?
# 2) Which value should be used to fill in the ILILag2 variable for the second observation in FluTest?

# PROBLEM 5.3 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET. Answer: 1) 1.853   2) 2.124
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1:2] == FluTrain$ILI[(nrow(FluTrain)-1):nrow(FluTrain)]

# PROBLEM 5.4 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET. Answer: RMSE = 0.2942
PredTest2 = exp(predict(FluTrend2, newdata=FluTest)) # NOTE: Need exp() ion predict because of log() on y.
SSE <- sum((FluTest$ILI - PredTest2)^2)
SSE
RMSE <- sqrt(SSE / nrow(FluTest))
RMSE

# PROBLEM 5.5 - EVALUATING THE TIME SERIES MODEL IN THE TEST SET. Answer: FluTrend2 (RMSE 0.2942 is better than 0.7491)

# In this problem, we used a simple time series model with a single lag term. ARIMA models are a more general
# form of the model we built, which can include multiple lag terms as well as more complicated combinations of
# previous values of the dependent variable. If you're interested in learning more, check out ?arima or the
# available online tutorials for these sorts of models.


# 4) STATE DATA (OPTIONAL)
# ------------------------
data(state) # Data from the 1970s on all fifty US states
head(state.x77)
class(state.x77) # Matrix
# convert to df and colbind in a lot of data in the workspace (loaded with data(state)):
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
head(statedata)
str(statedata)
pairs(statedata, col="blue", main="US State Data")
CorrelationPlot(statedata)
# Do some exploratory plots to find predictors influencing Life.Exp:
with(statedata, plot(Life.Exp ~ y, col="blue", main="Life.Exp versus Latitude", pch=16)) # Further South, less Life.Exp
with(statedata, plot(Life.Exp ~ Income, col="blue", main="Life.Exp versus Income", pch=16)) # Larger Income, greater Life.Exp
with(statedata, plot(Life.Exp ~ Illiteracy, col="blue", main="Life.Exp versus Illiteracy", pch=16)) # Smaller Illiteracy, greater Life.Exp
model <- lm(Life.Exp ~ Income, data=statedata)
summary(model) # Note: Positive here, but negative used with all other predictors! Multicollinearity!
abline(model, col="red")
with(statedata, plot(Life.Exp ~ Murder, col="blue", main="Life.Exp versus Murder Rate", pch=16)) # Larger MurderRate, less Life.Exp
with(statedata, plot(Life.Exp ~ HS.Grad, col="blue", main="Life.Exp versus HS.Grad", pch=16)) # Better HS.Grad, greater Life.Exp

# PROBLEM 1.1 - DATA EXPLORATION. Answer: statedata$x as first param (but can of course also plot as Ex. #2)
plot(statedata$x, statedata$y, col="blue", pch=16, main="State center lat/lon")
plot(statedata$y ~ statedata$x, col="blue", pch=16, main="State center lat/lon")

# PROBLEM 1.2 - DATA EXPLORATION. Answer: West had the highest average HS.Grad of all the states in the region
names(statedata)
tapply(statedata$HS.Grad, statedata$state.region, max, na.rm=T)

# PROBLEM 1.3 - DATA EXPLORATION. Answer: South region has the highest median murder rate
boxplot(statedata$Murder ~ statedata$state.region, col="wheat", main="Murder rate by region")

# PROBLEM 1.4 - DATA EXPLORATION. Answer: New York has the highest murder rate in the Northeast region
# Find the outlier in Northeast region:
region.northeast <- subset(statedata, state.region == "Northeast")
par(mar=c(6,3,2,1))
barplot(region.northeast$Murder, names.arg=region.northeast$state.name, cex.names=.7, las=2, col=2:8,
        main="Murder rate by states in Northeast region")
region.northeast[region.northeast$Murder == max(region.northeast$Murder), ]
par(mar=c(3,3,2,1))

# PROBLEM 2.1 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL. Answer: -2.18e-05
model <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(model) # Note that income shows a negative cor with Life.Exp. Must be multicollinearity here!

# PROBLEM 2.2 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL  
# Call the coefficient for income x (the answer to Problem 2.1). What is the interpretation of the coefficient x?
# Answer: For a one unit INCREASE in income, predicted life expectancy DECREASES by |x| (IMPORTANT TO UNDERSTAND THIS!!)
# If we increase income by one unit, then our model's prediction will increase by the coefficient of income, x.
# Because x is negative, this is the same as predicted life expectancy decreasing by |x|.

# PROBLEM 2.3 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL. Answer: Life expectancy is somewhat positively correlated with income.
plot(statedata$Income, statedata$Life.Exp)

# PROBLEM 2.4 - PREDICTING LIFE EXPECTANCY - AN INITIAL MODEL. Answer: Multicollinearity

# PROBLEM 3.1 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS. Answer: Murder+Frost+Population+HS.Grad
# Check if statedata$Income is highly correlated with something, and remove that:
CorrelationPlot(statedata)
model1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
summary(model1)
# Also note that Illiteracy shows a positive correlation with Life.Exp. This is also wrong!
# Income and HS.Grad are highly correlated. Try removing HS.Grad:
model2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + Frost + Area, data=statedata) # R2/adj.R2: 0.709/0.668
summary(model2) # Now income is positive, and illiteracy is negative, as it should be
mean(statedata$Life.Exp) # NOTE: This is the y-intercept, when all predictors are set to zero.
# Try removing unnecessary predictors. Should end up with a 4 predictor model:
model3 <- lm(Life.Exp ~ Murder + Frost + Population + HS.Grad, data=statedata) # R2/adj.R2: 0.736/0.713. Simplified and improved!
summary(model3)

# PROBLEM 3.2 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS. Answer:
# We expect the "Multiple R-squared" value of the simplified model to be slightly worse than that of the initial model.
# It can't be better than the "Multiple R-squared" value of the initial model.

# PROBLEM 3.3 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS. Answer: 1) Alabama 2) South Carolina
result <- predict(model3)
sort(result, decreasing=F)[1:10]
statedata[which.min(statedata$Life.Exp), ]

# PROBLEM 3.4 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS. Answer: 1) Washington 2) Hawaii
result <- predict(model3)
sort(result, decreasing=T)[1:10]
statedata[which.max(statedata$Life.Exp), ]

# PROBLEM 3.5 - PREDICTING LIFE EXPECTANCY - REFINING THE MODEL AND ANALYZING PREDICTIONS.
# Answer: Smallest residuals: Indiana Biggest: Hawaii
res <- model3$residuals
res[which.min(abs(res))] # Remember to use abs() since residuals can be both positive and negative!
res[which.max(res)]
statedata[which.min(abs(res)), ]
statedata[which.max(res), ]
result <- predict(model3)
sort(abs(result - statedata$Life.Exp))[1:5]


# 5) FORECASTING ELANTRA SALES (OPTIONAL)
# ---------------------------------------

# TODO: Do some timeseries stuff on this?

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 2/Assignment/"
elantra <- read.csv(paste0(folder, "elantra.csv"), header=T)
# Month = the month of the year for the observation (1 = January, 2 = February, 3 = March, ...).
# Year = the year of the observation.
# ElantraSales = the number of units of the Hyundai Elantra sold in the United States in the given month.
# Unemployment = the estimated unemployment percentage in the United States in the given month.
# Queries = a (normalized) approximation of the number of Google searches for "hyundai elantra" in the given month.
# CPI_energy = the monthly consumer price index (CPI) for energy for the given month.
# CPI_all = the consumer price index (CPI) for all products for the given month; this is a measure of the magnitude
#    of the prices paid by consumer households for goods and services (e.g., food, clothing, electricity, etc.).
str(elantra)
pairs(elantra, col="blue")
CorrelationPlot(elantra)
with(elantra, plot(ElantraSales ~ Unemployment, col="blue", pch=16, main="ElantraSales ~ Unemployment"))
with(elantra, plot(ElantraSales ~ CPI_all, col="blue", pch=16, main="ElantraSales ~ CPI_All")) # Strange, year cor? Yes! See below.
with(elantra, plot(ElantraSales ~ CPI_energy, col="blue", pch=16, main="ElantraSales ~ CPI_Energy")) # Strange, year cor? Yes! Sww below.
with(elantra, boxplot(CPI_all ~ Year, col="orange", pch=16, main="CPI_All ~ Year")) # Highly correlated! See above.
with(elantra, boxplot(CPI_energy ~ Year, col="orange", pch=16, main="CPI_Energy ~ Year")) # Highly correlated! See above.
with(elantra, boxplot(CPI_all ~ Month, col="orange", pch=16, main="CPI_All ~ Month")) # cyclic?
with(elantra, boxplot(CPI_energy ~ Month, col="orange", pch=16, main="CPI_Energy ~ Month")) # cyclic?
with(elantra, boxplot(ElantraSales ~ Year, col="violetred4", pch=16, main="ElantraSales ~ Year"))
with(elantra, boxplot(ElantraSales ~ Month, col="violetred4", pch=16, main="ElantraSales ~ Month"))
with(elantra, boxplot(ElantraSales ~ Unemployment, col="cornflowerblue", pch=16, main="ElantraSales ~ Unemployment", las=2))
with(elantra, plot(ElantraSales ~ Unemployment, col="cornflowerblue", pch=16, main="ElantraSales ~ Unemployment"))

# PROBLEM 1 - LOADING THE DATA. Answer: 50 rows in training set
train <- elantra[elantra$Year <= 2012, ]
test <- elantra[elantra$Year > 2012, ]
nrow(train)

# PROBLEM 2.1 - A LINEAR REGRESSION MODEL. Answer: R2 is 0.428
model1 <- lm(ElantraSales ~ Unemployment+CPI_all+CPI_energy+Queries, data=train)
summary(model1)

# PROBLEM 2.2 - SIGNIFICANT VARIABLES. Answer: 0 significant variable at 0.10 cutoff (Queries have 0.10, which is over cutoff)

# PROBLEM 2.3 - COEFFICIENTS. Answer: -3179.9 is the coeff of the Unemployment var.

# PROBLEM 2.4 - INTERPRETING THE COEFFICIENT. Answer: Below.
# What is the interpretation of this coefficient?
# Answer: For an increase of 1 in Unemployment, the prediction of Elantra sales decreases by approximately 3000.

# PROBLEM 3.1 - MODELING SEASONALITY. Answer: R2 = 0.434
model2 <- lm(ElantraSales ~ Month+Unemployment+CPI_all+CPI_energy+Queries, data=train)
summary(model2)

# PROBLEM 3.2 - EFFECT OF ADDING A NEW VARIABLE. Answer: The model is not better because the adjusted R-squared
# has gone down and none of the variables (including the new one) are very significant.
# Which of the following best describes the effect of adding Month?
# NOTE: Adj.R2 is down compared to model1!

# PROBLEM 3.3 - UNDERSTANDING THE MODEL. Answer:
# *** TODO...

# PROBLEM 3.4 - NUMERIC VS. FACTORS. Answer:
# *** TODO...

# PROBLEM 4.1 - A NEW MODEL. Answer: Multiple R-squared:  0.819 (Adjusted R-squared:  0.684)
train$MonthAsFactor <- as.factor(train$Month)
test$MonthAsFactor <- as.factor(test$Month)
model3 <- lm(ElantraSales ~ MonthAsFactor+Unemployment+CPI_all+CPI_energy+Queries, data=train)
summary(model3)

# PROBLEM 4.2 - SIGNIFICANT VARIABLES. Answer: All except Queries
# Which variables are significant, or have levels that are significant? Use 0.10 as your p-value cutoff.
summary(model3) # NOTE: The sign of the Queries variable has changed.

# PROBLEM 5.1 - MULTICOLINEARITY. Answer: (Month) Unemployment, Queries, CPI_All
# Which of the following variables is CPI_energy highly correlated with?
CorrelationPlot(train)

# PROBLEM 5.2 - CORRELATIONS. Answer: (Year) Unemployment, CPI_Energy, CPI_all
# Which of the following variables is Queries highly correlated with?

# PROBLEM 6.1 - A REDUCED MODEL. Answer: Removed Queries
summary(model3) # NOTE: The sign of the Queries variable has changed.
model4 <- lm(ElantraSales ~ MonthAsFactor+Unemployment+CPI_all+CPI_energy, data=train) # Removed Queries
summary(model4)

# PROBLEM 6.2 - TEST SET PREDICTIONS. Answer: SSE = 190757747
pred <- predict(model4, test)
pred
SSE <- sum((test$ElantraSales - pred)^2)
SSE
SST <- sum((test$ElantraSales - mean(train$ElantraSales))^2)
SST
R2 <- 1 - SSE/SST
R2

# PROBLEM 6.3 - COMPARING TO A BASELINE. Answer: 14462
predicted.test.score <- mean(predict(model4, train))
predicted.test.score
# Predicted test score = average score based on baseline model.
# Explanation: The baseline method that is used in the R-Squared calculation (to compute SST, the total
# sum of squares) simply predicts the mean of ElantraSales in the training set for every observation
# (i.e., without regard to any of the independent variables).

# PROBLEM 6.4 - TEST SET R-SQUARED. Answer: 
pred <- predict(model4, test)
pred
SSE <- sum((test$ElantraSales - pred)^2)
SSE
SST <- sum((test$ElantraSales - mean(train$ElantraSales))^2)
SST
R2 <- 1 - SSE/SST
R2

# PROBLEM 6.5 - ABSOLUTE ERRORS. Answer: 7491.5
sort(abs(test$ElantraSales - pred), decreasing=T)

# PROBLEM 6.6 - MONTH OF LARGEST ERROR. Answer: March 2013 (03/2013)
result <- abs(test$ElantraSales - pred)
pos <- as.integer(which(result == max(result)))
test[pos, ]
