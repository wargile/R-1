# Understanding logistic regression:
# ----------------------------------

# TODO: Understand log-likelihood...
# TODO: Understand odds ratios...
# TODO: Really understand Confidence Interval outputs...

# TIPS:
# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/complete_separation_logit_models.htm
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm

package.install("aod")
library(aod)
library(ggplot2)

set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
SetStandardOptions()

x <- seq(-pi, pi, .1)
sigmoid <- 1 / (1 + exp(1)^(-x))
plot(sigmoid, type="h", col="gray", main="The Logit/Sigmoid function", xlab="Predictors", ylab="Probability Y=1",
     ylim=c(0, 1))
lines(sigmoid, type="o", col="blue", lwd=2)

# http://www.ats.ucla.edu/stat/r/dae/logit.htm
# Example 2 about getting into graduate school
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
str(mydata)
# A researcher is interested in how variables, such as GRE (Graduate Record Exam scores), GPA (grade point average)
# and prestige of the undergraduate institution, effect admission into graduate school. The response variable,
# admit/don't admit, is a binary variable.
# This dataset has a binary response (outcome, dependent) variable called admit. There are three predictor variables:
# gre, gpa and rank. We will treat the variables gre and gpa as continuous. The variable rank takes on the values 1 through 4.
# Institutions with a rank of 1 have the highest prestige, while those with a rank of 4 have the lowest
pairs(mydata, col="blue")
head(mydata)
summary(mydata)
sapply(mydata, class)
sapply(mydata, sd)
barplot(table(mydata$rank), col="orange", main="Rank frequency")
# two-way contingency table:
with(mydata, table(admit, rank))

# convert outcome variable to factor:
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data=mydata, family="binomial")
summary(mylogit)
# Explanation:
# - For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
# - For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
# - The indicator variables for rank have a slightly different interpretation. For example, having attended an institution
#   with rank of 2, versus an institution with a rank of 1, changes the log odds of admission by -0.675.
with(mydata, plot(admit ~ rank ,col="wheat")) # The lower the rank value, the less the chance of admission (1)
# CIs using profiled log-likelihood
confint(mylogit)
# CIs using standard errors
confint.default(mylogit)

# How do I interpret odds ratios in logistic regression?
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm

# Test the sigmoid function on an actual dataset:
folder <- "C:/coding/R/testdata/"
census <- read.csv(paste0(folder, "census.csv"))
str(census)
model <- glm(as.factor(over50k) ~ age+capitalgain, data=census, family=binomial)
summary(model)
p <- predict(model, census, type="response")
table(census$over50k, p > 0.5) # cm table
coeffs <- model$coefficients
logit <- 1 / (1 + exp(1)^((coeffs[1] + (coeffs[2]*census$age) + (coeffs[3]*census$capitalgain))*-1))
table(round(logit)) # NOTE: Compare to cm table above. Same result as 'predicted' columns summed up
par(mfrow=c(2,1))
plot(logit, col="blue", type="l", main=("Logit function"), xlab="Predictors", ylim=c(0,1))
abline(h=0, col="red")
abline(h=1, col="red")
plot(sort(logit), col="blue", type="l", main=("Logit function"), xlab="Predictors", ylim=c(0,1))
abline(h=0, col="red")
abline(h=1, col="red")
par(mfrow=c(1,1))


# Logistic regression and logloss
n <- 250
y <- sample(0:1, n, replace=T)
x1 <- (y + rnorm(n)) + 1
x2 <- (y - rnorm(n)) * 1

fit <- glm(y ~ x1 + x2, family="binomial")
summary(fit)

x <- matrix(c(rep(1, n), x1, x2), ncol=3, byrow=F)
# NOTE: IMPORTANT: Need to add a column of 1's to X matrix IFF intercept is included in weights/coeffs!

par(mfrow=c(2,1))
z <- y * (as.numeric(fit$coefficients %*% t(x))) # TODO: How do we get z = fit$fitted.values here??
plot(z, type="o", col="blue", main="z")
logloss <- log(1 + exp(1)^(-z))
# Or just:
logloss <- log(1 + exp(-z))
plot(logloss, type="o", col="red", main="logloss")
par(mfrow=c(1,1))

average_logloss <- sum(logloss) / n
average_logloss
# Same as:
sum(log(1+exp(-z)))/n

# Sigmoid function
sigmoid <- 1 / (1 + exp(-z))
plot(sigmoid, pch=16, col="blue")
plot(sort(sigmoid), pch=16, col="blue")

sigmoid <- function(t) {
  return (1 + exp(-t))^(-1)
}

plot(sigmoid(as.numeric(fit$coefficients %*% t(x))))

plot(sign(as.numeric(fit$coefficients %*% t(x))))

# Calculate logloss:
z <- fit$fitted.values
epsilon <- 10e-12 # log(0) is undefined so need to add a small value to it, likewise subtract if p == 1
logloss <- numeric()
for (counter in 1:length(y)) {
  if (z[counter] == 1)
    z[counter] <- z[counter] - epsilon
  if (z[counter] == 0)
    z[counter] <- z[counter] + epsilon
  
  if (y[counter] == 0)
    logloss[counter] <- -log(1 - z[counter])
  else
    logloss[counter] <- -log(z[counter])
}
sum(logloss) / n
