# Elements of Statistical Learning
# --------------------------------
# http://www-bcf.usc.edu/~gareth/ISL/data.html
# http://statweb.stanford.edu/~tibs/ElemStatLearn/data.html
# See: UCI Machine Learning Repository - http://archive.ics.uci.edu/ml/

set.seed(1000)
SetStandardOptions()
folder <- "C:/coding/R/Coursera/edX StatisticalLearning/Datasets/"
package.install("ElemStatLearn")
library(ElemStatLearn)

# Challenges:

# 1) Identify the risk factors for prostate cancer


# 2) Classify a recorded phoneme based on a log-periodogram
data(phoneme)


# 3) South African Heart Disease Data (Men). Predict whether someone will have a heart attack on the basis
#    of demographic, diet and clinical measurements
# http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/linearclassification/linearclassification.R
hr <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data",
                 sep=",",head=T,row.names=1)
head(hr)
str(hr)
dim(hr)
sapply(hr, class)
pairs(hr[,c(-4,-6,-10)], col=hr$chd + 2, cex=1, cex.labels=1)
pairs(hr[,c(-4,-6,-10)], col=c("red", "turquoise"), cex=.8, cex.labels=1)
hr.lr <- glm(chd ~ sbp + tobacco + ldl + famhist + obesity + alcohol + age , family = binomial , data=hr)
summary(hr.lr)
heartfit <- glm(chd ~ ., data=hr, family=binomial)
heartfit <- glm(chd ~ tobacco+ldl+typea+famhist+age, data=hr, family=binomial)
summary(heartfit)
CorrelationPlot(hr)

# 4) Customize an email spam detection system
# http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/
# TODO: Classification tree


# 5) Identify the numbers in a handwritten zip code


# 6) Classify a tissue sample into one of sveral cancer classes, based on a gene expression profile
# TODO: Dendrogram, partial clustering, heatmap


# 7) Establish the relationship between salary and demographic variables in population survey data
#    (Income survey data for males, USA, Central Atlantic Region, 2009)


# 8) Classify the pixels in a LANDSAT image, by usage


############################################################################################################3
# CH2 OVERVIEW OF STATISTICAL LEARNING

# e = epsilon = irreducible error
# Nearest Neighbour Average, N(x): Take the average of the x-values closest to the x in question, to estimate y:
# f<hat>y = Average(Y|X part of N(x))
package.install("ISLR")
library(ISLR)
library(MASS)
data(Wage)
model1 <- lm(wage ~ age + year + education + jobclass, data=Wage)
summary(model1)
# TODO: Turn the xaxis labels vertical!
plot(Wage$wage ~ Wage$education, cex.axis=.5, las=1, col=c(2,3,4,5,6))
plot(Wage$wage ~ Wage$jobclass, cex.axis=.7, las=1, col=c(2,3))

model2 <- lm(wage ~ age, data=Wage)
plot(wage ~ age, col=Wage$wage, main="Wage by age", data=Wage)
abline(model2, col="red", lwd=2)

############################################################################################################3
# CH 4 CLASSIFICATION
# Logistic Regression Formula:
n <- 50
e <- exp(1)
x <- rnorm(n)*5
y <- sample(c(0,1), n, replace=T)
b0 <- MyLinearRegressionIntercept(x, y)
b1 <- MyLinearRegressionSlope(x, y)
Prob.X <- (e^(b0 + b1*x)) / (1 + e^(b0 + b1*x))
Prob.X
log(Prob.X / (1 - Prob.X)) # = B0 + B1*X = model1$fitted = log odds/logit transformation function
round(log(Prob.X / (1 - Prob.X)))
y
model1 <- lm(y ~ x)
summary(model1)
b0 + b1*x # This is model1$fitted

# We use Maximum Likelihood to estimate the parameters:
# l(b0,b) = ProductOf((y<i>=1) Prob.x<i>) * ProductOf((y<i>=0) 1 - Prob.x<i>) 

# Work with the Advertising data set:
# http://digitheadslabnotebook.blogspot.no/2014/02/linear-models.html
# TODO: Get all datasets from course site and keep in course/Coursera folder?
Advertising <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv", row.names = 1)
head(Advertising)

# Get the lm coefficients:
X <- Advertising$TV
Y <- Advertising$Sales
x_bar <- mean(X)
y_bar <- mean(Y)
beta_hat_1 <- sum((X - x_bar) * (Y - y_bar))/sum((X - x_bar)^2)
beta_hat_0 <- y_bar - beta_hat_1 * x_bar
cat(beta_hat_0, beta_hat_1)

# Tips: http://digitheadslabnotebook.blogspot.no/2014/02/regression-with-multiple-predictors.html
fit <- lm(Sales ~ TV, data=Advertising)
coef(fit)
summary(fit)
# Plot with nice graphics (note the 'segments' function call): 
plot(Sales ~ TV, data = Advertising, type = "n")
segments(Advertising$TV, Advertising$Sales, Advertising$TV, predict(fit, Advertising), col = "#cccccc")
points(Sales ~ TV, data = Advertising, pch = 21, col = "#990000", bg = "#ffcccc")
abline(fit, col = "blue")
title("Linear model fit to Advertising data with residuals")

n = nrow(Advertising)
y_hat = predict(fit, Advertising)
rss = sum((Advertising$Sales - y_hat)^2)
rse_squared = rss/(n - 2)
cat("RSE =", sqrt(rse_squared))

se_beta_0 = sqrt(rse_squared * (1/n + (x_bar^2)/sum((X - x_bar)^2)))
se_beta_1 = sqrt(rse_squared/sum((X - x_bar)^2))
cat(se_beta_0, se_beta_1)

# Get confidence intervals:
const <- 1.96
c(beta_hat_0 - const * se_beta_0, beta_hat_0 + const * se_beta_0)
confint(fit)

# Get the t-statistic for intercept (beta0) and beta1:
t.statistic.intercept = beta_hat_0/se_beta_0
p.value1 = 2 * pt(t.statistic.intercept, lower.tail = F, df = n - 2)
cat(t.statistic.intercept, p.value1)
t.statistic.beta1 = beta_hat_1/se_beta_1
p.value2 = 2 * pt(t.statistic.beta1, lower.tail = F, df = n - 2)
cat(t.statistic.beta1, p.value2)

# Here we're fitting 100 models from different random subsamples of the Advertising data:
plot(Sales ~ TV, data = Advertising, type = "n", cex.axis=.8)
points(Sales ~ TV, data = Advertising, pch = 21, col = "#99000040", bg = "#ff000040")
for (i in 1:100) {
  fit <- lm(Sales ~ TV, data = Advertising[sample(1:nrow(Advertising), nrow(Advertising) * 2/3), ])
  abline(fit, col = "#0000ff10")
}
title("Linear models tend to have low variance")

# 4.3 Multivariate Logistic Regression:
data(package="ElemStatLearn")
head(SAheart)
pairs(~ sbp + tobacco + ldl + famhist + obesity + alcohol + age,
      data=SAheart, pch=21, bg=c("red", "turquoise")[factor(SAheart$chd)])
pairs(SAheart[1:9], pch=21, bg=c("red","green")[factor(SAheart$chd)])
model1 <- glm(chd ~., family=binomial, data=SAheart)
summary(model1)

# 4.4 Case-control sampling and logistic regression:
# There are 160 cases of MI (Myocardial Infarction), and 302 controls, so the ratio
# of MI cases (pi~) is 0.35. But the prevalence of MI in the area is just 0.05 (pi).
# So, we correct the intercept (B0) thus:
# B0<new> = B0 + log(pi/1-pi) - log(pi~/1-pi~)
pi.tilde <- 0.35
pi.real <- 0.05
B0 <- model1$coeff[1]
B0.new <- B0 + log(pi.real/(1-pi.real)) - log(pi.tilde/(1-pi.tilde))
B0.new
# Sample about 5-6 controls for every case. More than that, the accuracy levels out.
# Tip for multiclass/multinomial regression:
# Use "glmnet" for logistic regression for more than two classes.

# 4.5 Discriminant Analysis, one variable:
# Bayes theorem for classification: Pr(Y = k|X = x) = (Pr(X = x|Y = k) * Prob(Y = k)) / Pr(X = x)
# Example:
n <- 10
y <- sample(c(1,2,3), n, replace=T)
y
x <- abs(rnorm(n)) * y
x
#prior.k <- n<k>/n
k <- 1
prior.k <- length(y[y == k])/length(y) # = Pr(k)
prior.k
# mu.k <- 1/n<k> * SumOf(i:y<i>=k)x<i>
mu.k <- (1/length(y[y == k])) * sum(x[y == k])
# sigma.squared <- (1/n-K) * SumOf(k=1:K) * SumOf(i:y<i>=k)(x<i>-mu.k)^2
sigma.squared <- (1/(n-length(unique(y)))) * length(unique(y)) * sum((x[y == k] - mu.k)^2)
sigma.squared
DisciminantScore.x <- x * (mu.k/sigma.squared)  - ((mu.k^2)/(2*sigma.squared)) + log(prior.k)
DisciminantScore.x
plot(DisciminantScore.x, pch=19)
lines(DisciminantScore.x)
# Using Discriminant Analysis on Fisher's Iris dataset:
pairs(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris,
      col=c("Turquoise3", "Orange2", "Green3")[factor(iris$Species)])
# When there are K classes, you can see a really good separation in the classes in a K-1 dimensional plot
# (Discriminant Var. #1 on x-axis and Discriminant Var. #2 on y-axis)


# Chapter 5: Resampling:
# ----------------------
# Bias: How far out on the average the model is from the truth
# Variance: How much the estimate vary around its average
library(ISLR)
plot(mpg ~ horsepower, data=Auto, pch=19, col="blue")
model1 <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
model2 <- lm(mpg ~ horsepower, data=Auto)
summary(model1)
summary(model2)
abline(model2, col="red")
abline(model1$coeff[1], model1$coeff[2], col="green", lwd=2)
plot(model1$resid ~ model1$fitted, col="blue")
plot(model2$resid ~ model2$fitted, col="blue")

dim(Auto)
folds <- 4
total.rows <- dim(Auto)[1]
sample.split <- total.rows / folds
rowlist <- list()

# NOTE: To split like below, one has to check first that the data does not have any bias/sorting
# NOTE: Production year makes a difference here, and this is randomized most in set 1 and least in set 4!
for (counter in 1:folds) {
  rowlist[[counter]] <- sample(total.rows - (sample.split * (counter - 1)), sample.split, replace=F)
}

fit <- lm(mpg ~ horsepower, data=Auto[rowlist[[1]], ])
pred <- predict(fit, newdata=Auto[rowlist[[2]], ], type="response")
mse1 <- MyMSE(Auto[rowlist[[2]], "mpg"], pred)
mse1
fit <- lm(mpg ~ horsepower, data=Auto[rowlist[[4]], ])
pred <- predict(fit, newdata=Auto[rowlist[[1]], ], type="response")
mse2 <- MyMSE(Auto[rowlist[[1]], "mpg"], pred)
mse2

# The bootstrap:
# --------------
total <- 10000
my.sample.x <- rnorm(total)
my.sample.y <- rnorm(total) + my.sample.x
model1.pop <- lm(my.sample.y ~ my.sample.x)
summary(model1.pop)
std.error.pop <- coef(summary(model1.pop))[, 2][2]
n <- 50
std.error <- numeric(n)

for (counter in 1:n) {
  rows <- sample(1:total, replace=T)
  x.sample <- my.sample.x[rows]
  y.sample <- my.sample.y[rows]
  model1 <- lm(y.sample ~ x.sample)
  std.error[counter] <- coef(summary(model1))[, 2][2]
}

sum(std.error) / n
hist(std.error, col="powderblue", main="Bootstrap example")
abline(v=(sum(std.error) / n), col="red", lwd=2)

if (std.error.pop != (sum(std.error) / n)) {
  abline(v=std.error.pop, col="blue", lwd=2)
}
# Confidence interval: Bootstrap Percentile Interval:
# http://en.wikipedia.org/wiki/Bootstrapping_(statistics)#Methods_for_bootstrap_confidence_intervals
abline(v=quantile(std.error, c(.25,.975)), col="green", lwd=2)

# For time series, do Block Bootstrap

# Ch 6: Linear Model Selection and Regularization
# -----------------------------------------------
