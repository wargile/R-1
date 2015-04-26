# TODO: Stuff to work on:
# -----------------------

# 10-fold cross validation

# K-fold cross validation:
# - http://en.wikipedia.org/wiki/Cross-validation_(statistics)
# - Jeff Leek video

# k nearest neighbor

# SVM

# bootstrapping

# Mediation/moderation

# Hypothesis testing and p-value/significance

# Create the formula list from StatisticsOne post: https://class.coursera.org/stats1-002/forum/thread?thread_id=2128


# Create a total formula list from StatisticsOne course, etc.! And/or update tools.R
PE <- read.csv("C:/coding/R/Coursera/StatisticsOne/Week 4/Assignment/DataForLab4.txt", header=T, sep="\t")
head(PE)
model1 <- lm(PE$endurance ~ PE$age)
summary(model1)
n <- nrow(PE)

# Quantiles
quantile(residuals(model1), c(0, 0.25, 0.5, 0.75, 1))

# Estimate B1
beta1 <- var(PE$age, PE$endurance) / var(PE$age)
beta1

# Estimate B0/Intercept
beta0 <- mean(PE$endurance) - beta1 * mean(PE$age)
beta0

# SST
SST <- sum((PE$endurance - mean(PE$endurance))^2)
SST

# SSE
SSE <- sum((fitted(model1) - mean(PE$endurance))^2)
SSE
SSR <- sum(residuals(model1)^2)
SSR

# SSE/SST = Multiple R-Squared
SSE/SST
# or:
1 - SSR/SST

# Adjusted R Squared
1 - (SSR / (n - 1 - 1)) / (SST / (n - 1))

# Residual standard error
SER2 <- SSR / (n - 2)
sqrt(SER2)

# Std.error b1
SSTx <- sum((PE$age - mean(PE$age))^2)
beta1.var <- SER2 / SSTx
sqrt(beta1.var)

# t-value b1
beta1 / sqrt(beta1.var)

# Pr(>|t|) for b1
2 * (1 - pt(abs(beta1 / sqrt(beta1.var)), n - 2))

# Std.error b0
beta0.var <- SER2 * sum(PE$age^2) / SSTx / n
sqrt(beta0.var)

# Pr(>|t|) for b0
2 * (1 - pt(abs(beta0 / sqrt(beta0.var)), n - 2))

# F-statistic
SSRr <- SST
SSRur <- SSR
q <- 1
F <- (SSRr - SSRur) / q / SSRur * (n - 2)
F

# p-value
1 - pf(F, q, n - 2)


##############################################################
# DataAnalysis Score
Quiz1 <- 10; Quiz2 <- 10; Quiz3 <- 10; Quiz4 <- 10; Quiz5 <- 10; Quiz6 <- 10;
Quiz7 <- 10; Quiz8 <- 10;
Assignment1score <- 68.5
Assignment2score <- 60.5

Total=Quiz1 + Quiz2 + Quiz3*(10/9) + Quiz4 + Quiz5 + Quiz6*(10/8) + Quiz7 +
  Quiz8 *(10/9) + Assignment1score*(40/85) + Assignment2score*(40/90)
Total
