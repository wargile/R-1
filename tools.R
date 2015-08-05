# tools.R -- Various stuff, just learning R...
# Copyright (C) Terje B 2013-2015
#
# http://en.wikipedia.org/wiki/Portal:Statistics

# Matches quantiles type 2,5, others?
MyQuantile <- function(data, return.names=TRUE) {
  if (is.null(data)) {
    warning("Data is NULL!")
    return()
  }
  
  for (counter in 1:length(data)) {
    if (is.na(data[counter])) {
      warning("Data is NA!")
      return()
    }
  }
  
  data <- sort(data)
  
  pos <- length(data) / 4.0
  
  min.quantile = data[1];
  
  if (pos %% 1 > 0)
    lower.quantile <- data[ceiling(pos)]
  else
    lower.quantile <- (data[pos] + data[pos + 1]) / 2.0
  
  pos <- 3 * length(data) / 4.0;
  
  if (pos %% 1 > 0)
    upper.quantile <- data[ceiling(pos)]
  else
    upper.quantile <- (data[pos] + data[pos + 1]) / 2.0
  
  pos <- length(data) / 2.0;
  
  if (pos %% 1 > 0)
    middle.quantile <- data[ceiling(pos)]
  else
    middle.quantile <- (data[pos] + data[pos + 1]) / 2.0
  
  max.quantile = data[length(data)]
  
  #quantiles.data <- data.frame(format(round(min.quantile, digits=2), nsmall = 2),
  #                             format(round(lower.quantile, digits=2), nsmall = 2),
  #                             format(round(middle.quantile, digits=2), nsmall = 2),
  #                             format(round(upper.quantile, digits=2), nsmall = 2),
  #                             format(round(max.quantile, digits=2), nsmall = 2))
  
  quantiles.data <- numeric(5)
  
  quantiles.data[1] <- round(min.quantile, digits=2)
  quantiles.data[2] <- round(lower.quantile, digits=2)
  quantiles.data[3] <- round(middle.quantile, digits=2)
  quantiles.data[4] <- round(upper.quantile, digits=2)
  quantiles.data[5] <- round(max.quantile, digits=2)      
  
  if (return.names)
    names(quantiles.data) <- c("0%","25%","50%","75%","100%")
  
  return(quantiles.data)
}

MyInterquartileRange <- function(data) {
  data <- MyQuantile(data, return.names=FALSE)
  
  range.data <- numeric(1)
  range.data <- data[4] - data[2]
  #names(range.data) <- "Range"
  return(range.data)
}

MyPercentile <- function(data, percentile=50) {
  if (percentile > 100 || percentile < 1 || is.na(percentile)) {
    warning("Percentile must be between 1 and 100!")
    return()
  }
  
  data <- sort(data)
  pos = (percentile * length(data)) / 100
  
  if (pos %% 1 > 0)
    percentile.data <- data[ceiling(pos)]
  else
    percentile.data <- (data[pos] + data[pos + 1]) / 2.0
  
  return(percentile.data)
}

MyArithmeticMean <- function(data) {
  # NOTE: A.k.a. "mean/avg"
  return(sum(data) / length(data))
}

MyGeometricMean <- function(data) {
  # http://en.wikipedia.org/wiki/Pythagorean_means
  # http://en.wikipedia.org/wiki/Geometric_mean
  
  # TEST:
  # x <- rnorm(100)
  # hist(x, col="orange", cex.main=1, cex.lab=.8, cex.axis=.8, main="Mean, geometric mean and median")
  # abline(v=MyArithmeticMean(x), col="red", lwd=2)
  # abline(v=MyGeometricMean(x), col="green4", lwd=2)
  # abline(v=MyMedian(x), col="blue", lwd=2)
  # legend("topright", legend=c("Mean","Geometric mean","Median"), col=c("Red","Green4","Blue"),
  #       lwd=2, cex=.7)
  
  temp <- 1.0
  root <- length(data)
  
  for (counter in 1:length(data)) {
    if (data[counter] < 0)
      temp <- temp * -abs(data[counter])^(1 / root) # NOTE handling of neg values!
    else
      temp <- temp * data[counter]^(1 / root)
  }
  
  return(temp)
}

MyHarmonicMean <- function(data) {
  # http://en.wikipedia.org/wiki/Pythagorean_means
  # http://en.wikipedia.org/wiki/Harmonic_mean
  
  temp <- 0.0
  root <- length(data)
  
  for (counter in 1:length(data)) {
    temp <- temp + (1 / data[counter])
  }
  
  return(length(data) / temp)
}

# TODO: MyWeightedArithmeticMean, etc. - or just add weights as a param to above methods??
# http://en.wikipedia.org/wiki/Weighted_arithmetic_mean

MyMedian <- function(data, remove.na=TRUE) {
  if (remove.na)
    data[!is.na(data)]
  
  if (!remove.na && length(is.na(data)) > 0)
    return(NA)
  
  data <- sort(data)
  
  if (length(data) %% 2 > 0) # odd number of elements
    return(data[(length(data) + 1) / 2.0])  
  else
    return((data[length(data) / 2.0] + data[(length(data) / 2.0) + 1]) / 2.0)
  
  # BOXPLOT Shows: lower bound, lower quartile, median, upper quartile and upper bound
  # boxplot(data)
}

MyVariance <- function(data, sample.based=TRUE) {
  if (length(data) < 2)
    return(NA)
  
  if (sample.based)
    my.variance <- sum((data - mean(data))^2) / (length(data) - 1)
    # (n-1: Used for inferential statistics. n: used for descriptive statistics)
  else
    my.variance <- sum((data - mean(data))^2) / length(data)
    # (n-1: Used for inferential statistics. n: used for descriptive statistics)
  
  return(my.variance)
}

MyStddev <- function(data, sample.based=TRUE) {
  if (is.matrix(data))
    return(NULL)
  # This gives sample based stddev (n-1 elements)
  # (n-1: Used for inferential statistics. n: used for descriptive statistics)
  # The lesser the stddev is, the closer the values are to the mean (that is, values are more consistent)
  return(sqrt(MyVariance(data, sample.based)))
}

MyStdScore <- function(value, data) {
  if (is.matrix(data))
    return(NULL)
  if (length(data) < 2)
    return(NA)
  
  return((value - MyArithmeticMean(data)) / MyStddev(data, sample.based=FALSE))
}

MyTScore <- function(data, population.mean=0) {
  # A t score is a statistic whose values are given by:
  # t = [ x - u ] / [ s / sqrt( n ) ]
  # where x is the sample mean, u is the population mean,
  # s is the standard deviation of the sample, n is the sample size, and t is the t score.
  x <- MyArithmeticMean(data)
  s <- MyStddev(data)
  n <- length(data)
  u <- population.mean
  
  return((x - u) / (s / sqrt(n)))
}

# Function to return type 2 skewness of data (used in SAS and SPSS)
MySkewness <- function(data, type=2) {
  my.temp <- 0
  my.length <- length(data)
  my.mean <- MyArithmeticMean(data)
  
  if (type == 2) {
    for (counter in 1:length(data))
      my.temp <- my.temp + (((data[counter] - my.mean) / MyStddev(data, sample.based=TRUE))**3)

    return((my.length / ((my.length - 1) * (my.length - 2))) * my.temp)
  }
  else return(warning("Unknown type. Only type 2 is currently supported."))
}

# Function to return mode of data (element with highest frequency)
MyMode <- function(data, round.digits=10) {
  data <- round(data, round.digits)
  d <- table(sort(data))
  names(d)[d == max(d)]
  # as.numeric(names(d)[d == max(d)])
  #as.numeric(names(sort(d))[length(d)])
}

MyZScore <- function(data) {
  # TEST IT: hist(MyZScore(rnorm(10000)), col="orange")
  
  (data - MyArithmeticMean(data)) / MyStddev(data, sample.based=F)
}

MySumOfCrossProducts <- function(x, y) {
  # Sum of CrossProducts (SP) = (X - Mx) x (Y - My)
  sum((x - MyArithmeticMean(x)) * (y - MyArithmeticMean(y)))  
}

MyMultipleRSquared <- function(x, y) {
  # R = (Covariance of X and Y) / (Variance of X and Y)
  # Regression coefficient - when the regression line is linear (y = ax + b)
  # The regression coefficient is the constant (a) that represents the rate of change
  # of one variable (y) as a function of changes in the other (x); it is the slope of the regression line
  
  # This calculation gives the value of multiple R-squared
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.X <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  intercept.Y = (sumY - (slope.X * sumX)) / n # y-intercept of abline
  
  #Regression Equation(y) = a + bx
  estimated.Y <- intercept.Y + (slope.X * x) # This is model$fitted
  resid.Y <- y - estimated.Y # This is model$resid
  my.rmse <- sqrt(sum((y - estimated.Y)^2) / n) # This is rmse(y, model$fitted)
  
  sum((estimated.Y - MyArithmeticMean(y))^2) / sum((y - MyArithmeticMean(y))^2) 
}

MyResidualStandardError <- function(model, y, parameters) {
  # Note: if b0 + b1X1, we have 2 parameters (N - 2 = degrees of freedom)
  sqrt((1 / (length(y) - parameters) * sum(model$residuals^2)))
}

MyRegressionSlope <- function(x, y) {
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.X <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  
  return(slope.X)
}

# Another way, using the weights formula: w = (Xt * X)-1 * Xt * y
MyRegressionSlope2 <- function(x, y) {
  x.ones <- rep(1, length(x))
  x <- cbind(x.ones, x)
  slope.X <- solve(t(x)%*%x)%*%t(x)%*%y
  
  # Note: slope.X is a matrix, so return 2nd element:
  return(slope.X[2])
}

MyRegressionIntercept <- function(x, y) {
  intercept.Y = (sumY - (MyRegressionSlope(x) * sum(x)))

  return(intercept.Y)
}

MyRMSE <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Root-mean-square_deviation (RMSE)
  # Root Mean Square Error
  
  return(sqrt(MyMSE(actual, predicted)))
}

MyMSE <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Mean_squared_error
  # Mean Square Error
  
  # One way: sqrt(mean((actual-predicted)^2, na.rm=T))
  return(sum((actual - predicted)^2) * (1 / length(actual)))
}

MyMPE <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Mean_percent_error
  # Mean Percent Error
  return(sum((actual - predicted) / actual) * (1 / length(actual)))
}

MyMAPE <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Mean_absolute_percentage_error
  # Mean Absolute Percentage Error
  return(sum(abs((actual - predicted) / actual)) * (1 / length(actual)))
}

MyME <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Mean_error
  # Mean Error/Deviation
  return(sum(actual - predicted) * (1 / length(actual)))
}

MyMAE <- function(actual, predicted) {
  # http://en.wikipedia.org/wiki/Mean_absolute_error
  # Mean Absolute Error/Deviation
  return(sum(abs(actual - predicted)) * (1 / length(actual)))
}

MyRMSLE <- function(actual, predicted) {
  # Root mean squared log error (Test: rmsle(actual, predicted) {Metrics})
  # See also: SLE, MSLE
  return(sqrt((1 / length(actual)) * sum((log(actual + 1) - log(predicted + 1))^2)))
}

MyLogLoss <- function(probs, y) {
  # TEST:
  # probs <- runif(100)
  # y <- sample(c(0, 1), 100, replace=T)
  # MyLogLoss(probs, y)
  
  epsilon <- 10e-12
  for (counter in 1:length(probs)) {
    if (probs[counter] == 1)
      probs[counter] <- probs[counter] - epsilon
    if (probs[counter] == 0)
      probs[counter] <- probs[counter] + epsilon
  }
  # log(0) is undefined, so when p is 0 we need to add a small value (epsilon) to it, likewise subtract if p == 1
  logloss <- ifelse(y == 1, -log(probs), -log(1 - probs))
}

# NOTE: Not correct for multiple regression!
MyUnstandardizedRegressionCoefficient <- function(x, y) {
  # NOTE: For single variable regression. This is slope.X (B1)
  return(MyCorrelationCoefficient(x, y) * (MyStddev(y, sample.based=F) / MyStddev(x, sample.based=F)))
}

# NOTE: Not correct for multiple regression!
MyStandardizedRegressionCoefficient <- function(x, y) {
  # NOTE: For single variable regression.
  # This is slope.X (B) with SD=1 (= standardized (Z-score) x and y values)
  sd.X <- 1
  sd.Y <- 1
  
  return(MyCorrelationCoefficient(x, y) * (sd.X / sd.Y))
}

MyAdjustedRegressionCoefficient <- function(r2, n, k) {
  # r2 = R squared
  # n = number of rows in dataset (number of observations)
  # k = number of independent variables (x'es)
  
  return(1 - (1 - r2) * (n - 1) / (n - k - 1))
}

# NOTE: The result here is covariance for one or two VECTORS!
# TODO: Do cov for just one vector + cov for one or two matrices!
MyCovariance <- function(x, y=NULL, sample.based=T) {
  if (is.null(y))
    y <- x 
  # TODO: For matrix, need to do x.outer(t(y))
  if (sample.based == F)
    sum((x - MyArithmeticMean(x)) * (y - MyArithmeticMean(y))) / length(x)
  else
    sum((x - MyArithmeticMean(x)) * (y - MyArithmeticMean(y))) / (length(x) - 1)
}

MyCorrelationCoefficient <- function(x, y) {
  if (!identical(length(x), length(y)))
    stop("incompatible dimensions")
  
  # r = Pearson Correlation Coefficient (Pearson C.C. = Centered Cosine Similarity)
  r <- MyCovariance(x, y, sample.based=F) /
    (sqrt(MyVariance(x, sample.based=F)) * sqrt(MyVariance(y, sample.based=F)))
  
  # Rule of thumb to determine if there is a relationship or not (TODO: Not so sure about this one...):
  if (abs(r) >= (2 / sqrt(length(x))))
    cat("A relationship exists!\n")
  else
    cat("There is no relationship!\n")
  
  return(r)
}

MyCorrelationCoefficient2 <- function(x, y) {
  if (!identical(length(x), length(y)))
    stop("incompatible dimensions")
  
  # r = Pearson correlation coefficient
  # Raw score formula:
  r <- MySumOfCrossProducts(x, y) /
    sqrt(MyVariance(x, sample.based=F) * MyVariance(y, sample.based=F)) / length(x)
  # TODO: Z-Score formula (see StatisticsOne, Week 3, 6-2 Calculation of r):
  #(sum(MyZScore(x) * (MyZScore(y)))) / (sqrt(MyVariance(x, sample.based=F)) *
  #  sqrt(MyVariance(y, sample.based=F)))
  
  # Rule of thumb to determine if there is a relationship or not:
  if (abs(r) >= (2 / sqrt(length(x))))
    cat("A relationship exists!\n")
  else
    cat("There is no relationship!\n")

  return(r)
}

MyBinomialCoefficient <- function(elements, set) {
  # The possible combinations of <elements> elements in a set of <set> elements. Used in binomial dist.
  # Usage: MyBinomialCoefficient(2, 5)
  
  if (elements > set)
    stop("Elements must be smaller or equal to set")
  
  factorial(set) / (factorial(elements) * factorial(set - elements))
}

MyLinearRegressionFormulas <- function(x, y) {
  # Good explanations and formulas:
  # http://reliawiki.org/index.php/Simple_Linear_Regression_Analysis
  
  # Regression Equation(y) = a + bx 
  # Slope(b) = (NsumXY - (sumX)(sumY)) / (NsumX^2 - (sumX)^2)
  # Intercept(a) = (sumY - b(sumX)) / N
  # From: http://easycalculation.com/statistics/learn-regression.php
  
  # Another way, by using correlation coefficient:
  # Regression coefficient = correlation coefficient * (standard deviation of Y / standard deviation of X)
  # B = r * (sdy / sdx)
  
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.B <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  intercept.A = (sumY - (slope.B * sumX)) / n # y-intercept of abline
  cat("Slope:", slope.B, "\n")
  cat("Intercept:", intercept.A, "\n")
  
  # Scatterplot it to test
  par(mfrow=c(1,1))
  #plot(x, y, main="Terje's Regression Test!")
  
  # Create abline/regression line (Regression Equation(y) = a + bx)
  my.regression.line <- numeric()
  
  for (i in 1:length(x))
    my.regression.line <- append(my.regression.line, intercept.A + (slope.B * x[i]))
  
  cat("\nRegression line:", my.regression.line, "\n")
  
  #lines(x, my.regression.line, col="blue")
  #grid()
  
  data <- data.frame(x, y)
  the.regression.line <- data.frame(x=x, y=my.regression.line)
  
  # NOTE: Find resid by subtracting the fitted line values from y values
  my.resid <- (y - my.regression.line)
  cat("\nResiduals:", my.resid, "\n")
  
  ggplot(data=data, aes(x=x, y=y, color=y)) + geom_point(size=3, alpha=.8) +
    geom_line(data=the.regression.line, aes(x=x, y=y), color="red") +
    ggtitle("Regression Test") +
    theme(plot.title=element_text(size=18, color="steelblue4"))
  
  #return(c(my.regression.line, my.resid))
}

MyLinearRegressionLine <- function(x, y) {
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.B <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  intercept.A = (sumY - (slope.B * sumX)) / n # y-intercept of abline
  
  # Create abline/regression line (Regression Equation(y) = a + bx)
  my.regression.line <- numeric() # These are the fitted values
  
  for (i in 1:length(x))
    my.regression.line <- append(my.regression.line, intercept.A + (slope.B * x[i]))
  
  names(my.regression.line) <- c(1:length(my.regression.line))
  
  return(my.regression.line)
}

MyLinearRegressionSlope <- function(x, y) {
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.B <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  # Or:
  slope.B <- sum((y-mean(y))*(x-mean(x))) / sum((x-mean(x))^2)
  
  intercept.A = (sumY - (slope.B * sumX)) / n # y-intercept of abline
  return(slope.B)
}

MyLinearRegressionIntercept <- function(x, y) {
  sumX <- sum(x)
  sumY <- sum(y)
  sumXY <- sum(x * y)
  sumX2 <- sum(x^2)
  n <- length(x)
  
  slope.B <- (n * sumXY - (sumX * sumY)) / (n * sumX2 - (sumX)^2) # Slope of abline
  # Or:
  slope.B <- sum((y-mean(y))*(x-mean(x))) / sum((x-mean(x))^2)
  
  intercept.A = (sumY - (slope.B * sumX)) / n # y-intercept of abline
  return(intercept.A)
}

MyLinearRegressionResiduals <- function(x, y) {
  my.regression.line <- MyLinearRegressionLine(x, y)
  
  return(y - my.regression.line)
}

# NOTE: This is the same as Standard Error (SE = SD / sqrt(N))
MyStddevOfSamplingDistribution <- function(stddev.of.population, sample.size) {
  # NOTE: This formula is used when sd of population is KNOWN (that is usually not the case)
  #       As sample size increases (denominator), sd.of.sampling.dist
  #       (= standard error of the mean) gets smaller
  
  return (stddev.of.population / sqrt(sample.size))
  # TODO: calcaulate for UNKNOWN population sd!
}

MyConfidenceInterval <- function() {
  # TODO: Try to get the formula for this! See here:
  # https://class.coursera.org/stats1-002/forum/thread?thread_id=2117&sort=newest
  
  data <- read.csv("C:/coding/R/TestData/ImpactStudies.txt", sep="\t", header=T)
  head(data, n=2)
  ds <- data[sample(1:nrow(data), 20, replace=FALSE),]
  head(ds, n=2)
  model1 <- lm(ds$total_symptom_retest ~ ds$verbal_memory_retest +
                 ds$visual_memory_retest + ds$visual.motor_speed_retest + ds$visual.motor_speed_retest +
                 ds$reaction_time_retest + ds$impulse_control_retest)
  model1 <- lm(ds$visual_memory_retest ~ ds$verbal_memory_retest)
  confint(model1, level=0.95)
  
  # TODO: In formula for confidence interval. HOW TO FIND t ??
  
  # Upper bound = M (mean of the sample) + t(SE)
  # Lower bound = M (mean of the sample) - t(SE)
  # (t depends on level of confidence desired and size of distribution)
  # SE = SD / sqrt(N)
  # where SD is the mean of the sample (since we don't know the population SD)
  # and N is the size of the sample
  
  # CI for regression coefficients:
  # Smooth gives the range (slopes of lower and upper bound) of possible regression lines
  p1 <- ggplot(data, aes(x=verbal_memory_retest, y=visual_memory_retest)) + geom_point() +
    geom_smooth(method="lm", colour="blue", fill="steelblue2", alpha=.3) +
    ggtitle("Population") +
    theme(plot.title=element_text(size=14, color="steelblue4"))
  
  p2 <- ggplot(ds, aes(x=verbal_memory_retest, y=visual_memory_retest)) + geom_point() +
    geom_smooth(method="lm", colour="blue", fill="steelblue2", alpha=.3) +
    ggtitle("Sample of 20") +
    theme(plot.title=element_text(size=14, color="steelblue4"))
  
  grid.arrange(p1, p2, nrow=1, ncol=2)
}

MyTValue <- function(y.intercept, std.error) {
  return(y.intercept / std.error)
}

MyChiSquare <- function(expected.values, observed.values) {
  sum((observed.values - expected.values)^2 / expected.values)
  # p-value: Find that in chi-square distribution table for result here + df
}

MyCramersPhi <- function(expected.values, observed.values, categorical.values) {
  # This is like a cor coeff for chi-square calc
  k <- categorical.values
  N <- sum(expected.values)
  
  sqrt(MyChiSquare(expected.values, observed.values) / (N * (k - 1)))
}

MyChiSquareTestOfIndependence <- function() {
  # TEST, chi-square test of independence:
  # df here is: (# of rows - 1) * (# of columns - 1)
  vote <- data.frame(Sex=c("Female", "Male"), Quinn=c(40, 90), Lhota=c(10, 40), Other=c(10, 10))
  vote.table <- table(vote$Sex)
  # E = R/N * C. Find for Female voting Quinn. Do this for all cells
  E <- (sum(vote[1, 2:4]) / sum(vote[, 2:4])) * sum(vote[, 2]) 
}

# A.k.a. "log odds". http://en.wikipedia.org/wiki/Logit
MyLogit <- function(probability) {
  log(probability / (1 - probability)) # log = natural logarithm (ln)
  
  # Example:
  # plot(MyLogit(seq(0,1,.001)), col="blue")
}

MyInverseLogit <- function(logit) {
  1 / (1 + exp(-logit)) # Inverse logit transform
}

MyOdds <- function(probability) {
  exp(MyLogit(probability))  
}

# = MySigmoid?
MyProbability <- function(intercept, coeffs, predictors) {
  e <- exp(1)
  1 / (1 + e^((intercept + (coeffs * predictors)) * -1))

  # Example:
  # y <- sample(0:1, 100, replace=T)
  # x <- y + rnorm(100)
  # model <- glm(y ~ x)
  # probs <- MyProbability(model$coefficients[1], model$coefficients[2], x)
  # plot(MyLogit(probs), pch=19, col="gray", main="Logit of probabilities")
  # plot(MyOdds(probs), pch=19, col="gray", main="Odds of probabilities")
  # points(model$fitted.values, col="red")
  # plot(sort(model$fitted.values))
}

MyWilcoxonSignedRankMethod <- function(pair.a, pair.b) {
  # wilcox.test(a, b, paired=TRUE, correct=TRUE)
  df <- data.frame(pair.a, pair.b, sign(pair.a - pair.b), abs(pair.a - pair.b),
                   rank(abs(pair.a - pair.b)), sign(pair.a - pair.b) * rank(abs(pair.a - pair.b)))
  names(df) <- c("Pair.A", "Pair.B", "Sign.S", "Abs", "Rank.R", "S.times.R")
  df
  
  # NOTE: S.times.R col adds up to zero under the null hyphotesis!
}

MyPredictions <- function(x, y, data) {
  # http://stats.stackexchange.com/questions/25389/
  #   obtaining-predicted-values-y-1-or-0-from-a-logistic-regression-model-fit
  model1 <- lm(y ~ x, data=data, family=binomial)
  p <- predict(lm, data, type="response")
  #(1) Sensitivity: P(Y^i=1|Yi=1) - the proportion of '1's that are correctly identified as so.

  #(2) Specificity: P(Y^i=0|Yi=0) - the proportion of '0's that are correctly identified as so
  
  #(3) (Correct) Classification Rate: P(Yi=Y^i) - the proportion of predictions that were correct.
}

# TODO....
MyNormalDistribution <- function(probs) {
  exp(1)^(-((probs-MyArithmeticMean(probs))^2) / ((2 * MyStddev(probs))^2)) / (MyStddev(probs)*sqrt(2*pi))
}

# TODO: Other norms
MyDNorm <- function(x) {
  (1 / sqrt(pi * 2)) * exp(-x^2 / 2) # Same result as dnorm(x)
  # Try: plot(MyDNorm(seq(-pi, pi, 0.1)), pch=21, bg="powderblue", col="blue")
}

# See Kuhn, p.34
MySquaredNorm <- function(predictor) {
  # NOTE: Predictor should be centered and scaled first!
  return(predictor / sum(predictor^2))
}

#####################################################################################################################
# General stuff:

# Using RSiteSearch and apropos in {utils}
SearchForMethod <- function(method) {
  apropos(method)
  RSiteSearch(metod, restrict="functions") # restrict = c("functions", "vignettes", "views"), 
}

IsSkewed <- function(data) {
  # Sample skewness statistic? Kuhn page 31
  # Create a skewed distribution: x <- dbeta(seq(0,1,.001), 18,4,20)
  v <- sum((data - mean(data))^2) / (length(data) - 1)
  ratio <- sum((data - mean(data))^3) / ((length(data) - 1) * v^(3/2))
  return (ratio)
}

# Normalize values between min and max
Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  #if (length(curval) <= 1)
  #  return(0)
  
  # Find the normalized (0-1 range) value of curval
  normalized <- (curval - minval) / (maxval - minval)
  normalized
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

# Normalize any values between 0 and 1
Normalize2 <- function(x) {
  if (length(x) <= 1)
    return(0)
  
  numerator <- x - min(x)
  denominator <- max(x) - min(x)
  return (numerator / denominator)
}

LarchProverNormalize <- function(data, p) {
  return ((sum((abs(data))^p))^(1/p))  
}

# Normalize between 0 and 1
ScaleZeroToOne <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

NormalizeWithZeroMean <- function(x) {
  return (x - mean(x)) / (max(x) - min(x))
}

BagIntersect <- function(x, y) {
  xy <- sort(unique(c(x, y))) # TODO: Sort needed?
  intersect.array <- integer(0) # TODO: Handle all datatypes
  #intersect.array <- class(x)
  for (counter in 1:length(xy)) {
    n.x <- length(which(xy[counter] == x))
    n.y <- length(which(xy[counter] == y))
    if (n.x > 0 & n.y > 0)
      intersect.array <- c(intersect.array, rep(xy[counter], min(n.x, n.y)))
  }
  return (intersect.array)
}

GetJaccardSetSimilarity <- function(document1, document2, apply.shingling=T, k) {
  document1 <- tolower(document1)
  document2 <- tolower(document2)
  
  if (apply.shingling == T) {
    k.shingles.doc1 <- unique(sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1))))
    k.shingles.doc2 <- unique(sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1))))
    jaccard.sim <- length(intersect(k.shingles.doc1, k.shingles.doc2)) / length(union(k.shingles.doc1, k.shingles.doc2))
  } else {
    jaccard.sim <- length(intersect(document1, document2)) / length(union(document1, document2))
  }
  
  return (jaccard.sim)
}

# http://www.dbinfoblog.com/post/142/union,-intersection,-and-difference-of-bags-/
GetJaccardBagSimilarity <- function(document1, document2, apply.shingling=T, k) {
  document1 <- tolower(document1)
  document2 <- tolower(document2)
  
  # NOTE: Need to do a different intersect/union here:
  # http://poincare.matf.bg.ac.rs/~vladaf/Courses/PmfBl%20TI%20IP/Materijali/Kent%20State/lecture2.ppt
  if (apply.shingling == T) {
    k.shingles.doc1 <- (sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1))))
    k.shingles.doc2 <- (sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1))))
    jaccard.sim <- length(BagIntersect(k.shingles.doc1, k.shingles.doc2)) / length(c(k.shingles.doc1, k.shingles.doc2))
  } else {
    jaccard.sim <- length(BagIntersect(document1, document2)) / length(c(document1, document2))
  }
  
  return (round(jaccard.sim, 2))
}

RemoveStopwords <- function(text, returnAsVector=F) {
  if (length(text) == 0)
    return (character(0))
  
  final_text <- character(0)
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  # result <- gregexpr("findwhat", "Find findwhat here")
  # match.pos <- result[[1]]
  # match.length <- attr(result[[1]], "match.length")
  
  if (length(word.vec) == 0)
    return (character(0))
  
  if (! exists("stopwords")) {
    stopwords <- read.csv("c:/coding/python/stopwords.txt", header=F)
    stopwords <- as.character(unlist(stopwords))
  }
  
  for (counter in 1:length(word.vec))
    if (! word.vec[counter] %in% stopwords)
      final_text <- c(final_text, word.vec[counter])
  
  if (returnAsVector == F)
    return (gsub(",", "", toString(final_text)))
  else
    return (final_text)
}

# Enhanced from: https://gist.github.com/corynissen/8935664
# NOTE: This is word-level bigram. See MyNGrams for handling both. NOTE: Now returns "word1.word2" format
# Usage:
# bigrams <- MyBigrams(text)
MyBigrams <- function(text) {
  # word.vec <- strsplit(text, "\\s+")[[1]]
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  word.vec.length <- length(word.vec)
  bigrams <- lapply(1:(word.vec.length - 1), function(x) c(word.vec[x], word.vec[x + 1]))
  #return (bigrams)
  return (lapply(bigrams, function(x) paste(x[1], x[2])))
  #return (unlist(lapply(bigrams, function(x) paste0(x[1], ".", x[2]))))
}

# NOTE: This is word-level trigrams. See MyNgrams for handling both
MyTrigrams <- function(text) {
  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  word.vec.length <- length(word.vec)
  trigrams <- lapply(1:(word.vec.length - 2), function(x) c(word.vec[x], word.vec[x + 1], word.vec[x + 2]))
  return (trigrams)
  #return (lapply(trigrams, function(x) paste(x[1], x[2], x[3])))
  #return (unlist(lapply(trigrams, function(x) paste(x[1], x[2], x[3]))))
}

# Generic n-gram method. Handles n-grams and shingles
MyNgrams <- function(text, ngram.length=2, word.level.ngram=T, return.set=F) {
  if (nchar(text) == 0)
    return("")
  
  # Handle n-grams for character strings with no spaces
  # Create shingles instead (or: not word-level ngrams)
  if (length(unlist(strsplit(text, " "))) == 1 | word.level.ngram == F) {
    k <- ngram.length
    k.shingles <- sapply(1:(nchar(text) - (k - 1)), function(x) substr(text, x, x + (k - 1)))
    
    if (return.set == T)
      return (unique(k.shingles))
    else
      return (k.shingles)
  }

  word.vec <- regmatches(text, gregexpr("\\b\\w+'?\\w?\\b", text))[[1]]
  word.vec.length <- length(word.vec)
  
  if (ngram.length <= 0 | !(class(ngram.length) %in% c("integer", "numeric")))
    return (character(0))
  
  ngram.length <- round(ngram.length, 0)
  
  if (ngram.length >= word.vec.length)
    return (word.vec)
  
  ngrams <- lapply(1:(word.vec.length - (ngram.length - 1)),
                   function(x) sapply(word.vec[x:(x + (ngram.length - 1))], c, USE.NAMES=F))
  if (return.set == T)
    return (unique(ngrams))
  else
    return (ngrams)
}

# Convert a string with spaces or a string vector into integer vectors
# Can use this with CosineSimilarity functions below
ConvertStringsToIntegerVectors1 <- function(x, y, to.lowercase=T) {
  if (to.lowercase == T) {
    x <- tolower(x)
    y <- tolower(y)
  }
  
  match <- gregexpr(" ", x)[[1]][1]
  
  if (length(x) == 1 & length(y) == 1 & match != -1) {
    x <- unlist(strsplit(x, " "))
    y <- unlist(strsplit(y, " "))
  }
  
  xy <- union(x, y)
  x.vec <- integer(0)
  y.vec <- integer(0)
  
  for (counter in 1:length(xy)) {
    if (xy[counter] %in% x)
      x.vec <- c(x.vec, which(x %in% xy[counter]))
    else
      x.vec <- c(x.vec, 0)
  }
  
  for (counter in 1:length(xy)) {
    if (xy[counter] %in% y)
      y.vec <- c(y.vec, which(y %in% xy[counter]))
    else
      y.vec <- c(y.vec, 0)
  }
  
  return (list(x=x.vec, y=y.vec))
}

ConvertStringsToIntegerVectors2 <- function(x, y) {
  match <- gregexpr(" ", x)[[1]][1]
  
  if (length(x) == 1 & length(y) == 1 & match != -1) {
    x <- unlist(strsplit(x, " "))
    y <- unlist(strsplit(y, " "))
  }
  
  xy <- unique(c(x, y))
  x.vec <- integer(0)
  y.vec <- integer(0)
  
  for (counter in 1:length(y)) {
    x.vec <- c(x.vec, which(xy %in% x[counter]))
  }
  
  for (counter in 1:length(y)) {
    y.vec <- c(y.vec, which(xy %in% y[counter]))
  }
  
  return (list(x=x.vec, y=y.vec))
}


# http://reference.wolfram.com/language/ref/CosineDistance.html
# http://en.wikipedia.org/wiki/Cosine_similarity
# http://www.appliedsoftwaredesign.com/archives/cosine-similarity-calculator
# http://cs.joensuu.fi/~zhao/Link/Similarity_strings.html
CosineSimilarity <- function(x, y) {
  if (class(x) == "character") {
    # Fix string vectors
    result <- ConvertStringsToIntegerVectors2(x, y) # Implementation 1 or 2 best?
    x <- result$x
    y <- result$y
  }

  # Assumes that vectors have equal length. If not, pads the shortest with zeroes
  if (length(x) != length(y))
    if (length(x) < length(y))
      x <- c(x, rep(0, length(y) - length(x)))
  else
    y <- c(y, rep(0, length(x) - length(y)))

  numerator <- 0
  for (counter in 1:length(x))
    numerator = numerator + (x[counter] * y[counter])
  denominator1 <- 0
  denominator2 <- 0
  for (counter in 1:length(x))
    denominator1 = denominator1 + abs(x[counter]^2)
  for (counter in 1:length(y))
    denominator2 = denominator2 + abs(y[counter]^2)
  
  numerator / (sqrt(denominator1) * sqrt(denominator2))
  # Same as: x %*% y / sqrt(x%*%x * y%*%y)
  
  # cos( d1, d2 ) = dot(d1, d2) / ||d1|| ||d2||
}

CosineSimilarity2 <- function(x, y) {
  if (class(x) == "character") {
    # Fix string vectors
    result <- ConvertStringsToIntegerVectors1(x, y)
    x <- result$x
    y <- result$y
  }
  
  # Assumes that vectors have equal length. If not, pads the shortest with zeroes
  if (length(x) != length(y))
    if (length(x) < length(y))
      x <- c(x, rep(0, length(y) - length(x)))
    else
      y <- c(y, rep(0, length(x) - length(y)))
  
  return (as.numeric((x %*% y) / sqrt((x %*% x) * (y %*% y))))
}

# Find principal eigenvalue and eigenvector (only good for quadratic matrices?) 
DoPowerIteration <- function(M) {
  if (ncol(M) != nrow(M)) # Not quadratic matrix
    M <- M %*% t(M)
  
  x1 <- rep(1, ncol(M)) # Init
  result <- M %*% x1
  counter <- 0
  old.x1 <- 0
  
  while (T) {
    Frobenius.norm <- sqrt(sum(result^2)) # NOTE: sqrt(5^2 + 8^2)
    result <- (result / Frobenius.norm)
    # result
    old.x1 <- x1
    x1 <- result
    result <- M %*% x1
    # result
    
    if ((sum(x1) == sum(old.x1)) | (counter == 1000))
      break
    
    counter <- counter + 1
  }
  
  # x1 # OK!
  # Then compute the principal eigenvalue:
  principal.eigenvalue <- t(x1) %*% M %*% x1
  # principal.eigenvalue # OK!
  ret <- list()
  ret$eigenvector <- x1
  ret$principal.eigenvalue <- principal.eigenvalue
  return (ret)
}

# Norm, used in L<n> regularization
# http://cseweb.ucsd.edu/~saul/teaching/cse291s07/L1norm.pdf
# http://www.quora.com/Machine-Learning/What-is-the-difference-between-L1-and-L2-regularization
# http://cs.stanford.edu/people/ang/papers/aaai06-efficientL1logisticregression.pdf
# http://en.wikipedia.org/wiki/Regularization_(mathematics)
MyNormFunction <- function(data, norm=2) {
  (sum(data^norm))^(1/norm) # Remember, vector can not be zero!
  # x <- rnorm(50)
  # norm <- MyNormFunction(x, 2)
  # norm.result <- x + norm
  # plot(x, ylim=c(min(x) - 1, max(norm.result) + 1), cex.axis=.8, col="green4", pch=17, main="Norm", cex.lab=.8)
  # points(norm.result, col="red", pch=17)
}
  
# Create Kaggle submission files
KaggleSubmission <- function(submissionData, submissionFolder, submissionType, competitionName="Kaggle",
                             rowNames=F, quote=F) {
  write.csv(submissionData, paste0(submissionFolder, "/", competitionName, "_", submissionType, "_",
                                   format(Sys.time(), "%Y%m%d_%H%M"), ".csv"), row.names=rowNames, quote=quote)
}

# Create nice confusion matrix
ConfusionMatrix <- function(data, labels=NA, xlab=NA, ylab=NA, title="Confusion Matrix") {
  z <- matrix(data, ncol=length(labels), byrow=F, dimnames=list(labels, labels))
  confusion <- as.data.frame(t(as.table(z)))
  
  # TEST: Turn table around on y-axis
  # confusion <- as.data.frame(t(as.table(z[nrow(z):1,])))
  # confusion <- as.data.frame(data[nrow(data):1,])
  
  require(ggplot2)
  
  plot <- ggplot()
  plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq), data=confusion, color="black", size=0.1) + 
    labs(x=ifelse(is.na(xlab), "Observed", xlab), y=ifelse(is.na(ylab), "Predicted", ylab)) + ggtitle(title) +
    theme(plot.title=element_text(size=18, color="black")) +
    geom_text(aes(x=Var1, y=Var2, label=sprintf("%.1d", Freq)), data=confusion, size=4, colour="black") +
    scale_fill_gradient(low="white", high="green", name="Counts") +
    theme(axis.text=element_text(colour="black", size=11), 
          axis.title=element_text(face="bold", colour="black", size=14),
          axis.text.x=element_text(angle = 90, hjust = 1),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12))
}

# Create another nice confusion matrix
ConfusionMatrix2 <- function(y, predicted, labels) {
  result <- table(y, predicted) # Diagonal gives the prediction
  
  # Get the prediction score this way from confusion matrix:
  score <- (result[1] + result[4]) / sum(result)
  # Or get the prediction score this way:
  score <- (table(predicted == y)[2]) / length(y)
  # Or this way:
  score <- sum(diag(y)) / length(y)
  score
  
  # Nice confusion matrix
  confusion.matrix <- as.matrix(Normalize(max(result), min(result), 1, 0, result))
  image(x=seq(2), y=seq(2), z=(confusion.matrix), ylab="Predicted", xlab="y", cex.main=1,
        main=paste0("Confusion matrix (Score = ", round(score, 4), ")"), xaxt="n", yaxt="n",
        col=heat.colors(255))
  axis(side=1, at=seq(2), labels=c(labels[1], labels[2]), las=1, cex.axis=.8)
  axis(side=2, at=seq(2), labels=c(labels[2], labels[1]), las=2, cex.axis=.8)
  text(expand.grid(x=seq(2), y=seq(2)), labels=t(result[c(2,4,1,3)]), cex=1.5)
  box() # Draw a box around the image  
}

CorrelationPlot <- function(data.in, threshold=.5) {
  # Fix the data format (change factor cols to numeric). TODO: Fix/delete char cols??
  data <- data.in
  cols <- ncol(data)
  remove.cols <- integer(0)
  
  zero.var.cols <- which(sapply(data, var) == 0) # Remove cols with zero var
  if (length(zero.var.cols > 0))
    data <- data[, -zero.var.cols] # Remove cols
  
  for (counter in 1:length(cols)) {
    col.class <- sapply(data, class)[counter]
    if (!(col.class %in% c("numeric", "integer")))
      remove.cols <- c(remove.cols, counter) # Remove the col as it is not numeric/integer
  }
  if (length(remove.cols) > 0)
    data <- data[, -remove.cols] # Remove cols
  
  cols <- as.integer(which(sapply(data, class) == "factor"))
  if (length(cols) > 0) {
    for (counter in 1:length(cols))
      data[, cols[counter]] <- as.numeric(data[, cols[counter]])
  }
  
  # TODO: Fix the long var names for display: Use <First part>...<Last part> if longer than <n> chars
  label.max <- 12
  the.labels <- names(data)
  if (max(nchar(the.labels)) < 7) {
    max.margin <- 3
  } else {
    max.margin <- 5.5
  }
  for (counter in 1:length(the.labels))
    if (nchar(the.labels[counter]) > label.max)
      the.labels[counter] <- paste0(substr(the.labels[counter], 1, 4), "...",
                                    substr(the.labels[counter], nchar(the.labels[counter]) - 4,
                                           nchar(the.labels[counter])))
  oldmar <- par()$mar
  par(mar=c(max.margin, max.margin, 2, .4))
  COR <- cor(data, use="complete.obs")
  size <- dim(data)[2]
  
  fontsize <- 8/size
  if (fontsize < .5) fontsize <- .5
  
  # Red color for highest cor, white for lowest: Do z=1-abs(COR) below, otherwise z=abs(COR)
  image(x=seq(size), y=seq(size), z=1-abs(COR), xlab="", ylab="",
        cex.axis=.6, cex.lab=.7, cex.main=.8,
        main=paste0("Correlation Matrix for ", deparse(substitute(data.in))), xaxt="n", yaxt="n")
  axis(side=1, at=seq(1, size), labels=the.labels, las=2, cex.axis=.6)
  axis(side=2, at=seq(1, size), labels=the.labels, las=2, cex.axis=.6)
  text(expand.grid(x=seq(size), y=seq(size)), labels=round(c(COR), 3), cex=fontsize)
  box() # Draw a box around the image
  par(mar=oldmar)
  data <- NULL
  
  # Get and return the highest correlated cols, pos/neg (> limit or < limit):
  limit <- threshold
  highcor.row <- character(0)
  highcor.col <- character(0)
  cor.val <- numeric(0)
  cors <- COR
  
  for (rowcounter in 1:nrow(cors)) {
    for (colcounter in 1:ncol(cors)) {
      if (((cors[rowcounter, colcounter] > limit) | (cors[rowcounter, colcounter] < -limit)) &
            (cors[rowcounter, colcounter] < 1.0)) {
        if (! colnames(cors)[colcounter] %in% highcor.row) {
          highcor.row <- c(highcor.row, rownames(cors)[rowcounter])
          highcor.col <- c(highcor.col, colnames(cors)[colcounter])
          cor.val <- c(cor.val, cors[rowcounter, colcounter])
        }
      }
    }
  }
  
  df <- data.frame(var1=highcor.row, var2=highcor.col, cor.val=cor.val)
  df <- df[order(abs(df$cor.val), decreasing=T),]
  df
  
  return (df)
  # See also: http://flowingdata.com/2010/01/21/how-to-make-a-heatmap-a-quick-and-easy-solution/
}

CorrelationPlot2 <- function(data) {
  # Note: All cols must be numeric!
  z <- cor(data)
  require(lattice)
  levelplot(z, contour=F, main="Correlation matrix", cex.main=.8, cex.lab=.7, cex.axis=.7)
}

# http://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
Heatmap <- function(table.input, title.in="Title", xlab.in="x", ylab.in="y", low.in="white", high.in="red",
                    legend.title="Value", rounding.in=1) {
  #table.input <- table.input[nrow(table.input):1,]
  
  if (class(table.input) %in% c("table","matrix")) { # TODO: handle matrix!
    result <- as.data.frame(table.input)
    ggplot(result, aes(x = Var1, y = Var2)) + geom_tile(aes(fill = abs(Freq))) +
      geom_text(aes(label=round(as.numeric(Freq), digits=2)), size=4) +
      xlab(xlab.in) + ylab(ylab.in) + 
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_gradient(name=legend.title, low=low.in, high=high.in) + ggtitle(title.in) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    result <- as.data.frame(melt(table.input))
    #result <- result[order(-result[2,])]
    names(result) <- c("X1","X2","value")
    ggplot(result, aes(x = X1, y = X2)) + geom_tile(aes(fill = abs(value))) +
      geom_text(aes(label=round(as.numeric(value), digits=2)), size=4) +
      xlab(xlab.in) + ylab(ylab.in) + 
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_gradient(name=legend.title, low=low.in, high=high.in) + ggtitle(title.in) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
}

VariancePlot <- function(data, y) {
  variance <- numeric(0)
  
  for (counter in 1:ncol(data)) {
    variance[counter] <- (var(data[counter]) / sum(var(data))) * 100
  }
  
  plot(variance, pch=19, col="blue", xaxt="n", xlab="", cex.axis=.8, cex.main=1,
       cex.lab=.8, main="Variance plot")
  axis(side=1, at=1:ncol(data), labels=names(data), las=2, cex.axis=.8)
}

CoVariancePlot <- function(data, y) {
  variance <- numeric(0)
  
  for (counter in 1:ncol(data)) {
    variance[counter] <- (cov(data[counter], y) / sum(var(data))) * 100
  }
  
  plot(variance, pch=19, col="blue", xaxt="n", xlab="", cex.axis=.7, cex.main=.8,
       cex.lab=.7, main="CoVariance plot")
  axis(side=1, at=1:ncol(data), labels=names(data), las=2, cex.axis=.7)
}

# TODO: Enable rotation angle, CV/CCV, mirroring, etc. as extra params?
RotateImage <- function(image.data) {
  rotated.image <- t(apply(image.data, 2, rev))
  #image(rotated.image, col=rainbow(5))
  return (rotated.image)
}

# Do cross-validation (K-fold and LOOCV) for a GLM model
DoCV <- function(y, x, K) {
  DoLOOCV <- F
  counter <- 1
  rows <- nrow(x)
  if (K == rows) {
    DoLOOCV <- T
    rmse <- rep(0.0, rows)
  } else {
    rmse <- rep(0.0, K)
  }
  
  if (DoLOOCV) {
    for (counter in 1:rows) {
      fit <- glm(y[-counter] ~ ., data=x[-counter, ],
                 family=binomial(link="logit"))
      pred <- predict(fit, newdata=x[counter, ], type="response")
      pred <- ifelse(pred < 0.5, 0, 1)
      
      if (class(y) == "factor")
        rmse[counter] <- MyRMSE(as.integer(pred), as.integer(y[counter])-1)
      
      if (class(y) %in% c("numeric","integer"))
        rmse[counter] <- MyRMSE(pred, y[counter])
    }
  } else {
    #fold.rows <- list()
    sample.rows <- 1:rows
    row.length <- rows / K
    
    for (counter in 1:K) {
      if (row.length > length(sample.rows)) row.length <- length(sample.rows)
      temp <- sample(sample.rows, row.length, replace=F)
      #fold.rows[[counter]] <- temp
      sample.rows <- sample.rows[-temp]
      fit <- glm(y[-temp] ~ ., data=x[-temp, ],
                 family=binomial(link="logit"))
      pred <- predict(fit, newdata=x[temp, ], type="response")
      pred <- ifelse(pred < 0.5, 0, 1)
      #print(paste0("K: ", counter))
      #print(table(as.integer(validation.sub$V2)-1, pred))
      flush.console()
      if (class(y) == "factor")
        rmse[counter] <- MyRMSE(as.integer(pred), as.integer(y[temp])-1)
      
      if (class(y) %in% c("numeric","integer"))
        rmse[counter] <- MyRMSE(pred, y[temp])
    }
  }
  
  if (DoLOOCV)
    return (mean(rmse))
  else {
    plot(rmse, type="o", col="blue", pch=21, bg="cyan",
         main=paste0("RMSE (mean: ", round(mean(rmse), 4), ")"), xlab="Folds",
         cex.lab=.8, cex.axis=.8, cex.main=1)
    return (rmse)
  }
}

# A forecasting function
Forecast <- function(data, start, end, frequency, forecast.period) {
  # t = time series
  t <- 1:length(data)
  
  quarters <- frequency
  years=(end - start)

  # TODO: Fix for frequency, from-to
  df <- data.frame(t=t, quarters=rep(c(1:quarters)))
  df
  #plot(df$sales, type="b", col="blue", pch=21, bg="cyan", main="Car Sales Time Series", xlab="Quarters", ylab="Sales")
  #axis(side=1, at=seq(1,16))
  
  # moving average
  ma <- rep(NA, (years * quarters))
  for (counter in 3:(length(data)-1)) {
    ma[counter] <- mean(data[(counter - 2):(counter + 1)])
  }
  ma
  df$ma <- ma
  lines(ma, col="red")
  
  # centered moving average
  cma <- rep(NA, (years * quarters))
  for (counter in 3:(length(data)-1)) {
    cma[counter] <- mean(ma[counter:(counter + 1)])
  }
  cma
  df$cma <- cma
  lines(cma, col="green")
  
  # Seasonal and Irregular component
  df$StIt <- (df$data / df$cma)
  
  # The mean of the same quarter in all the years 
  meanquarters <- numeric(4)
  meanquarters[1] <- mean(df$StIt[df$quarters == 1], na.rm=T)
  meanquarters[2] <- mean(df$StIt[df$quarters == 2], na.rm=T)
  meanquarters[3] <- mean(df$StIt[df$quarters == 3], na.rm=T)
  meanquarters[4] <- mean(df$StIt[df$quarters == 4], na.rm=T)
  
  # Seasonal component
  df$St <- rep(meanquarters, years)
  # De-seasonalized
  df$DS <- df$sales / df$St
  
  # Get the trend
  fit <- lm(df$DS ~ df$t)
  summary(fit)
  
  # Trend
  df$Tt <- fit$coeff[1] + fit$coeff[2] * df$t
  # Forecast
  df$forecast <- df$St * df$Tt
  df
  lines(df$ma, col="red")
  lines(df$cma, col="green")
  lines(df$forecast, col="orange", lwd=2)
  
  # So, now we have all the base data we need.
  # Make a <forecast.period> ahead forecast:
  # Create new rows for the forecast. t, quarter, St, Tt and Forecast must have new values for the forecast period
  years.new <- forecast.period
  t.new <- 1:(forecast.period * frequency)
  quarters.new <- rep(1:quarters, years.new)
  sales.new <- rep(NA, quarters * years.new)
  ma.new <- rep(NA, quarters * years.new)
  cma.new <- rep(NA, quarters * years.new)
  StIt.new <- rep(NA, quarters * years.new)
  St.new <- rep(meanquarters, years.new)
  DS.new <- rep(NA, quarters * years.new)
  Tt.new <- fit$coeff[1] + fit$coeff[2] * t.new
  forecast.new <- St.new * Tt.new
  df.new <- data.frame(t=t.new, quarters=quarters.new, sales=sales.new, ma=ma.new, cma=cma.new,
                       StIt=StIt.new, St=St.new, DS=DS.new, Tt=Tt.new, forecast=forecast.new)
  df.new 
  df <- rbind(df, df.new)
  
  plot(df$data, type="b", col="blue", pch=21, bg="cyan", main="Seasonal Time Series", cex.axis=.6,
       xlab="Quarters", ylab="Data", xaxt="n", ylim=(c(min(df$forecast)-1, max(df$forecast))))
  axis(side=1, at=seq(1,length(df$t), by=1), cex.axis=.6, labels=rep(1:quarters, years + years.new))
  lines(df$ma, col="red")
  lines(df$cma, col="green")
  lines(df$forecast, col="orange", lwd=1)
  
  ggplot(data=df, aes(x=t, y=data)) + geom_line(col="blue") + geom_point(col="blue", size=3, , na.rm=T) + 
    geom_line(aes(x=t, y=cma), col="green4", sixe=.7, linetype=2) +
    geom_line(aes(x=t, y=ma), col="red", size=.7, linetype=2) +
    geom_line(aes(x=t, y=forecast), col="orange", size=1) + ggtitle("Seasonal Time Series") +
    ylab("Sales") +
    scale_x_discrete("Quarters", labels=rep(1:quarters, years + years.new)) +
    theme_bw()
  
  # Compare:
  plot(forecast(ts(data, start=2007, frequency=4)))
  plot(forecast(HoltWinters(ts(data, start=2007, frequency=4))))
}

# Binarize (turn color or grayscale image into BW image):
Binarize <- function(data, threshold=0.5) {
  return (ifelse(data < threshold, 0, 1))
}

MyEuclideanDistance <- function(p1, p2) {
  # NOTE: This is the same as the Pythagorean thoerem: c^2 = a^2 + b^2. So distance c = sqrt(c^2)
  # return (sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2))
  return (sqrt(sum((p1 - p2)^2))) # NOTE: Changed to handle more than two dimensions
}

MyManhattanDistance <- function(p1, p2) {
  #return (abs(p1[1] - p2[1]) + abs(p1[2] - p2[2]))
  return (sum(abs(p1 - p2))) # NOTE: Changed to handle more than two dimensions
}

# Sigmoid function
# TEST: plot(MySigmoid(seq(-pi*2,pi*2,by=.1)), col="blue", type="h")
MySigmoid <- function(data) {
  return (1 / (1 + exp(-data)))
  # This can also be written as: f(x) = e(x) / (e(x) + 1)
  # This can also be written as: f(x) = 1 / (1 + e^-x)
  # Maps ANY number to range 0,1. Interpret the result as a probability.
  # plot(exp(data) / (exp(data) + 1), col="gray", pch=16)
}

# Majority vote for a variable number of vectors
MyMajorityVote <- function(..., return.binary=1) {
  dataset.list <- list(...)
  n = length(dataset.list)
  
  if (n == 0) {
    #warning("No data.")
    return(NULL)
  }

  if (n == 1)
    return (dataset.list[[1]])
  
  result <- numeric(length(dataset.list[[1]]))
  
  for (counter in 1:n)
    result <- result + dataset.list[[counter]]
  
  result <- result / length(dataset.list) 
  
  if (return.binary == 1) {
    if (n == 2) return (result) else return (round(result))
  }
  else
    return (result)
}

# A Perl tip: To create a 1% sample of the training set:
# perl -ne 'print if (rand() < 0.01)' train.csv > trainSample.csv
CreateTrainAndValidationSets <- function(data, percent.train=75, random.selection=T) {
  if (percent.train > 99) {
    warning("Sum percent.train must be 99 or less.")
    return ()
  }
  
  rows.train <- nrow(data) * (abs(percent.train / 100))
  if (random.selection == T) {
    trainRows <- sample(x=1:nrow(data), size=rows.train, replace=F)
  } else {
    trainRows <- 1:rows.train
  }

  #names(result) <- c("Train", "Verification")
  return (list(train=data[trainRows, ], validation=data[-trainRows, ]))
}

CreateMovingAverage <- function(data, window) {
  # TEST IT:
  # a <- rnorm(50)
  # smooth1 <- CreateMovingAverage(a, 3)
  # smooth2 <- CreateMovingAverage(smooth1, 5)
  # plot(a, type="l", col="blue")
  # lines(smooth1, col="red", lwd=2)
  # lines(smooth2, col="green3", lwd=2)
  # legend("topright", legend=c("Data","Smoother1","Smoother2"), col=c("blue","red","green3"), lwd=2, cex=.7)
  
  if (window %% 2 == 0) {
    window.start <- (window / 2) - 1
    window.end <- window / 2
  } else {
    window.start <- floor(window / 2)
    window.end <- floor(window / 2)
  }
  
  return (sapply(1:length(data),
                 function(x) ifelse(x > window.start, mean(data[(x-window.start):(x+window.end)]), NA)))
}

# Return some nice palettes
SetColors <- function(palette.no=1, setAsPalette=F, gradient=F, range=20) {
  if (gradient == T) {
    colors <- character(0)
    for (counter in 1:range) {
      if (palette.no == 1)
        colors <- c(colors, rgb(1 - (counter/(range * 2)), .5, .5 + (counter/(range * 2))))
      if (palette.no == 2)
        colors <- c(colors, rgb(.5, 1 - (counter/(range * 2)), .5 + (counter/(range * 2))))
      if (palette.no == 3)
        colors <- c(colors, rgb(.5, .5 + (counter/(range * 2)), 1 - (counter/(range * 2))))
      if (palette.no == 4)
        colors <- c(colors, rgb(.5, .5, 1 - (counter/(range * 2))))
      if (palette.no == 5)
        colors <- c(colors, rgb(1 - (counter/(range * 2)), .5, .5))
      if (palette.no == 6)
        colors <- c(colors, rgb(.5, 1 - (counter/(range * 2)), .5))
    }
    if (setAsPalette == T)
      palette(colors)
  } else {
    if (setAsPalette == T)
      palette(c("cornflowerblue", "wheat", "green3", "orange2", "blueviolet","aquamarine","darkolivegreen2",
                "darkseagreen2"))
  }
  
  if (setAsPalette == F)
    return(colors)
}

# Remove all objects in memory/workspace (NOTE: from parent environment for the function body) except objects.to.keep
RemoveObjects <- function(objects.to.keep="") {
  objects.to.remove <- ls(parent.frame())
  rm(list=objects.to.remove[!objects.to.remove %in% c("RemoveObjects", objects.to.keep)],
     envir=parent.frame())
  return (objects.to.remove)
}

# Upgrade R to the new version, and fix libraries, etc.
# SET R_LIBS="C:\Tools\edit\emacs\tb-tools\R\win-library\3.1"
# .libloc <<- "C:/Tools/edit/emacs/tb-tools/R/win-library/3.1"
UpgradeR <- function() {
  if(!require(installr)) { 
    install.packages("installr")
    require(installr)
  }
  
  updateR(download_dir="c:/temp/installR", keep_install_file=T, install_R=T, copy_packages=T, update_packages=T,
          quit_R=T)
}

# Convenience method to create intervals/curve (lin/log)  for a resolution from min to max value
# Test: Do not plot curve, just return curve data:
# plot(CreateCurveForResolution(2.5, 13, 28, F, F), type="o", col="blue", cex.lab=.7, cex.axis=.7, cex.main=1,
#      ylab="Values", xlab="Points", main="Create Curve for Resolution")
# lines(CreateCurveForResolution(2.5, 13, 28, T, F), type="o", col="red")
CreateCurveForResolution <- function(min.val, max.val, resolution, linear=T, do.plot=T) {
  interval <- ((min.val - max.val) * -1) / (resolution - 1)
  interval
  curve <- numeric(0)
  
  for (counter in 0:(resolution - 1))
    if (linear == T)
      curve <- c(curve, min.val + (counter * interval))
  else
    curve <- c(curve, (min.val + (counter * interval)^2 / (max.val - min.val)))
  
  if (do.plot == T) {
    op <- par()
    par(mar=c(5,4.5,3,1))
    plot(curve, pch=21, type="o", col="brown", bg="goldenrod1", main="Create Curve For Resolution",
         cex.main=1, cex.lab=.7, cex.axis=.7, xlab="Points", ylab="Values", cex=1.5)
    par <- op
  }
  
  return (curve)
}

# Euclidean distance: centroid (average between two or more points)
# Non-euclidean distance: clustroid: (data)point closest to other points
# TEST: find the point to use as the "centroid" in the clustroid: The point that's nearest to the other points:
GetClustroid <- function(n, distance.type=1, point.numbers=F, clustroid.numbered=T, show.centroid=T) {
  op <- par()
  par(mar=c(5,5,2.3,1))
  n <- n * 2
  p <- matrix(runif(n), ncol=2, byrow=T)
  p
  distances <- list()
  min_temp_dist <- 0.0
  old_temp_dist <- Inf
  
  for (counter in 1:nrow(p)) {
    centroid <- c(p[counter,1],p[counter,2])
    temp_dist <- 0.0
    
    for (counter2 in 1:nrow(p)) {
      if (counter2 != counter) {
        temp_dist <- temp_dist + sqrt((c(p[counter2,1],p[counter2,2]) - centroid) %*%
                                        (c(p[counter2,1],p[counter2,2]) - centroid))
      }
    }
    
    # TODO:
    # if (distance.type == 2) # distance.type=2: Smallest average distance to other points
    # if (distance.type == 3) # distance.type=3: Smallest sum of squares of distances to other points
    
    if (temp_dist < old_temp_dist) {
      min_temp_dist <- c(p[counter,1],p[counter,2])
      old_temp_dist <- temp_dist
    }
    distances[[counter]] <- temp_dist
  }
  
  # Get centroid too
  centroid <- c(mean(p[,1]), mean(p[,2]))
  
  plot(p, pch=21, bg="wheat", col="goldenrod", main="Clustroid test", cex.main=1, cex.lab=.7,
       cex.axis=.7, xlab="X", ylab="Y", cex=3.5, xlim=c(min(p[,1])-.1, max(p[,1])+.1),
       ylim=c(min(p[,2])-.1, max(p[,2])+.1))
  min_points <- which(unlist(distances) == min(unlist(distances)))
  
  for (counter in min_points)
    points(p[counter,1], p[counter,2], pch=21, bg="cyan", col="darkcyan", cex=3.5)
  
  if (show.centroid == T) # Plot centroid too
    points(centroid[1], centroid[2], col="red", cex=1.5, pch="+")
  
  if (point.numbers == T)
    text(x=p[,1], y=p[,2],1:nrow(p), cex=.6) # , pos=3, offset=-.1)
  else # Just plot clustroid number
    for (counter in min_points)
      text(x=p[counter,1], y=p[counter,2], counter, cex=.6)
  
  par <- op
  return (unlist(distances))
}

# TODO...
ExtractColorsFromImage <- function(image.file) {
  # http://www.mepheoscience.com/colourful-ecology-part-1-extracting-colours-from-an-image-and-selecting-them-using-community-phylogenetics-theory/
}

# Set some standard options for plot margins, etc.
SetStandardOptions <- function()
{
  par(cex.lab=.7)
  par(cex.main=.8)
  par(cex.axis=.7)
  par(mar=c(3,3,2,1))
  par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot
  options(digits=4) 
}

# H. Boström. Estimating class probabilities in random forests. In Proc. of the International Conference on
# Machine Learning and Applications, pages 211-216, 2007 (see PDF in Docs folder)
CalibrateRF <- function(data, r=.94) {
  result <- apply(data, 1, function(x) {
    #print(x[1] > x[2])
    if (x[1] == x[2])
      return (x)
    a <- x[1]
    b <- x[2]
    a.norm <- a + r * (1 - a)
    #a.norm
    b.norm <- b * (1 - r)
    #b.norm
    #a.norm + b.norm
    if (x[1] > x[2]) {
      x[1] <- a.norm
      x[2] <- b.norm
    } else {
      x[1] <- b.norm
      x[2] <- a.norm
    }
    return (x)
  })
  #rownames(result) <- NULL
  return (t(result))
}

PlotROC <- function(pred, y, title="ROCR plot") {
  # Show ROCR colorized plot
  library(ROCR)
  par(mar=c(3,3,2,2))
  predROCR = prediction(pred, y)
  score <- as.numeric(performance(predROCR, "auc")@y.values)
  perfROCR = performance(predROCR, "tpr", "fpr")
  plot(perfROCR, colorize=TRUE, main=paste0(title, " (Score = ", round(score, 4), ")"), lwd=3)
  lines(c(0,1),c(0,1), col="gray", lty=2)
  # TODO: Add text
  # http://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/
  # NOTE: At a cutoff of 0.6-0.8, we predict a good TP rate, while at the same time having a low FP rate.
  par(mar=c(3,3,2,1))
  # Compute and return AUC
  return (score)
  #sn <- slotNames(predROCR)
  #sapply(sn, function(x) length(slot(predROCR, x)))
}

GetOrdinalVariableRank <- function(data) {
  return (sapply(1:length(data), function(x) (x - 1) / (length(data) - 1)))
  # Example:
  # students <- c("freshman","sophomore","junior","senior")
  # rank <- GetOrdinalVariableRank(students)
  # Find distance: abs(rank[3] - rank[4])
}

# TIP: In linear regression, you can achieve a better RMSE by using quadratic features
# (like python's itertools.product([1,2,3], repeat=2)
# or itertools.permutations([1,2,3])):
GetQuadraticFeatures <- function(data) {
  my.array = numeric(0)
  counter <- 1
  pos <- 1
  size <- length(data)
  for (counter in 1:size) {
    for (inner_counter in 1:size) {
      my.array[pos] <- data[counter] * data[inner_counter]
      pos <- pos + 1
    }
  }
  return (my.array)
}

# TEST:
# n <- sample(8, 1) + 1
# poly.x <- sample(10, n)
# poly.y <- sample(10, n)
# cat("The area of the polygon is: ", GetPolygonArea(poly.x, poly.y))
# plot(min(poly.y):max(poly.y), type = "n", xlim=c(min(poly.x), max(poly.x)), ylim=c(min(poly.y), max(poly.y)))
# polygon(poly.x, poly.y, col="blue")
GetPolygonArea <- function(poly.x, poly.y) {
  mpoly <- matrix(c(poly.x, poly.y), length(poly.x), 2)
  mpoly
  
  a <- numeric(1)
  b <- numeric(1)
  i <- 0
  
  for (i in 1:(length(poly.x)-1)) {
    a[1] <- a[1] + (mpoly[i, 1] * mpoly[(i+1), 2])
    cat("a:", a[1], "\n")
  }
  
  
  for (i in 1:(length(poly.y)-1)) {
    b[1] <- b[1] + (mpoly[i, 2] * mpoly[(i+1), 1])
  }
  
  return (abs(a - b) / 2)
}

