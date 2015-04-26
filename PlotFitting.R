# Tips on adding trend lines:
# http://stackoverflow.com/questions/15102254/how-do-i-add-different-trend-lines-in-r
# Also see: {splines} and {mgcv} packages

# NOTE: Some really nice examples here!
# http://www.r-bloggers.com/supervised-classification-beyond-the-logistic/
# or:
# http://freakonometrics.hypotheses.org/19277
# -----------------------------------------------------------------------------------------------------------------

library(scales)

SetStandardOptions()

# Using polynomials (poly() from {stats}) to alter fit
n <- seq(-pi,pi,.02)^3
y <- (n * abs(rnorm(length(n))))
x <- seq(1, length(y))
plot(y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
model1 <- lm(y ~ x)
summary(model1)
lines(model1$fitted, col="cyan", lwd=2)
model2 <- lm(y ~ poly(x, 2))
summary(model2)
lines(model2$fitted, col="red", lwd=2)
model3 <- lm(y ~ poly(x, 3))
summary(model3)
lines(model3$fitted, col="green3", lwd=2)
model4 <- lm(y ~ poly(x, 4))
summary(model4)
lines(model4$fitted, col="violet", lwd=2)

# More polynomials
range <- seq(-10,10,.5)
y <- 1/(1 + exp(-1)^range)
x <- 1:length(y)
plot(y, type="o", col="blue", ylim=c(-.1,1.1), main="Curve Fitting")
result1 <- lm(y ~ poly(x, 3))
points(result1$fitted, col="red", pch=21, bg="yellow")
result2 <- lm(y ~ x)
points(result2$fitted, col="green4", pch=21, bg="lightgreen")

# Exp
x <- seq(-2, 2, by=0.02)
y <- x^2 + rnorm(length(x))/2
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
fit1 <- lm(y ~ x)
summary(fit1)
lines(x, fit1$fitted, col="green", lwd=2)
fit2 <- lm(y ~ (x * exp(x)))
fit2 <- lm(y ~ I(x^2)) # NOTE: Same result as above
summary(fit2)
lines(x, fit2$fitted, col="red", lwd=2)

plot(fit1$residuals, col="blue", main="Residuals")
points(fit2$residuals, col="red")

x <- seq(-4, 0, by=0.02)
y <- x^2 + rnorm(length(x))
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
fit1 <- lm(y ~ x)
summary(fit1)
lines(x, fit1$fitted, col="green", lwd=2)
fit2 <- lm(y ~ (x * exp(x)))
fit2 <- lm(y ~ I(x^2)) # NOTE: Same result as above
summary(fit2)
lines(x, fit2$fitted, col="red", lwd=2)


# Make a prediction
x.new <- seq(-2, 2, by=0.02) + rnorm(length(x))/8
plot(x.new, type="l")
result <- predict(fit2, newdata=data.frame(x=x.new), type="response")
plot(result, pch=19, col=alpha("blue", .3), main="Prediction result")

plot(fit2$fitted, fit2$resid)

# Do Box-Cox
library("MASS")
par(mfrow=c(1,2))
boxcox(abs(y) ~ I(x^2))                            # basic box-cox plot
boxcox(abs(y) ~ I(x^2), lambda=seq(-.2,.2,.01))    # finer resolution on lambda
par(mfrow=c(1,1))
oldmar <- par()$mar
par(mar=c(3,3,2,1))
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))
par(mar=oldmar)


# Log
x <- seq(0, 2, by=0.01)
y <- log(x + 0.01) + rnorm(length(x))/2
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
fit1 <- lm(y ~ x)
summary(fit1)
lines(x, fit1$fitted, col="green", lwd=2)
fit2 <- lm(y ~ (x * log(x + 0.01)))
summary(fit2)
lines(x, fit2$fitted, col="red", lwd=2)

plot(fit1$residuals, col="blue", main="Residuals")
points(fit2$residuals, col="red")

# x^3, using I() function
x <- seq(-2, 2, by=0.01)
y <- (x^3) + rnorm(length(x))/2
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
fit1 <- lm(y ~ x)
summary(fit1)
lines(x, fit1$fitted, col="green", lwd=2)
fit2 <- lm(y ~ I(x^3)) # NOTE: Use I() here!
summary(fit2)
lines(x, fit2$fitted, col="red", lwd=2)

plot(fit1$residuals, col=alpha("blue", .5), pch=16, main="Residuals")
points(fit2$residuals, col=alpha("red", .5), pch=16)

# Get some details and compare the fit's
oldmar <- par()$mar
par(mfrow=c(2,2))
par(mar=c(2.5,2,1.5,.5))
plot(fit1, cex.axis=.8)
plot(fit2, cex.axis=.8)
par(mfrow=c(1,1))
par(mar=oldmar)

# Cos * Sin
oldmar <- par()$mar
par(mar=c(4.5,4,2.5,.7))
x <- seq(-3, 3, by=0.01)
y <- 2 * (cos(x) * sin(x)) + rnorm(length(x))/2
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting")
fit1 <- lm(y ~ x)
summary(fit1)
lines(x, fit1$fitted, col="green", lwd=2)
fit2 <- lm(y ~ I(cos(x)*sin(x))) # NOTE: Use I() here!
summary(fit2)
lines(x, fit2$fitted, col="red", lwd=2)
legend("topright", legend=c("y ~ x","fitted","cos * sin"),
       col=c("blue","green","red"), lwd=2, cex=.7)
par(mar=oldmar)

plot(fit1$residuals, col="blue", main="Residuals")
points(fit2$residuals, col="red")

# x + x^2 + x^3
op <- par()
par(mar=c(4.5,4,2.5,.7))
x1 <- seq(-1, 1, by=0.01)
x2 <- seq(0, 4, by=0.02)
x3 <- seq(-4, 4, by=0.04)
y <- (x1 * x2^2 * x3^3) * runif(201, 1, 3)
plot(x1, y, pch=19, col=alpha("blue", .3), main="Curve Fitting",
     ylim=c(-100, max(y)), xlab="x")
fit1 <- lm(y ~ x1 + I(x2^2) + I(x3^3))
summary(fit1)
lines(x1, fit1$fitted, col="red", lwd=2)
fit2 <- lm(y ~ x1 + I(x2 * x2^2) + I(x3 * x3^3))
summary(fit2)
lines(x1, fit2$fitted, col="green4", lwd=2)
par <- op

# x + x^2 + x^3
op <- par()
par(mar=c(4.5,4,2.5,.7))
x1 <- seq(-1, 1, by=0.01)
x2 <- seq(-2, 2, by=0.02)
x3 <- seq(-4, 4, by=0.04)
y <- (x1 * x2^2 * x3^3) * runif(201, 1, 3)
plot(x1, y, pch=19, col=alpha("blue", .3), main="Curve Fitting",
     ylim=c(-100, max(y)), xlab="x")
fit1 <- lm(y ~ x1 + I(x2^2) + I(x3^3))
summary(fit1)
lines(x1, fit1$fitted, col="red", lwd=2)
fit2 <- lm(y ~ x1 + I(x2 * x2^2) + I(x3 * x3^3))
summary(fit2)
lines(x1, fit2$fitted, col="green4", lwd=2)
par <- op

# 4x^5 + 2x^(1/3)
op <- par()
par(mar=c(4.5,4,2.5,.7))
x <- seq(0.01, 1, by=0.01)
y <- (4*x^5 + 2*x^(1/3) * runif(100, .5, 2)) + 5
plot(x, y, pch=19, col=alpha("blue", .3), main="Curve Fitting",
     xlab="x")
fit1 <- lm(y ~ I(x^5))
summary(fit1)
lines(x, fit1$fitted, col="red", lwd=2)
fit2 <- lm(y ~ I(x^5) + I(x^(1/3)))
summary(fit2)
lines(x, fit2$fitted, col="green4", lwd=2)
par <- op

# Try a LOOCV on the last one:
rows <- length(y)
acc <- numeric(0)
for (counter in 1:rows) {
  #fit1 <- lm(y[-counter] ~ I(x[-counter]^5))
  fit1 <- lm(y[-counter] ~ I(x[-counter]^5) + I(x[-counter]^(1/3)))
  pred <- predict(fit1, newdata=data.frame(x=x[counter]))
  acc[counter] <- abs(y[counter] - pred)
}
acc

# -----------------------------------------------------------------------------------------
# Using runif to generate some data and create a nice regression plot...
max_elements <- 80
x <- 1:max_elements
y=(1:max_elements) + runif(max_elements, 1, 40)
plot(x, y, pch=19, col="blue",  main="Fit")
fit <- lm(y ~ x)
summary(fit)
cor(x, y)
#grid()

for (counter in 1:max_elements) {
  segments(x0=counter, x1=counter, y0=fit$coef[1] + counter * fit$coef[2],
           y1=y[counter], col="gray")
}

abline(fit, col="red", lwd=1)
points(x, y, pch=19, col="blue")

# ------------------------------------------------------------------------------------------
# Residuals, etc.
elements <- 40
x <- 1:elements
y <- rnorm(1:elements) + (x^2 / 2.5)
plot(y ~ x, pch=16, col="blue", main="y ~ x")
fit <- lm(y ~ x)
summary(fit)
abline(fit, col="red")
fit2 <- lm(y ~ x * I(x^1.75))
summary(fit2)
abline(fit2, col="green")
lines(fit2$fitted)

# These are the same:
fit$residuals
y - fit$fitted

# -------------------------------------------------------------------------------------------
# Distribution with high variance at tails, low in middle
start <- -2
end <- 2
x <- seq(from=start, to=end, by=.01)
y <- (rnorm(400) * x[-1])
plot(y, pch=16, col=alpha("blue", .3), bg="cyan", main="y ~ x")
fit1 <- lm(y ~ I(x[-1]^2))
summary(fit1)
#abline(fit1, col="red")
lines(fit1$fitted, col="cornflowerblue", lwd=2)
fit2 <- lm(y ~ I(x[-1]^3))
summary(fit2)
#abline(fit2, col="green")
lines(fit2$fitted, col="cyan", lwd=2)
lines(x[-1]^3, col="red")

x <- seq(start*100, end*100)
x <- x[-(length(x)-1)]
formula <- (cos(-x^3 + 1)*x) / 50
# plot(formula, type="l", col="blue")

fit3 <- lm(y ~ I(formula))
summary(fit3)
lines(formula, col="green", lwd=1)

# --------------------------------------------------------------------------------------------------
# Bootstrap example
op <- par()
par(mar=c(4.8,4.5,2,1))
n <- 75
x <- rnorm(n)
y <- rnorm(n) + x
coeff.x <- numeric(0)
plot(y ~ x, col="gray", pch=19, main="Bootstrap example")
for (counter in 1:6) {
  rows <- sample(1:n, n, replace=T) # NOTE: Easy way to get random row number!
  x.sample <- x[rows]
  y.sample <- y[rows]
  fit <- lm(y.sample ~ x.sample)
  #summary(fit)
  coeff.x[counter] = fit$coeff[1]
  abline(fit, col="blueviolet")
}
mean(coeff.x) # Get the mean of the x coeff for all regression lines
par <- op

# --------------------------------------------------------------------------------------------------
# http://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)
# Using example from text:
x=1:4
y=c(6,5,7,10)

x <- -100:100
y <- (x + rnorm(length(x)) * 5)^3

# Get slope and intercept:
SSxy <- sum((y - mean(y)) * (x - mean(x))) / length(x)
SSx <- sum((x - mean(x))^2) / length(x)

SSxy/SSx
# Same as:
slope <- cov(x, y) / var(x)
slope

intercept <- mean(y) - (slope * mean(x))
intercept
min.y <- min((x * slope) + intercept) - 2
if (min.y > min(y)) min.y <- min(y)
max.y <- max(y) + 2
plot(y ~ x, col="blue", pch=21, bg="cyan", ylim=c(min.y, max.y), xlim=c(min(x), max(x)))
fit <- lm(y ~ x)
fit <- lm(y ~ I(x^3))
summary(fit)
#grid()
#abline(fit, col="red")
lines(x, fit$fitted, col="red", lwd=1)
points(x, (x * slope) + intercept, col="green4") 
abline(v=0, col="green")

# --------------------------------------------------------------------------------------------------------
# Creating a smoothing function with simple moving average
n <- 100
x <- 1:n
y <- x + rnorm(n) * 10
plot(x, y, pch=16, col="gray", main="WMA")
sma <- rep(0, n)
start <- 3
for (counter in start:(length(y)-1)) {
  sma[counter] <- mean(y[(counter - 2):(counter + 1)])
}
lines(sma[start:counter], col="red")

# ---------------------------------------------------------------------------------------------------------
# First derivative
x <- runif(1:50)
df <- data.frame(x1=x, x2=x*1.2, x3=x*1.35, x4=x*1.6) # Difference if linearly dependent or not??
#df <- data.frame(x1=x, x2=x*1.2, x3=x*1.21, x4=x*1.22)
df_der <- df - cbind(NA, df)[, -(dim(df)[2]+1)]
y.min <- min(df, df_der, na.rm=T)
y.max <- max(df, df_der, na.rm=T)

plot(df$x1, type="l", ylim=c(y.min, y.max), main="First derivative example", ylab="Measurements")
lines(df$x2, type="l", col="red")
lines(df$x3, type="l", col="green")
lines(df$x4, type="l", col="blue")

lines(df_der$x2, type="l", col="violet")
lines(df_der$x3, type="l", col="brown")
lines(df_der$x4, type="l", col="magenta")

# ---------------------------------------------------------------------------------------------------------
# Step function, fit model params to parts of outcome variable
n <- 400
start.age <- 35
end.age <- 65
start.age.p <- start.age * n / 100
end.age.p <- end.age * n / 100
age.interval <- start.age.p:end.age.p
y <- abs(rnorm(1:n)) * 10
y[age.interval] <- y[age.interval] * 2
x <- 1:n
fit1 <- lm(y ~ x)
summary(fit1)
xx <- x
xx[age.interval] <- xx[age.interval] * 2
fit2 <- lm(y ~ x + xx)
summary(fit2)
plot(y, col="blue", main="Step function", xlab="Age")
lines(fit2$fitted, col="red")

library(ISLR)
oldmar <- par()$mar
par(mar=c(5,4.7,2.5,1))
# ISLPR ch 1: Income survey data for males in central Atlantic region of USA
data(Wage)
head(Wage, n=2)

plot(Wage$wage ~ Wage$age, col="gray", pch=16, xlab="Age", ylab="Wage", main="Wage by age")
mean.wage <- aggregate(wage ~ age, data=Wage, FUN=mean)
lines(mean.wage, lwd=2, col="steelblue4")

fit <- lm(wage ~ poly(age, 4), data=Wage)
summary(fit)
lines(fit$fitted, col="red")

# ----------------------------------------------------------------------------------------------------------
# Local regression
# Create a decaying weight function on local neigbourhood of x0:
# - Calculate euclidean distance from (x0,y0) to (xn,yn), closest point is 1, furthest is 0
op <- par()
n <- 200
x <- (n / 2 * -1):((n / 2) - 1)
y <- (abs(rnorm(n)) * 30 + x)^2
plot(y ~ x, col="gray", pch=19, main="Local Regression example")
range <- n / 6 # Number of 'slices'/cuts for local regression, TODO: Use 'cut' function here?
e.dist <- numeric(0)

for (counter in 0:((n / range) - 1)) {
  pos <- 1
  interval <- ((counter * range) + 1):((counter * range) + range)
  pos0 <- interval[range / 2]
  for (counter in interval) {
    # Find the Euclidean distance:
    e.dist[pos] <- sqrt((c(x[counter], y[counter]) - c(x[pos0], y[pos0])) %*%
                          (c(x[counter], y[counter]) - c(x[pos0], y[pos0])))
    pos <- pos + 1
  }

  e.dist.norm <- Normalize(min(e.dist), max(e.dist), 0, 1, e.dist)
  #plot(sort(e.dist.norm))
  fit.local <- lm(y[interval] ~ I(x[interval] + e.dist.norm))
  #summary(fit.local)
  lines(x[interval], fit.local$fitted, col="red", lwd=2)
}

par <- op
