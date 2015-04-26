# Regression formulas:
# http://www3.nd.edu/~rwilliam/stats2/l02.pdf
#------------------------------------------------------------
# Matrix programming based approach
#------------------------------------------------------------

# Regression matrices require a column of 1's in order to calculate 
# the intercept or constant, create this column of 1's as x0
x0 <- c(1,1,1,1,1) # column of 1's
x1 <- c(1,2,3,4,5) # original x-values
x2 <- c(3,2,3,2,3) # TEST: Add another predictor variable

# create the x- matrix of explanatory variables
x <- as.matrix(cbind(x0,x1,x2))
x

# create the y-matrix of dependent variables
y <- as.matrix(c(3,7,5,11,14))
y

# estimate  b = (X'X)^-1 X'y
b <- solve(t(x)%*%x)%*%t(x)%*%y

print(b) # this gives the intercept and slope - matching exactly 
# the results from lm() below
model1 <- lm(y ~ x1 + x2)
summary(model1)
model2 <- glm(y ~ x1 + x2)
summary(model2)


# Using the example in StatisticsOne lecture 11, slide 52 in R:
Xnp <- matrix(c(3,2,3, 3,2,3, 2,4,4, 4,3,4, 4,4,3, 5,4,3, 2,5,4, 3,3,2, 5,3,4, 3,5,4), nrow=10, byrow=T)
Xnp
X1 <- matrix(c(rep(1,10),Xnp[,2:3]),nrow=10) 
# rep(1,10): a vector of 1s of size 10, 
# concatenate with 2nd and 3rd cols of Xnp
X1
X1t <- t(X1) # transpose of X1
y <- Xnp[,1] # first column of Xnp
B <- solve(X1t %*% X1) %*% (X1t %*% y)
# solve() gives inverse of matrix, %*% multiplies matrices
cat("Result for B (regression coefficients):\n")
B

# Test it:
cat("Result from lm:\n")
model2 <- lm(y ~ Xnp[,2] + Xnp[,3])
summary(model2)

# Implement feature scaling
Xnp.scaled <- X1
Xnp.scaled[, 2:3] <- (Xnp[, 2:3] - mean(Xnp[, 2:3])) / sd(Xnp[, 2:3])
solve(t(Xnp.scaled) %*% Xnp.scaled) %*% t(Xnp.scaled) %*% y # w/ feature scaling
# Results using standard lm function match results above
# summary(lm(y ~ Xnp[, 2] + Xnp[, 3])) # w/o feature scaling
summary(lm(y ~ Xnp.scaled[, 2] + Xnp.scaled[, 3])) # w/feature scaling

# http://www.win-vector.com/blog/2012/08/what-does-a-generalized-linear-model-do/
# Sigmoid function:
s <- function(z) { 1 / (1 + exp(-z)) }
# Properties of sigmoid function:
s(0) # 0.5
s(-z) # 1-s(z)
#deriv(s(z), "z")
s(z)*(1-s(z))
s(-Inf) # 0
s(Inf) # 1

d <- read.table(file='http://www.win-vector.com/dfiles/glmLoss/dGLMdat.csv',
                header=T,sep=',')
head(d)

m <- glm(y ~ x1 + x2, data=d, family=binomial(link='logit'))
summary(m)
sum(with(d, 2 * (s - y)))
sum(with(d, 2 * (s - y) * x1))
sum(with(d, 2 * (s - y) * x2))

p <- function(x) { s(x[1] + x[2] * d$x1 + x[3] * d$x2) }

f <- function(x) {
  v <- p(x)
  d <- v - d$y
  sum(d * d)
}

f(m$coefficients)
m$coefficients
sum(with(d, (y - s) * (y - s)))

# We now derive and plug in our modified coefficients and show the lower square loss:
opt <- optim(m$coefficients, f, method='CG')  # default optimizer failed to optimize
opt$par
f(opt$par)

# Showing y, fitted, residuals, abline, etc. in same plot
x <- runif(30)
y <- x * rnorm(30)
model1 <- lm(y ~ x)
summary(model1)
plot(y ~ x, pch=19, col="blue", main="Regression example")
abline(model1, col="red")
points(model1$coeff[1] + (model1$coeff[2] * x) ~ x, col="red", pch=19)
# Above is same as:
points(model1$fitted ~ x)
points(model1$residuals ~ x, col="green")

# Another example: We assume that y = f(x) = 2 + 3x^2
maxpoints <- 200
x <- runif(maxpoints)
y <- (2 + ((3 * x)^2)) + abs(rnorm(maxpoints) / 4) # Introduce some noise...
model1 <- lm(y ~ x)
summary(model1) # Note that intercept and slope is close to 2 and 3, and that R is high
plot(y ~ x, pch=19, col="blue", main="Regression Example", ylim=c(min(model1$residuals), max(y)))
abline(2, 3^2, col="green", lwd=2) # The observed relationship
abline(model1$coeff[1], model1$coeff[2], col="red", lwd=2)
points(model1$fitted ~ x, col="red")
points(model1$residuals ~ x, col="green")

# Example with prediction, finding ratio, ROC curve, etc. Not sure about the ratio thingy on this....
# http://www.r-bloggers.com/roc-curves-and-classification/
n <- 500
y <- sample(c(0, 1), n, replace=T)
y.new <- sample(c(0, 1), n/2, replace=T)
x.new <- data.frame(x=rnorm(n/2) * ((y.new + 1) * 10))
x <- rnorm(n) * ((y + 1) * 10)
model1 <- glm(y ~ x, family=binomial(link="logit"))
summary(model1)
plot(model1$fitted)
result <- predict.glm(model1, newdata=x.new, type="response")
round(result) == y.new
ratio <- 1 - length(y.new[y.new == 1]) / length(y.new)
ratio
result.final <- ifelse(result > ratio, 1, 0)
result.final == y.new




result <- predict()