# Regression, Apache Spark
# Partial derivative, gradient, etc.:
# https://www.khanacademy.org/math/multivariable-calculus/partial_derivatives_topic
# https://www.khanacademy.org/math/multivariable-calculus/partial_derivatives_topic/partial_derivatives/v/partial-derivatives

SetStandardOptions()

data <- rnorm(100)
weights <- c(-1.24, 2.16, 0.76)
df <- data.frame(x1=data, x2=data*2, x3=data/1.2)
t(weights) * df
plot(df$x1, col="blue", type="l", ylim=c(floor(min(df$x1, df$x2, df$x3)), ceiling(max(df$x1, df$x2, df$x3))))
lines(df$x2, col="red")
lines(df$x3, col="green4")

# Objective: Minimize squared loss: (y - y.hat)^2
# Linear assumption: y.hat = t(w)*x
# Find w that minimizes squared loss over training points
# min(w) = sum(i=1:n) of ((t(w)*x(i) / y.hat(i)) - y(i))^2

# Given two-dimensinal data, quadratic features (phi) are:
x1 <- 1.56
x2 <- 2.11
x <- t(c(x1, x2))
phi.x <- t(c(x1^2, x1*x2, x2*x1, x2^2))
phi.x
z1 <- 2.56
z2 <- 3.11
z <- t(c(z1, z2))
phi.z <- t(c(z1^2, z1*z2, z2*z1, z2^2))
phi.z
# Inner product:
(x1*z1) + (2*x1*x2*z1*z2) + (x2*z2)

# Most ML models have free parameters (called 'hyperparameters'), like lambda in ridge regression, that needs to be tuned.
# We can use a validatiopn set (and not the test set), to verify best tune result for hyperparameters.
# So then we split our train set into THREE sets: train, validation and test.
# Otherwise, we may overfit if we tune these parameters on the test set.
# - Training: Train various models
# - Validation: Evalulate various models (e.g. Grid Search)
# - Test: Evalutate final model's accuracy

# Least Squares Regression, Closed Form Solution (if inverse (^-1) exists):
# w = (t(X)*X)^-1 * (t(X)*y)
# Computation time complexity in BigO notation (n=number of observations, d=number of features): O(nd^2 + d^3)
# Storage complexity: Consider storing values as 64-bit floats (8 bytes): O(nd + d^2)
# 1) t(X)*X and its inverse: O(d^2) floats
# 2) X: O(nd) floats
# This process in Apache Spark, simplified notation: trainData.map(computeOuterProduct).reduce(sumAndInvert) 

# Vector norms:
# https://en.wikipedia.org/wiki/Norm_%28mathematics%29

# Vector update:
# w<i+1> = w<i> * sum(j=1:n) of ((t(w)<i> * x<j>) - y<j>) * x<j>
# The summand is: ((t(w)<i> * x<j>) - y<j>) * x<j>
# Above is the MAP step!
# Compute summands in parallell. Note: Workers must ALL have w<i>, so set as accumulator?
# Then the reduce step sums all the summands together

# Python code for gradient descent:
# train.cache() # Important, minimize network communication across worker nodes!
# for i in range(numIters):
#   alpha_i = alpha / (n * np.sqrt(i + 1))
#   gradient = train.map(lambda lp: gradientSummand(w, lp)).sum()
#   w -= alpha_i * gradient
# return w

# Speeds:
# RAM to CPU: 50GB/sec
# Disk to CPU: 100MB/sec
# Network: 10Gbps (= 1GB/sec)

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

my.array <- c(2,3)
GetQuadraticFeatures(my.array)
my.array <- c(1,2,3)
GetQuadraticFeatures(my.array)
my.array <- c(1,2,3,4)
GetQuadraticFeatures(my.array)
my.array <- 1:8
plot(sort(GetQuadraticFeatures(my.array)), pch=21, bg="cyan", main="Quadratic Features test", ylab="Data")

# Test it:
n <- 100
y <- rnorm(n)
x1 <- y * 10 + rnorm(n)
x2 <- x1 + rnorm(n)
df <- data.frame(y=y, x1=x1, x2=x2, x3=rep(0, n), x4=rep(0, n), x5=rep(0, n))
for (counter in 1:nrow(df)) {
  q.features <- GetQuadraticFeatures(c(df$x1[counter], df$x2[counter]))
  #cat(paste(q.features, "\n"))
  df$x3[counter] <- q.features[1]
  df$x4[counter] <- q.features[2]
  df$x5[counter] <- q.features[4]
  #df$x6[counter] <- q.features[4]
}
head(df)

# Technically, would this be the same as doing I(x^2), I(x^3) and so on, on predictors in lm?
model1 <- lm(y ~ ., data=df[, 1:3])
summary(model1)
model2 <- lm(y ~ ., data=df)
summary(model2)
