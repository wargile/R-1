# Coursera: Andrew Ng's Machine Learning class
# --------------------------------------------
# Good notes:
# http://digitheadslabnotebook.blogspot.no/p/ml-class.html
# http://digitheadslabnotebook.blogspot.no/2012/07/linear-regression-by-gradient-descent.html

################################################################################################
# Week 1

# Three competing hyphotesis:
# h0(x) = -40 + 0-25x
# h0(x) = 200 + 0.1x
# h0(x) = -150 + 0.4x

# House sizes: 2104, 1416, 1534, 852

h <- matrix(nrow=2, ncol=3, c(-40,0.25,200,0.1,-150,0.4))

m <- matrix(nrow=4, ncol=2, c(1,1,1,1,2104,1416,1535,852))
dimnames(m) = list(c("House1", "House2", "House3", "House4"),c("Col1","Price"))

m %*% h # Note syntax for matrix multiplication

# Feature scaling (machine learning): Get every feature into approx. a (-1 <= xi <= 1) range:
base.a <-c(-2000,2000,30000,400,5000,600,70000,-70000)
base.b <-c(-0.0002, 0.005, -0.000123, 0.00056)
feature.a <- base.a / max(base.a)
feature.a
# Works on all size ranges...
feature.b <- base.b / max(base.b)
feature.b
# Ratio is:
ratio.a <- sum(base.a) / sum(feature.a)
# Get base.a from feature.a:
feature.a * ratio.a

# Another way: Scale including the mean + divide by (max-min):
feature.a <- (base.a - mean(base.a)) / (max(base.a) - min(base.a))
feature.a
# Another way: Scale including the mean + divide by sd:
feature.b <- (base.b - mean(base.b)) / sd(base.b)
feature.b

################################################################################################
# Week 2: TODO!

################################################################################################
# Week 3: Regression

par(mar=c(5,5,4,1))
par(mfrow=c(1,1))
malignant <- c(0,0,0,1,0,0,0,1,1)
tumorsize <- c(1.0, 1.1, 1.2, 3.2, 1.0, 0.9, 0.3, 2.2, 3.3)
train <- data.frame(tumorsize=tumorsize, malignant=malignant)

lm1 <- lm(malignant ~ tumorsize, data=train)
lm2 <- glm(malignant ~ tumorsize, data=train, family=binomial(link="logit"))
summary(lm2)
plot(train$malignant ~ train$tumorsize, pch=19, col="blue", main="Benign or malignant tumors")
abline(lm1$coef, col="red")
abline(lm2$coef[1], lm2$coef[2], col="green")

malignant.test <- c(1,1,0,1,0,0,0,1,1,0)
tumorsize.test <- c(3.0, 3.1, 1.2, 3.2, 1.0, 0.9, 0.3, 2.2, 8.2, 1.65)
test <- data.frame(tumorsize=tumorsize.test, malignant=malignant.test)
#test <- data.frame(tumorsize=tumorsize.test)
pred1 <- predict(lm2, newdata=test, type="response")
result <- ifelse(pred1 > 0.5, 1, 0)
result
error.rate <- (result == test$malignant)
error.rate.percent <- (1 - (length(error.rate[error.rate == TRUE]) / length(error.rate))) * 100
error.rate.percent

# Sigmoid/logistic function: g(z) = 1 / (1 + e^(-z))
# h<theta>(x) = P(y = 0 | x; theta) + P(y = 1 | x; theta) = 1
# h<theta>(x) = P(y = 0 | x; theta) = 1 - P(y = 1 | x; theta)
startval <- -4 # min(tumorsize.test) / max(tumorsize.test)
stopval <- 4 # max(tumorsize.test) / max(tumorsize.test)
interval <- 0.2
z <- seq(startval, stopval, interval)

e <- exp(1)
formula <- 1 / (1 + e^(-z))
plot(formula, pch=19, col="blue", xlab="Values", ylab="Probability", main="Sigmoid/logistic function")
abline(v=(((abs(startval) + stopval) / interval) / 2) + 1, col="cyan")
abline(h=0.5, col="cyan")

# TODO: Look at how to find decision boundaries (not sure about this...)
# See: C:\coding\R\Coursera\MachineLearning\Week 3\videos (video "Descision Boundary")
y.is.1 <- lm1$coef[1] + tumorsize.test >= 0
y.is.1

# Cost function:
# J(<theta>) = 1/m sum(i=1:m) of Cost(h<theta>(x(i)), y(i))
# Cost h<theta>(x, y) = -log(h<theta>(x)) if y = 1
#                       -log(1 - h<theta>(x)) if y = 0
z <- seq(0.01, 1, 0.01)
par(mfrow=c(1,1))
plot(-log(z), pch=19, type="l", col="blue")
lines(-log(1 - z), pch=19, col="red")
grid()
par(mfrow=c(1,1))

# More compact cost function:
# -y*log(h<theta>(x)) - (1-y)*log*(1-h<theta>(x))
# So if y = 1 we have:
# -1*log(h<theta>(x)) - (1-1)*log*(1-h<theta>(x))
# The second term goes away, and we're left with:
# -log(h<theta>(x)) if y = 1
# And if y = 0 we have:
# -0*log(h<theta>(x)) - (1-0)*log*(1-h<theta>(x))
# The first term goes away, and we're left with:
# -log(1 - h<theta>(x)) if y = 0

# So if we want min<theta> of J(<theta>), we need:
# Repeat {
#  <theta>j := <theta>j - a*(dericative of J(<theta)))
#  (simultaneously update all <theta>j. a = g.descent interval)
# }

deriv.exp <- expression(-y * log(1 / (1 + e^(-x))) - (1 - y) * log(1 - (1 / (1 + e^(-x)))))
my.derivative <- D(deriv.exp, "x")
my.derivative <- (-(e^x * (-1 + y) + y)/(1 + e^x))
interval <- 0.01
counter = 1
breakout = 0

while (breakout == 0) {
  old.x <- x
  
  x <- x - (interval * eval(my.derivative))

  if (abs(sum(old.x) - sum(x)) < interval) { break }
  
  if (counter > 1000) { breakout = 1 } # Just in case...
  
  counter <- counter + 1
}

x

#######################################################################################
# With iris data:
# TIPS: http://www2.warwick.ac.uk/fac/sci/moac/people/students/peter_cock/r/iris_plots
# TIPS: http://www.math.wustl.edu/~victor/classes/ma322/r-eg-26.txt
data(iris)
head(iris)
plot(Sepal.Length ~ Species, data=iris, col="orange")
plot(iris[-5], main="Edgar Anderson's Iris Data", pch=21,
     bg=c("red","green3","blue")[unclass(iris$Species)])
pairs(iris[1:4], main="Edgar Anderson's Iris Data", pch=21,
      bg=c("red", "green3", "blue")[unclass(iris$Species)]) # Same as above

testrows <- sample(1:nrow(iris), nrow(iris)/2)
iris.train <- iris[testrows, ]
iris.test <- iris[-testrows, ]
lm.iris <- lm(unclass(Species) ~ ., data=iris.train)
lsfit(iris$Petal.Length, iris$Petal.Width)$coefficients
summary(lm.iris)
plot(iris.train$Petal.Length, iris.train$Petal.Width, pch=21,
     bg=c("red","green3","blue")[unclass(iris.train$Species)],
     main="Edgar Anderson's Iris Data", xlab="Petal length", ylab="Petal width")
abline(lsfit(iris$Petal.Length, iris$Petal.Width)$coefficients, col="blue")

pred.iris <- predict(lm.iris, newdata=iris.test, type="response")
pred.iris
