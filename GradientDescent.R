# Gradient descent for cost function, logistic regression
##########################################################


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
max.vars <- 20
x <- sample(1:20, max.vars)
y <- ifelse(x <= 7, 0, 1)
lm1 <- glm(y ~ x)
summary(lm1)

e <- exp(1)

# TIPS: http://www.cleveralgorithms.com/machinelearning/optimization/gradient_descent.html
#       http://cs229.stanford.edu/notes/cs229-notes1.pdf
#       http://stackoverflow.com/questions/15478327/implementation-of-logistic-regression-formula-in-r
#       http://work.caltech.edu/library/091.html

# Cost function: -y*log(h<theta>(x)) - (1-y)*log*(1-h<theta>(x))
deriv.exp <- expression(-y * log(1 / (1 + e^(-x))) - (1 - y) * log(1 - (1 / (1 + e^(-x)))))
my.derivative <- D(deriv.exp, "x")
my.derivative <- (-(e^x * (-1 + y) + y)/(1 + e^x))

interval <- 0.01
counter = 1
breakout = 0
org.x <- x

while (breakout == 0) {
  old.x <- x
  
  x <- x - (interval * eval(my.derivative))
  
  if (abs(sum(old.x) - sum(x)) < interval) { break }
  
  if (counter > 10000) { breakout = 1 } # Just in case...
  
  counter <- counter + 1
}

org.x
x
y
#lm1$fitted


