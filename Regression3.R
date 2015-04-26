SetStandardOptions()

# DataScience Specialization - 07 Regression
library(UsingR)
data(galton)
par(mfrow=c(1,2))
hist(galton$child, col="cornflowerblue", breaks=20, main="Child", cex.lab=.8, cex.axis=.8)
hist(galton$parent, col="wheat", breaks=20, main="Parent", cex.lab=.8, cex.axis=.8)
par(mfrow=c(1,1))

result <- lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data=galton) # -1: Don't fit the intercept
summary(result)
par(mfrow=c(1,2))
plot(child ~ parent, data=galton, pch=21, col="blue", bg="cyan", main="Galton",
     xlim=c(0, max(galton$parent)), ylim=c(0, max(galton$child)))
intercept <- mean(galton$child) - (result$coeff * mean(galton$parent))
intercept
abline(a=intercept, b=result$coeff, col="red")
result2 <- lm(child ~ parent, data=galton)
summary(result2)
plot(child ~ parent, data=galton, pch=21, bg="cyan", col="blue", main="Galton") # Also get intercept here
abline(result2, col="red")
par(mfrow=c(1,1))

# Least squares:
mse <- numeric()
pos <- 1
for (counter in seq(min(galton$child), max(galton$child), by=1)) {
  mse[pos] <- mean((galton$child - counter)^2) # Mean squared error
  # sum((galton$child - counter)^2) # Sum of squared errors
  pos <- pos + 1
}
plot(mse, type="o", pch=21, col="blue", bg="cyan", cex.lab=.8, cex.axis=.8, main="MSE", ylab="MSE", xlab="Height")
abline(v=which(mse == min(mse)), col="red")
sum((galton$child - mean(galton$child))^2)

library(manipulate)
myHist <- function(mu) {
  hist(galton$child, col="blue", breaks=100)
  lines(c(mu, mu), c(0, 150), col="red", lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 140, paste("MSE = ", round(mse, 2)))
}
# NOTE: slider is also in aplpack package, so prefix:
manipulate(myHist(mu), mu = manipulate::slider(min=62, max=74, initial=62,step=.5))

# Logistic regression:
# http://www.youtube.com/watch?v=znDexex66oE
# Convert from logit result theta = (log(u - (1 - u))) back to probabilities between 0 and 1:
# Handle large numbers with exp():
# install.packages("Rmpfr")
# library("Rmpfr")
n <- 60
size <- 1
y <- sample(0:1, n*2, replace=T)
x <- runif(n) + (y[1:n] * 4) + rnorm(n)
fit <- glm(y[1:n] ~ x, family=binomial(link="logit"))
summary(fit)
new.y <- y[(n+1):(n*2)]
new.data <- data.frame(x=runif(n) + (new.y * 4) + rnorm(n))
theta <- predict(fit, newdata=new.data)
theta <- theta / 100 # Avoid Inf problem with exp()
pred.result <- ifelse(exp(theta) / (1 + exp(theta)) < 0.5, 0, 1)
my.rmse <- MyRMSE(new.y, pred.result)
# sqrt(n.misclass / n) gives same result
# n.misclass <- length(which(pred.result != new.y))
plot(pred.result, type="p", col="red", ylim=c(-0.5, 1.5), cex.main=1, cex.lab=.8,
     pch=19, cex.axis=.8, main=paste0("GLM, RMSE=", round(my.rmse, 4)), lwd=size) # Plot prediction
points(new.y, col="gray", pch=19, lwd=size) # Plot holdout y
table(new.y, pred.result)
ConfusionMatrix(table(new.y, pred.result), c("0","1"))
