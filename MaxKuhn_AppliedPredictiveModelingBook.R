# Max Kuhn: Applied Predictive Modeling

# http://appliedpredictivemodeling.com/
# http://appliedpredictivemodeling.com/data/
# http://cran.r-project.org/web/packages/AppliedPredictiveModeling/AppliedPredictiveModeling.pdf

package.install("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
?AppliedPredictiveModeling
scriptLocation()

data(FuelEconomy)
head(cars2010)
head(cars2011)
head(cars2012)
names(cars2010)
names(cars2011)
cars2010$year <- "2010"
cars2011$year <- "2011"
cars2012$year <- "2012"
cars.2010.2011 <- rbind(cars2010, cars2011)
View(cars.2010.2011)

cor(cars2010$FE, cars2010$EngDispl)

fit1 <- lm(FE ~ EngDispl, data=cars2010)
summary(fit1)
# Fit with squared EngDispl predictor
fit2 <- lm(FE ~ EngDispl + I(EngDispl^2), data=cars2010)
summary(fit2)
# Using splines
# http://cran.r-project.org/web/packages/mda/mda.pdf
library(mda)
fit3 <- bruto(y=cars2010$FE, x=cars2010$EngDispl)
summary(fit3)

# http://en.wikipedia.org/wiki/Engine_displacement
# pi/4 * bore^2 * stroke * cylinders
xyplot(FE ~ EngDispl|year, data=cars.2010.2011, pch=16, col=cars.2010.2011$EngDispl,
       ylab="Fuel Efficiency (MPG)", xlab="Engine Displacement", main=c("Model Year 2010","Model Year 2011"))

# Plot the data and fit:
plot(FE ~ EngDispl, data=cars2010, col=cars2010$EngDispl, pch=16, cex.main=1,
     ylab="Fuel Efficiency (MPG)", xlab="Engine Displacement", cex.lab=.8, cex.axis=.8, main="Model Year 2010")
abline(fit1, col="gray")
formula <- fit2$coef[1] + (fit2$coef[2] * cars2010$EngDispl) + (fit2$coef[3] * cars2010$EngDispl^2)
lines(formula ~ cars2010$EngDispl, col="green")
# Plot observed/predicted spread:
plot(cars2010$FE, fit1$fitted, ylim=c(min(cars2010$FE), max(cars2010$FE)), cex.main=1,
     pch=19, col="blue", ylab="Predicted", xlab="Observed", cex.lab=.8, cex.axis=.8,
     main="Model Year 2010 - obs/pred fit1")
plot(cars2010$FE, fit2$fitted, ylim=c(min(cars2010$FE), max(cars2010$FE)), cex.main=1,
     pch=19, col="blue", ylab="Predicted", xlab="Observed", cex.lab=.8, cex.axis=.8,
     main="Model Year 2010 - obs/pred fit2")

# Centering and scaling, p. 30
par(mfrow=c(2,2))
oldmar=par()$mar
par(mar=c(5, 4, 2,.5))
data <- rbinom(120, 25, prob=.10)
hist(data, col="orange", cex.main=1, cex.lab=.8, cex.axis=.8)
# Centered, mean = 0:
centered <- data - mean(data)
hist(centered, col="cornflowerblue", cex.main=1, cex.lab=.8, cex.axis=.8, main="Centered, mean=0")
abline(v=mean(centered), col="red", lwd=2)
# Scaled, sd = 1:
scaled <- (data - mean(data)) / sd(data)
hist(scaled, col="wheat", cex.main=1, cex.lab=.8, cex.axis=.8, main="Scaled, sd=1")
abline(v=sd(scaled), col="red", lwd=2)
hist(log(data), col="orange", cex.main=1, cex.lab=.8, cex.axis=.8, main="Log transform")
par(mar=oldmar)
par(mfrow=c(1,1))

# Skewness, p. 31
SkewnessStatistic <- function(data) {
  # Sample skewness statistic? Kuhn page 31
  # create a skewed distribution: x <- dbeta(seq(0,1,.001), 18,4,20)
  v <- sum((data - mean(data))^2) / (length(data) - 1)
  ratio <- sum((data - mean(data))^3) / ((length(data) - 1) * v^(3/2))
  return (ratio)
}
SkewnessStatistic(data)
SkewnessStatistic(log(data + 0.0001))

