# Introduction to Statistical Learning (ISLR book)
# ------------------------------------------------

# http://www.r-bloggers.com/in-depth-introduction-to-machine-learning-in-15-hours-of-expert-videos/
# http://www-bcf.usc.edu/
# https://github.com/asadoughi/stat-learning (Solutions to ISLR exercises)

library(ISLR) # Most of the datasets used
# http://cran.r-project.org/web/packages/ISLR/ISLR.pdf
library(MASS) # For one of the data sets used

set.seed(19620716)
oldpar <- par()
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)
par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot
par(mar=c(3,3,2,1))

# ---------------------------------------------------------------------------------------------------------------------
# Chapter 1, Introduction

# Income survey data for males in central Atlantic region of USA
data(Wage)
head(Wage, n=2)

plot(Wage$wage ~ Wage$age, col="gray", pch=16, xlab="Age", ylab="Wage", main="Wage by age, Atlantic region of USA")
mean.wage <- aggregate(wage ~ age, data=Wage, FUN=mean)
lines(mean.wage, lwd=2, col="steelblue4")

plot(Wage$wage ~ Wage$year, col="gray", pch=16, xlab="Year", ylab="Wage", main="Wage by year")
mean.wage <- aggregate(wage ~ year, data=Wage, FUN=mean)
lines(mean.wage, lwd=2, col="steelblue4")

# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
oldmar <- par()$mar
par(mar=c(7,4.7,2.5,1))
plot(Wage$wage ~ Wage$education, col=c("cornflowerblue", "wheat", "green3", "orange2", "blueviolet"),
     pch=21, bg="lightgray", xlab="", ylab="Wage", main="Wage by education", las=2)
par(mar=oldmar)

# Daily percentage returns for S&P (Standard & Poor) 500 over a 5-year period
data(Smarket)
head(Smarket, n=2)

plot(Smarket$Lag1 ~ Smarket$Direction, col=c("cornflowerblue", "wheat"),
     pch=21, bg="lightgray", xlab="Today's Direction", ylab="Percentage change in S&P",
     main="Stock market yesterday, Standard & Poor Index")

data(NCI60)
names(NCI60)
head(NCI60)
class(NCI60)
NCI60$data[1:10]
plot(NCI60$data, pch=1:6, col=1:6)

# -------------------------------------------------------------------------------------------------------------------
# Chapter 2, Statistical Learning

package.install("plot3D")
library(plot3D)
# http://cran.r-project.org/web/packages/plot3D/plot3D.pdf

# Not correct below...
x <- as.matrix(Wage$year)
y <- as.matrix(Wage$wage)
z <- as.matrix(Wage$age)
surf3D(x, y, z,
       colvar = y, colkey = FALSE, shade = 0.5,
       box = FALSE, theta = 60)

