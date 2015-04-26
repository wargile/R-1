# Some useful stuff....

# http://www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/
# http://onepager.togaware.com/ ("One Page R: A Survival Guide to Data Science with R")
# http://www.edii.uclm.es/~useR-2013/Tutorials/Ostrouchov.html
# http://www.mathfinance.cn/handling-large-datasets-in-R/
# http://blog.revolutionanalytics.com/2013/12/tips-on-computing-with-big-data-in-r.html

# General tip for RF, and "can't allocate vector of <N> Mb..." error message:
# "Increase the nodesize parameter to something bigger than the default, which is 5 for a regression RF."

df <- data.frame(stuff=c(5,5,8,4,3,2,7,8,8,2), morestuff=rnorm(10, 10), mynames=LETTERS[1:10])
# Get the row(s) with the min/max values

rows.min <- subset(df, df$stuff==min(df$stuff))
rows.max <- subset(df, df$stuff==max(df$stuff))

# Suppose you have two vectors, one containing the weight for an individual
# and another containing the sex as a factor (M, F).
# If you want to sort it by the median, you could use:
weight <- c(60, 66, 66, 61, 78, 81, 76, NA)
#sex <- as.factor(c("F", "F", "M", "F", "M", "M", "F", NA)) # as.factor is unecessary here
sex <- as.factor(c("F", "F", "M", "F", "M", "M", "F", NA))
ordered.factor <- reorder(sex, weight, FUN = median, na.rm=TRUE)
boxplot(weight ~ ordered.factor, col=c("orange", "cyan"), xlab="Sex", ylab="Weight", main="Weight by sex")

# There is another solution, which is to use tapply this way:
ordered.factor <- tapply(weight, sex, FUN = median, na.rm = TRUE)
ordered.factor <- sort(ordered.factor)
cat(ordered.factor)

# Math notation in plot:
x <- rnorm(100)
hist(x, xlab=expression("The mean (" * bar(x) * ") is " * sum(x[i]/n, i==1, n) ), col="orange")
x <- rnorm(100, mean=2.5)
hist(x, xlab=(bquote(bar(x) == .(round(mean(na.omit(x)), 1)))), col="cyan")

# main = substitute(paste("Heart Attack (", bar(X) == x, ")"), list(x = col_mean[[1]]))

# Sweave/Knitr, using chunks:
# http://yihui.name/knitr/demo/reference/

# Useful stuff from Assignment 3 forum, Computing for Data Analysis...
ocm <- data.frame(read.csv("C:/coding/R/Coursera/ComputingForDataAnalysis/Week 3/Assignment/outcome-of-care-measures.csv", colClasses="character"))
# table uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels = in this case sums number of hospitals by state (read ?table)
sth <- table(ocm$State)
# picks states that have over 20 hospitals
sth20 <- subset(sth, sth>=20)
# or this way:
sth20 <- sth[sth>=20]
# with this code, you get a vector of names (in this case states with > 20 hospitals)
sth20nm <- names(sth20)
# this code subsets from all data (ocm), by column State (ocm$State), picking only this states that have > 20 hospitals (sth20nm)
ocm2 <- subset(ocm, ocm$State %in% sth20nm)

# Feature scaling (machine learning): Get every feature into approx. a (-1 <= xi <= 1) range:
base.a<-c(-2,2,3,4,5,6,7,-7)
feature.a <- base.a / max(base.a)
feature.a

# Create a table in a graph format:
library(OIdata)
library(gridExtra)
data(birds)
levels(birds$effect) <- gsub(" ", "\n", levels(birds$effect))
xyTable <- table(birds$sky, birds$effect)
qplot(1:10, 1:10, geom = "blank") + theme_bw() + theme(line = element_blank(),
                                                       text = element_blank()) +
  annotation_custom(grob = tableGrob(xyTable,  
  # change font sizes:  gpar.coltext = gpar(cex = 1.2),  gpar.rowtext = gpar(cex = 1.2)),
  # xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  ))

# Create a nice 3D graph with library 'rgl':
library(rgl)
x <- rnorm(100); y <- rnorm(x); z <- rnorm(100)
plot3d(x,y,z, col=c("red", "blue", "green"), size=15)

# Matrix formula to find linear regression coefficients:
# ------------------------------------------------------
x1 <- c(100,102,103)
x1 <- rnorm(10)
x2 <- c(55,52,51)
x2 <- rnorm(10) * 2
x3 <- c(1,2,3)
x3 <- rnorm(10) * 10
y <- c(1001,1010,1111)
y <- rnorm(10) * 100 # NOTE: y must have length = nrows(M)
M <- as.matrix(data.frame(x1, x2, x3))
model <- lm(y ~ x1 + x2 + x3)
summary(model)
# Get the coefficient for y-intercept (at x=0) and the coefficients for slope:
my.slope <- model$coefficients[2] + model$coefficients[3] + model$coefficients[4]
my.intercept <- model$coefficients[1]
# Plot it with abline:
qplot(x=x1, y=y, colour=y, data=as.data.frame(M)) +
  geom_abline(intercept=my.intercept, slope=my.slope)
  
# Regression coefficients formula in Octave: pinv(M' * M) * M' * y
# Create a new matrix M1 and add a first col with 1's to matrix above
M1 <- matrix(c(rep(1,10), M[ ,1:3]), nrow=10)
solve(t(M1) %*% M1) %*% t(M1) %*% y
# Above match lm coefficients output below
model1 <- lm(y ~ M1[,2] + M1[,3] + M1[,4])
model2 <- lm(y ~ M1[,2:4]) # Works too!
summary(model1)
summary(model2)

# Try with more cols...
n <- 100
M <- as.matrix(data.frame(rnorm(n), rnorm(n) * 2, rnorm(n) * 10, rnorm(n) * -20))
y <- rnorm(n) * 3
M <- matrix(c(rep(1,n), M[ ,1:4]), nrow=n)
solve(t(M) %*% M) %*% t(M) %*% y
model <- lm(y ~ M[,-1]) # Works too!
summary(model)

# Moderation/mediation, dummy coding
# TODO: Weighted dummy coding, contrast dummy coding, mediation/moderation (StatOne Week 6/7)....
# -----------------------------------------------------------------------------------------------
df <- data.frame(condition=c("control", "control", "threat1", "threat1", "threat2", "threat2"),
                 WM=c(130, 134, 110, 113, 111, 116), IQ=c(149,156,122,126,125,128))
# Create new cols
df$D1 <- 0
df$D2 <- 0
# Create a centered variable col for IQ by subtracting mean(IQ) from IQ
df$IQ.centered <- df$IQ - mean(df$IQ)
# Assign values for dummy coding
# Assign 1 to dummy codes for threat1 (D1) and threat2 (D2)
df$D1[df$condition == "threat1"] <- 1
df$D2[df$condition == "threat2"] <- 1
df
library(psych)
describeBy(x=df, group=df$condition)
# Is there an effect of the sterotype threats?
model0 <- lm(df$IQ ~ df$D1 + df$D2)
summary(model0)
confint(model0)
# R2 = 0.96 (high). F-statistic is 25.2, not that high?
# Diff between control group and threat1 = -30.500 drop in IQ (significant)
# Diff between control group and threat2 = -16.500 drop in IQ (significant)

# Do ANOVA with aov function
model0a <- aov(df$IQ ~ df$condition)
summary(model0a)
# TODO: Is sum of squares treatment from aov above this formula: SStotal = SSerror + SStreatments ??
# TODO: F-statistic = variance between treatments / variance within treatments
# TODO: F = (MStreatments / MSerror) = (SStreatments / (I - 1)) / (SSerror / (nT - 1))
#       I = number of treatments, nT = number of cases
#       See: http://en.wikipedia.org/wiki/Analysis_of_variance
TukeyHSD(model0a)

# Check for 'first order effects' (uncentered moderation analysis):
model1 <- lm(df$IQ ~ df$WM + df$D1 + df$D2)
summary(model1)
ggplot(df, aes(x=WM, y=IQ)) + geom_smooth(method="lm") + geom_point(col="blue", size=4)
# WM has an effect on IQ

# Create new predictor variables
df$WM.D1 <- (df$WM * df$D1)
df$WM.D2 <- (df$WM * df$D2)
model2 <- lm(df$IQ ~ df$WM + df$D1 + df$D2 + df$WM.D1 + df$WM.D2)
summary(model2)
anova(model1, model2)

WM.control <- df$WM[df$condition == "control"]
IQ.control <- df$IQ[df$condition == "control"]
WM.threat1 <- df$WM[df$condition == "threat1"]
IQ.threat1 <- df$IQ[df$condition == "threat1"]
WM.threat2 <- df$WM[df$condition == "threat2"]
IQ.threat2 <- df$IQ[df$condition == "threat2"]
color <- c("red", "green", "blue")
ggplot(df, aes(x=WM, y=IQ)) + stat_smooth(method="lm", se=F) +
  geom_point(aes(color=condition))
ggplot(df, aes(x=WM, y=IQ)) + geom_smooth(aes(group=condition), method="lm", se=T, color="black", fullrange=T) +
  geom_point(aes(color=condition))

# Mediation
# ---------
# Create automatic named columns with 0/1 values for dummy variables:
folder <- "C:/coding/R/Coursera/DataAnalysis/Week 4/Assignment/"
moviesData <- read.csv(paste0(folder, "movies.txt"), header=T, sep="\t")
head(moviesData, n=2)
names(moviesData)

ratings <- unique(moviesData$rating)
ratings
offset <- ncol(moviesData)
counter <- 1

for (my.rating in ratings) {
  moviesData[offset + counter] <- 0
  moviesData[moviesData$rating == my.rating, offset + counter] <- 1
  counter <- counter + 1
}

names(moviesData)[(offset + 1):(offset + length(ratings))] <- (paste0("Dummy_", ratings))
head(moviesData, n=5)
tail(moviesData, n=5)
names(moviesData)

# Get and map Norwegian postal codes:
postal.codes <- read.csv("C:/coding/R/TestData/NorskePostnummerMedKoordinater.txt", header=T, sep="\t")

names(postal.codes)
names(postal.codes) <- c("PostalCode", "PostalPlace", "PostalCodeAndPlace", "Usage", "Population", "Township",
                         "CountyNumber", "County", "Region", "Latitude", "Longitude", "DataQuality",
                         "DataQualityExplanation", "LastUpdated")

# Strip some useless columns
cols <- c(12,13,14)
postal.codes <- postal.codes[,-cols]

head(postal.codes)
dim(postal.codes)
unique(postal.codes$Usage)

# Replace some nynorsk stuff...
postal.codes$PostalPlace <- gsub("ikkje i bruk", "not used", x=postal.codes$PostalPlace)
postal.codes$PostalCodeAndPlace <- gsub("ikkje i bruk", "not used", x=postal.codes$PostalCodeAndPlace)

postal.codes$Usage <- gsub("Postboksar", "P.o.box", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Gateadresser og postboksar", "Street addresses and p.o.boxes", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Firma/organisasjon med eige postnummer",
                           "Company/organisation with own postal code", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Gate-/veg-adresse", "Street/road address", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Gate-/veg-adresse", "Street/road address", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Fleire bruksomrede", "Several usages", x=postal.codes$Usage)
postal.codes$Usage <- gsub("Servicepostnummer", "Service postal code", x=postal.codes$Usage)

postal.codes[postal.codes$PostalCode > 3186 & postal.codes$PostalCode < 3188,]
postal.codes[postal.codes$PostalPlace %in% c("Horten", "Txnsberg"),]
postal.codes[postal.codes$County %in% c("Horten"),]

# http://www.molecularecologist.com/2012/09/making-maps-with-r/

package.install("googleVis")
library("googleVis")

package.install("mapdata")
library(maps)
library(mapdata)
m <- map("worldHires","Norway", col="gray90", fill=TRUE) #, xlim=c(-141,-53), ylim=c(40,85), col="gray90", fill=TRUE)
area.map(m, "Norway")
postal.codes.Horten <- postal.codes[postal.codes$PostalCode > 3186 & postal.codes$PostalCode < 3188,]
points(postal.codes.Horten$Lat, postal.codes.Horten$Lon, col="red", pch=19)

# Caret package, creating training and test sets automatically:
# http://www.r-bloggers.com/optimizing-probability-thresholds-for-class-imbalances/
library(caret)
library(pROC)
set.seed(442)
training <- twoClassSim(n = 1000, intercept = -16)
testing <- twoClassSim(n = 1000, intercept = -16)
table(training$Class)
set.seed(949)
mod0 <- train(Class ~ ., data = training, method = "rf", metric = "ROC", tuneGrid = data.frame(.mtry = 3),
              trControl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary))
getTrainPerf(mod0)
# (See URL above for more info/code)
result.predicted.prob <- predict(mod0, newdata=testing, type="raw")
prob.no.yes <- ifelse(result.predicted.prob == "Class1", 0, 1)
# TODO: ROC stuff below does not work....
result.roc <- roc(result.predicted.prob, testing[,-16], controls=prob.no.yes[prob.no.yes == 1],
                  cases=prob.no.yes[prob.no.yes == 0])
result.roc <- roc(result.predicted.prob, testing[,-16])

plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
# Get some more values
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)

# Create a pivot table:
genes = paste('MMP', sprintf("%04d",1:10), sep="")
data = expand.grid(gene=genes, condition=c('copper', 'cheetos', 'beer', 'pizza')) # Cool....
data$value = rnorm(40)
plot(value ~ condition, data=data, col=2:5)
cast(data, gene ~ condition)

# Animation:
package.install("animation")
package.install("ImageMagick") # Needed, but not found...
library(ImageMagick)
library(animation) # Could also use manipulate...
saveGIF({
  for(i in 1:100){
    x <- seq(0 + (i * 0.05), 3 + (i * 0.05), length= 100)
    y <- x
    f <- function(x, y) { sin(x * y) }
    z <- outer(x, y, f)
    persp(x, y, z, theta = 45, phi = 35, expand = 0.4, col = "orange")
  }
}, interval = 0.1, ani.width = 550, ani.height = 550)

# Nice correlation heatmap:
data <- airquality[,1:4]
library(ggplot2)
library(reshape2)
# NOTE: use="p", shorthand for: use="pairwise.complete.obs"
qplot(x=X1, y=X2, data=melt(cor(data, use="p")), fill=value, geom="tile") + scale_fill_gradient2(limits=c(-1, 1))

# Draw a histogram with a density curve:
BMI <- rnorm(n=1000, m=24.2, sd=2.2)
hist(BMI, freq=FALSE, xlab="Body Mass Index", main="Distribution of Body Mass Index",
     col="lightgreen", xlim=c(15,35),  ylim=c(0, .20))
curve(dnorm(x, mean=mean(BMI), sd=sd(BMI)), add=TRUE, col="darkblue", lwd=2)

# Playing around with my Normalize (tools.R) function:
# Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
pixels <- 900
imagedata <- rnorm(pixels)
imagedata.norm <- Normalize(min(imagedata), max(imagedata), 0, 255, imagedata)
image(matrix(imagedata.norm, nrow=sqrt(pixels), ncol=sqrt(pixels)), col = heat.colors(256))
par(mar=c(4.5,4.5,2,1))
par(mfrow=c(1,2))
hist(imagedata, main="imagedata", col="cornflowerblue")
hist(imagedata.norm, main="imagedata.norm", col="wheat")
par(mfrow=c(1,1))

# Creating and playing with a Design Matrix
# http://en.wikipedia.org/wiki/Design_matrix
# http://www.r-bloggers.com/using-r-barplot-with-ggplot2/
library(ggplot2)
library(plyr)
library(reshape2)
n <- 10
group <- rep(1:4, n)
mass.means <- c(10, 20, 15, 30)
mass.sigma <- 4
score.means <- c(5, 5, 7, 4)
score.sigma <- 3
mass <- as.vector(model.matrix(~0+factor(group)) %*% mass.means) + rnorm(n*4, 0, mass.sigma)
score <- as.vector(model.matrix(~0+factor(group)) %*% score.means) + rnorm(n*4, 0, score.sigma)
data <- data.frame(id=1:(n*4), group, mass, score)

melted <- melt(data, id.vars=c("id", "group"))
means <- ddply(melted, c("group", "variable"), summarise, mean=mean(value))

means.sem <- ddply(melted, c("group", "variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)

means.barplot <- qplot(x=group, y=mean, fill=variable, data=means, geom="bar", stat="identity", position="dodge")
means.barplot + geom_errorbar(aes(ymax=upper, ymin=lower), position=position_dodge(0.9), data=means.sem) +
  ggtitle("Testing barplot with errorbars...")

# Test stripchart:
y <- sample(0:20, 100, replace=T)
stripchart(y, method="stack", offset=.5, at=.05, pch=19, col="blue", main="Dot Plot Example",
           xlab="Random Y Sample", frame.plot=F)

# Random walk:
# http://www.r-bloggers.com/random-love/
state <- cumsum(sample(c(-1, 1), 100, replace = TRUE))
plot(state, main="Random Walk", xlab="Steps", ylab="Where are we")
lines(state)

# kmeans clustering, and finding the "knee" in a kmeans curve:
data <- as.matrix(rnorm(20) * 100)
km <- kmeans(data, 5, 10)
km
#plot(data, pch=21, bg=km$cluster + 1)
n <- 15
ssq <- numeric(n)
for (i in 1:n) ssq[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:n, ssq, type="b", pch=21, bg="cyan", lwd=1, xlab="Number of clusters",
     ylab="Sum of squares within group",
     main="kmeans clusters", col="blue", cex.axis=.8)
# Find 'knee' in curve: (http://people.cs.umass.edu/~irwin/simplex.pdf)
# Nice!
straight_line <- seq(ssq[1], ssq[n], length.out=n)
knee <- which.max(straight_line - ssq)
abline(v=knee, col="red", lty=3)
lines(straight_line, col="blue", type="b")

# Nice Fractals:
# http://aschinchon.wordpress.com/2014/03/27/blurry-fractals/
library(ggplot2)
package.install("numDeriv")
library(numDeriv)
library(RColorBrewer)
library(gridExtra)
## Polynom: choose only one or try yourself
f  <- function (z) {z^3-1}        #Blurry 1
#f  <- function (z) {z^4+z-1}     #Blurry 2
#f  <- function (z) {z^5+z^3+z-1} #Blurry 3
z <- outer(seq(-2, 2, by = 0.01), 1i*seq(-2, 2, by = 0.01), '+')
for (k in 1:5) z <- z-f(z)/matrix(grad(f, z), nrow=nrow(z))
## Supressing texts, titles, ticks, background and legend.
opt <- theme(legend.position="none",
             panel.background = element_blank(),
             axis.ticks=element_blank(), 
             axis.title=element_blank(), 
             axis.text =element_blank())
z <- data.frame(expand.grid(x=seq(ncol(z)), y=seq(nrow(z))), z=as.vector(exp(-Mod(f(z)))))
# Create plots. Choose a palette with display.brewer.all()
p1 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(8, "Paired")) + opt
p2 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(7, "Paired")) + opt
p3 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(6, "Paired")) + opt
p4 <- ggplot(z, aes(x=x, y=y, color=z)) + geom_tile() + scale_colour_gradientn(colours=brewer.pal(5, "Paired")) + opt
# Arrange four plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol=2)

# Simple linspace function. Same as: seq(minval, maxval, length.out=elements)
linspace <- function(minval, maxval, elements=2) {
  if (elements <= 0) {
    return (integer(0))
  }
  
  if (elements == 1) {
    return (minval)
  }
  
  result <- numeric(elements)
  step <- (maxval - minval) / (elements - 1) # Creates positive or negative step
  result[1] <- minval
  
  for (counter in 2:elements) {
    result[counter] <- result[counter - 1] + step
  }
  
  return (result)
}

# Sigma Scaling:
# Example: plot(G(1, .5, c(.15, .1, .05, .01)), type="l")
# TODO: Try in Mathematica:
# https://reference.wolfram.com/mathematica/tutorial/ImageProcessing.html
G <- function(x, mu, sigma) {
  e <- exp(1)
  return (1 / (1 + e^(-((x - mu) / sigma))))
}

# Upgrade R:
if(!require(installr)) { 
  install.packages("installr")
  require(installr)
}
updateR(to_checkMD5sums = FALSE)

# Install swirl:
install.packages("swirl")  # Installs swirl
library(swirl)  # Loads swirl
swirl()  # Runs swirl
# For help: help.start()

# Example of how to embed another function in a parent function. The embedded function's
# parent environment is then the parent function itself
make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  
  pow
}
# Test it:
cube <- make.power(3)
square <- make.power(2)
cube(10)
square(10)
ls(environment(cube))
ls(environment(square))
get("n", environment(cube))
get("n", environment(square))


# Some subsetting and ordering of data frames:
df <- data.frame(x=sample(1:5), y=sample(11:15), z=NA)
df$z[c(1,3)] <- c(22,33)
df[which(df$z > 0), c("x", "y")]
sort(df$z, na.last=T)
sort(c(df$x, df$y), decreasing=F)
df[sort(df$x, decreasing=T),]
df[order(df$y, df$x),]
library(plyr)
arrange(df, desc(x))
df$var4 <- rnorm(nrow(df))

# Summarizing data:
if (!file.exists("./TestData")) { dir.create("./TestData")}
fileUrl <- "http://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile="./TestData/BaltimoreRestaurants.csv")
restData <- read.csv("./TestData/BaltimoreRestaurants.csv")
summary(restData) # Always good to do! Easy to see weird values, etc.
str(restData) # Always good to do. Quick info on datatypes, etc.
quantile(restData$councilDistrict, na.rm=T)
quantile(restData$councilDistrict, na.rm=T, probs=c(.25, .5, .9))
table(restData$zipCode, useNA="ifany")
table(restData$councilDistrict, restData$zipCode, useNA="ifany")
sum(is.na(restData$councilDistrict)) # NOTE: Use sum to get number of na's!
sum(!is.na(restData$councilDistrict)) # NOTE: Use sum to get number of NOT na's!
all(restData$zipCode > 0) # Check if all elements has a certain value/range
all(restData$zipCode < 0 | restData$zipCode > 0) # Check if all elements has a certain value/range
table(restData$zipCode %in% c("21212", "21213"))
restData[restData$zipCode %in% c("21212", "21213"),]
data(UCBAdmissions)
class(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
summary(DF)
x <- xtabs(Freq ~ Gender + Admit, data=DF)
x
warpbreaks$replicate <- rep(1:9, len=54)
xt <- xtabs(breaks ~ ., data=warpbreaks)
xt
ftable(xt)
fakeData <- rnorm(1e5) # Create a big data set
object.size(fakeData) # Get the size
print(object.size(fakeData), units="Mb")
# Easier cutting:
library(Hmisc)
restData$zipGroups <- cut2(restData$zipCode, g=4)
table(restData$zipGroups)

# Distance between two points on the real line:
x <- 5
y <- -12
sqrt((x - y)^2)
# Calculating Euclidian distance:
# http://en.wikipedia.org/wiki/Euclidean_distance
a <- c(0, 0)
b <- c(6, 6)
sqrt((a - b) %*% (a - b))
a <- c(1, 2, 3, 4)
b <- c(2, 4, 6, 8)
sqrt((a - b) %*% (a - b))
# or:
sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2 + (a[3] - b[3])^2 + (a[4] - b[4])^2)
# or just:
sqrt(sum((a - b)^2))
# Calculating Manhattan/taxicab distance:
# http://en.wikipedia.org/wiki/Manhattan_distance
x1 <- c(0, 0)
x2 <- c(6, 6)
sum(abs(x1 - x2))

# Multiple hypothesis testing, probability correction (Bonferroni and Sidak):
par(mfrow=c(1,2))
a <- 0.05
k <- 1:100
prob.acc <- 1 - (1 - a)^k
plot(prob.acc, col="blue", type="l", main="Multiple hypothesis testing",
     cex.main=.8, cex.lab=.8, cex.axis=.8, ylab="Probability", xlab="Hyphotesis tests")
bonferroni <- 1 - (1 - (a/k))^k
sidak <- 1 - (1 - (1 - (1 - a)^(1/k)))^tests
plot(bonferroni, col="green4", type="l", main="Bonferroni and Sidak corrections",
     cex.main=.8, cex.lab=.8, cex.axis=.8, ylab="Probability", xlab="Hyphotesis tests")
lines(sidak, col="red", type="l")
legend("right", legend=c("Bonferroni", "Sidak"), col=c("green4", "red"), lwd=2, cex=.7)
par(mfrow=c(1,1))

# Entropy - measure of uncertainity, used in ML decision trees branching, etc.:
# (http://en.wikipedia.org/wiki/Entropy_(information_theory))
# See: C:\coding\R\Coursera\IntroductionToDataScience\Week 6\Videos, 6 - Information Gain
sides.coin <- 2
prob.fair.coin <- 1/sides.coin
sides.die <- 6
prob.fair.die <- 1/sides.die
entropy.coin.flip <- -((prob.fair.coin * log2(prob.fair.coin)) * sides.coin)
entropy.coin.flip # 1
entropy.die.toss <- -((prob.fair.die * log2(prob.fair.die)) * sides.die)
entropy.die.toss # 2.58
# Example: Entropy for a weighted die (prob 0.1 for roling 1 to 5, and prob 0.5 for rolling a 6):
-((0.1 * log2(0.1)) * 5) + -((0.5 * log2(0.5)) * 1) # 2.16
# We see that a weighted (not fair) die is LESS unpredictable than a fair die

# A data frame example:
rainy <- "rainy"
overcast <- "overcast"
sunny <- "sunny"
cool <- "cool"
mild <- "mild"
hot <- "hot"
normal <- "normal"
high <- "high"
no <- "no"
yes <- "yes"

playing.outside <- data.frame(
  outlook=c(overcast,overcast,overcast,overcast,rainy,rainy,rainy,rainy,rainy,sunny,sunny,sunny,sunny,sunny),
  temperature=c(cool,hot,hot,mild,cool,mild,cool,mild,mild,hot,hot,mild,cool,mild),
  humidity=c(normal,high,normal,high,normal,high,normal,high,normal,high,high,high,normal,normal),
  windy=c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
  play=c(yes,yes,yes,yes,no,no,yes,yes,yes,no,no,no,yes,yes))

playing.outside

rows <- nrow(playing.outside)

play.yes <- length(playing.outside$play[playing.outside$play == yes])
play.no <- rows  - play.yes
entropy.before <- -((play.yes/rows * log2(play.yes/rows)) + (play.no/rows * log2(play.no/rows)))
round(entropy.before, 2) # 0.94

# Which predictor column gives the highest entropy (info gain) related to the outcome column 'play'?
# Higher value is higher uncertainty. 0 = no undertainity.
# Testing on column 'outlook':
outlook.total <- table(playing.outside$outlook)
outlook.yes <- table(playing.outside$outlook[playing.outside$play == yes])
outlook.no <- table(playing.outside$outlook[playing.outside$play == no])

# outlook.no = 0, so skip in calculation:
overcast.entropy <- -((outlook.yes[1] / outlook.total[1]) * log2(outlook.yes[1] / outlook.total[1]))
round(overcast.entropy, 2)
rainy.entropy <- -(((outlook.yes[2] / outlook.total[2]) * log2(outlook.yes[2] / outlook.total[2])) +
  ((outlook.no[2] / outlook.total[2]) * log2(outlook.no[2] / outlook.total[2])))
round(rainy.entropy, 2)
sunny.entropy <- -(((outlook.yes[3] / outlook.total[3]) * log2(outlook.yes[3] / outlook.total[3])) +
                     ((outlook.no[3] / outlook.total[3]) * log2(outlook.no[3] / outlook.total[3])))
round(sunny.entropy, 2)

new.outlook.entropy <- ((outlook.total[1] / rows) * overcast.entropy) +
  ((outlook.total[2] / rows) * rainy.entropy) +
  ((outlook.total[3] / rows) * sunny.entropy)
names(new.outlook.entropy) <- "Outlook entropy"
round(new.outlook.entropy, 2)
gain <- entropy.before - new.outlook.entropy
round(gain, 3)

# Testing on column 'humidity':
humidity.total <- table(playing.outside$humidity)
humidity.yes <- table(playing.outside$humidity[playing.outside$play == yes])
humidity.no <- table(playing.outside$humidity[playing.outside$play == no])

high.entropy <- -(((humidity.yes[1] / humidity.total[1]) * log2(humidity.yes[1] / humidity.total[1])) +
                    ((humidity.no[1] / humidity.total[1]) * log2(humidity.no[1] / humidity.total[1])))
# -(((3/7)*log2(3/7)) + ((4/7)*log2(4/7)))
round(high.entropy, 2)
normal.entropy <- -(((humidity.yes[2] / humidity.total[2]) * log2(humidity.yes[2] / humidity.total[2])) +
                      ((humidity.no[2] / humidity.total[2]) * log2(humidity.no[2] / humidity.total[2])))
# -(((6/7)*log2(6/7)) + ((1/7)*log2(1/7)))
round(normal.entropy, 2)

new.humidity.entropy <- ((humidity.total[1] / rows) * high.entropy) +
  ((humidity.total[2] / rows) * normal.entropy)
names(new.humidity.entropy) <- "Humidity entropy"
round(new.humidity.entropy, 2)
gain <- entropy.before - new.humidity.entropy
round(gain, 3)

# Test it:
library(tree)
myTree <- tree(as.factor(play) ~ ., data=playing.outside, split="deviance")
plot(myTree)
title(main="Decision Tree")
text(myTree, cex=.8)
summary(myTree)
# Selects Humidity col, Misclassification error rate: 0.285714285714 = 4 / 14
# But outlook has a slightly higher gain using mehod above....

playing.outside$windy <- as.factor(playing.outside$windy)
fit <- glm(play ~., data=playing.outside, family=binomial(link="logit"))
summary(fit)

# Do a LOOCV on training set playing.outside:
playing.outside$play <- as.factor(playing.outside$play)
playing.outside$windy <- as.factor(playing.outside$windy)
playing.outside$humidity <- as.factor(playing.outside$humidity)
playing.outside$temperature <- as.factor(playing.outside$temperature)
playing.outside$outlook <- as.factor(playing.outside$outlook)

rows <- nrow(playing.outside)
acc <- numeric(0)
prediction <- numeric(0)
for (counter in 1:rows) {
  fit1 <- glm(play ~ outlook + temperature + humidity + windy,
              data=playing.outside[-counter, ], family=binomial(link="logit"))
  #summary(fit1)
  pred <- predict(fit1, newdata=playing.outside[counter, ], type="response")
  prediction[counter] <- ifelse(pred <= 0.5, "no", "yes")
  acc[counter] <- ifelse(pred == playing.outside[counter, 5], 1, 0)
}
acc
table(playing.outside$play, prediction)
ConfusionMatrix2(playing.outside$play, prediction, c("no", "yes"))

# -----------------------------------------------------------------------------------------------------------------
# Creating and interpreting a qqplot:
# http://stats.stackexchange.com/questions/111010/interpreting-qqplot-is-there-any-rule-of-thumb-to-decide-for-non-normality?
# newsletter=1&nlcode=329536%7c673f
x <- rnorm(80)
y <- x * rnorm(80)
qqplot(x, y, col="blue", cex.lab=.8, cex.axis=.8, pch=21, bg="cyan", main="QQPlot")
# Or use another package:
package.install("qualityTools")
library(qualityTools)
qqPlot(x, col="blue", cex.lab=.8, cex.axis=.8, pch=21, bg="cyan", main="QQPlot",
       bounds.lty=1, bounds.col="gray", border="violetred")

# ---------------------------------------------------------------------------------------------------------------
# Using dplyr on the Titanic data set
library(dplyr)
set.seed(16071962)

folderData <- "C:/coding/Kaggle/Titanic/R/data/"
train <- read.csv(paste0(folderData, "train.csv"), header=T, sep=",", stringsAsFactors=F)
head(train, n=2)
my.vars <- select(train, Survived, Sex, Pclass, Age)
head(my.vars, n=2)
# OR select all cols from Survived to Age:
my.vars <- select(train, Survived:Age)
head(my.vars, n=2)
FirstClass <- filter(train, Pclass == 1)
train <- arrange(train, Fare, Pclass) # TODO: Order by Desc is probably also possible
head(train, n=5)

# ----------------------------------------------------------------------------------------------------------------
# Detecting colinearity in predictor variables:
# http://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
# See also: http://www.statmethods.net/stats/rdiagnostics.html
# See also: http://socserv.socsci.mcmaster.ca/jfox/Courses/Brazil-2009/slides-handout.pdf
variance.inflation.factor <- function(model) {
  r.squared <- summary(model)$r.squared
  term.labels <- attr(model$terms, "term.labels")
  vif.result <- (1 / (1 - r.squared))
  result <- numeric(length(term.labels))
  names(result) <- term.labels
  return (sapply(result, function(x) result[x] <- vif.result))
}
# Regress the variable in question against all other predictor variables
x1 <- rnorm(100)
x2 <- x1 + rnorm(100)
x3 <- x2 * x1 + rnorm(100)
y <- x1 * x2 * x3 + rnorm(100)

fit <- lm(x1 ~ x2 + x3)
summary(fit)$r.squared
variance.inflation.factor(fit)
# Test it against a library function:
library(car)
vif(fit) # Not the same result...

# ----------------------------------------------------------------------------------------------------------------
# Signal smoothing with derivatives (Use the local minimum for each derivative along the curve):
# http://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter
# http://en.wikipedia.org/wiki/Local_regression
# http://arxiv.org/pdf/1006.3342.pdf
# http://www.wire.tu-bs.de/OLDWEB/mameyer/cmr/savgol.pdf
# http://wiki.scipy.org/Cookbook/SavitzkyGolay
# http://en.wikipedia.org/wiki/Convolution
# http://www.ece.rice.edu/~fk1/classes/ELEC697/Lec_11_KernelMethods.pdf
# http://ttic.uchicago.edu/~gregory/courses/LargeScaleLearning/lectures/lwr.pdf
x <- seq(from=-pi*2, to=pi*2, by=.1)
f <- 1/2*x^3 * cos(12*x^2)
f <- 1/2*x^3 * 12*x^2
plot(f, type="l", col="blue")
#curve(1/2*x^3 * cos(12*x^2), from=-pi*2, to=pi*2, col="red")
lines((t(x) %*% x)^(-1) * x, col="green")
for (counter in 1:(length(x)-10)) {
  window <- x[counter:(counter + 10)]
  #points(eval(D(expression(1/2*x^3 * cos(12*x^2)), 'x')), col="red")
  points((window - mean(window))^2, col="red")
}

# Just playing around....
x <- seq(-10,10,by=1)^2
n <- length(x)
poly <- (sum((diff(x[1:n])-mean(diff(x[2:n+1])))^2)) / n
poly
plot(x, col="blue")
# WMA:
smoother <- numeric(0) 
for (counter in 1:(n-4)) {
  smoother <- c(smoother, mean(x[counter:counter+4]))
}
points(smoother, col="red", type="o")

diag(x)

# ----------------------------------------------------------------------------------------------------------------
# TODO: Fit a MARS model (MULTIVARIATE ADAPTIVE REGRESSION SPLINES MODEL with mda package)
# http://www.r-bloggers.com/fit-and-visualize-a-mars-model/
# Good to ensemble with the Lasso model for sparse outcome variable (Fire Peril Loss competition...)

# ----------------------------------------------------------------------------------------------------------------
# TODO: Convay's Game of Life:
# http://www.r-bloggers.com/looking-for-life/
# http://en.wikipedia.org/wiki/Distance_matrix
# http://en.wikipedia.org/wiki/Similarity_matrix
# http://en.wikipedia.org/wiki/Euclidean_distance_matrix
# http://en.wikipedia.org/wiki/Adjacency_matrix
# www.cs.cmu.edu/~tcortina/activate/ct/Recursion.ppt

# See also: igraph Forest Fire Game: http://cran.r-project.org/web/packages/igraph/igraph.pdf, page 94
library(igraph)
nodes <- 100
fwprob <- .5
ambs <- 1
f <- forest.fire.game(nodes, fw.prob=fwprob, bw.factor=1, ambs, directed=TRUE)
plot(f)

# ----------------------------------------------------------------------------------------------------------------
# TODO: Lifecycle graphs/matrix:
# http://www.r-bloggers.com/periodic-matrix-model-for-annual-plant-demography/

# ----------------------------------------------------------------------------------------------------------------
# TODO: Using ChiSquare test with categorical variables:
# http://www.r-bloggers.com/the-chi-squared-test-of-independence-an-example-in-both-r-and-sas/
# http://chemicalstatistician.wordpress.com/category/r-programming/
# http://chemicalstatistician.wordpress.com/category/categorical-data-analysis/
# Video examples here:
# http://chemicalstatistician.wordpress.com/2014/07/07/video-tutorial-calculating-expected-counts-in-contingency-tables-using-marginal-proportions-and-marginal-totals/
# http://chemicalstatistician.wordpress.com/2014/08/04/calculating-expected-counts-in-a-contingency-table-using-joint-probabilities/

# ----------------------------------------------------------------------------------------------------------------
# TODO: Markov Chains
# http://en.wikipedia.org/wiki/Markov_chain

# ----------------------------------------------------------------------------------------------------------------
# TODO: Monte Carlo method and Monte Carlo integration
# http://en.wikipedia.org/wiki/Monte_Carlo_method
# http://en.wikipedia.org/wiki/Monte_Carlo_integration

# ----------------------------------------------------------------------------------------------------------------
# TODO: Markov Chain Monte Carlo
# http://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo

# ----------------------------------------------------------------------------------------------------------------
# TODO: Kernel functions

# ----------------------------------------------------------------------------------------------------------------
# TODO: Sweep line algorithm and Voronoi diagrams
# http://en.wikipedia.org/wiki/Sweep_line_algorithm
# http://www.ams.org/samplings/feature-column/fcarc-voronoi

# ----------------------------------------------------------------------------------------------------------------
# TODO: Big O notation
# http://en.wikipedia.org/wiki/Big_O_notation
# http://stackoverflow.com/questions/487258/plain-english-explanation-of-big-o

# ----------------------------------------------------------------------------------------------------------------

# Some examples of using lists....

ngram1 <- MyNgrams("This is just a test! Yes, indeed!", 4)
ngram2 <- MyNgrams("Here is just another test! Yes, indeed, it is! Isn't it?", 5)
mycollection <- list()
mycollection[[1]] <- ngram1
mycollection[[2]] <- ngram2
length(mycollection)
names(mycollection) <- c("Ngram1","Ngram2")

if (length(which((names(mycollection) %in% c("Ngram2")) == T) > 0))
  mycollection[[which(names(mycollection) %in% c("Ngram2") == TRUE)]]

is.null(mycollection[["Ngram2"]]) # TRUE
is.null(mycollection[["Ngram22"]]) # FALSE

searchTerm <- "Ngram1"
mycollection[[which(names(mycollection) %in% searchTerm == TRUE)]]
# or simply:
mycollection[[searchTerm]]

# -------------------------------------------------------------------------------------------------------------------
# Random Walk example
n <- 250
start.y <- 0
random.walks <- list()
the.min <- Inf
the.max <- -Inf

for (iteration in 1:5) {
  random.walk <- rep(0, n)
  random.walk[1] <- start.y

  for (counter in 2:n) {
    random.walk[counter] <- random.walk[counter - 1] + ifelse(sample(0:1, 1) == 0, 1, -1)
  }
  
  if (min(random.walk) < the.min) the.min <- min(random.walk)
  if (max(random.walk) > the.max) the.max <- max(random.walk)
  
  random.walks[[iteration]] <- random.walk
  
}

plot(random.walks[[1]], type="l", col="black", main="'Drunkard's Walk' (Random Walk)",
     ylab="Random Walk", xlab="Steps", cex.lab=.8, cex.main=1, cex.axis=.8,
     ylim=c(the.min-1, the.max+1))

for (counter in 2:length(random.walks)) {
  lines(random.walks[[counter]], col=counter)
}

# ------------------------------------------------------------------------------------------------------------------
# Get primes for a number
GetPrimeDivisors <- function(number) {
  primes <- integer(0)
  is.prime <- F

  if (number < 2)
    return (primes)

  if (number == 2)
    return (2)
  
  for (counter1 in 2:(number - 1)) {
    is.prime <- T
    
    for (counter2 in seq((counter1 - 1), 1, -1)) {
      if (counter1 %% counter2 == 0 & (counter2 > 1)) {
        is.prime <- F
        break
      }
    }
  
    if ((is.prime == T) & ((number %% counter1) == 0))
      primes <- c(primes, counter1)
  }
  
  return (primes)
}

sapply(c(15, 21, 24, 30, 49), function(x) GetPrimeDivisors(x))
sapply(c(2,3,5,7,11,13,17,19,23,29,31,37,41), function(x) GetPrimeDivisors(x))
