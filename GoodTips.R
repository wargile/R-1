# Wald test for logistic regression
# When is a z- and t-value used?
# http://stats.stackexchange.com/questions/60074/wald-test-for-logistic-regression
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)
my.mod <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(my.mod)
summary(lm(Fertility~., data=swiss))

# Sobel test - test wether a mediator variable has any influence:
package.install("bda") # For mediation.test
library(bda)
mv = rnorm(100)
iv = rnorm(100)
dv = rnorm(100)
mediation.test(mv,iv,dv)
sobel(iv,mv,dv)

# Tutorial, data wrangling in R:
# http://www.computerworld.com/s/article/9243391/4_data_wrangling_tasks_in_R_for_advanced_beginners?taxonomyId=9&pageNumber=1

# Package tree:
data(Cars93, package="MASS")
treeCars <-tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + Width + Length + Weight +
                 Price + Cylinders + Horsepower + Wheelbase, data=Cars93)
plot(treeCars)
text(treeCars, cex=.8)
par(mfrow=c(1,2))
plot(cv.tree(treeCars, FUN=prune.tree, method="misclass"))
plot(cv.tree(treeCars))
par(mfrow=c(1,1))

# Examples below from: http://gastonsanchez.wordpress.com/

# Dendrograms in various forms:
# http://gastonsanchez.wordpress.com/2012/10/03/7-ways-to-plot-dendrograms-in-r/ 
# prepare hierarchical cluster
hc = hclust(dist(mtcars))
# very simple dendrogram
plot(hc, cex=.8)
# labels at the same level
plot(hc, hang=-1, cex=.8)

## tweeking some parameters for plotting a dendrogram
# set background color
op = par(bg="#DDE3CA")
# plot dendrogram
plot(hc, col="#487AA1", col.main="#45ADA8", col.lab="#7C8071",
     col.axis="#F38630", lwd=3, lty=3, sub='', hang=-1, axes=FALSE)
# add axis
axis(side=2, at=seq(0, 400, 100), col="#F38630",
     labels=FALSE, lwd=2)
# add text in margin
mtext(seq(0, 400, 100), side=2, at=seq(0, 400, 100),
      line=1, col="#A38630", las=2)
par(op)

# using dendrogram objects
hcd = as.dendrogram(hc)
# alternative way to get a dendrogram
op = par(mfrow = c(2,1))
plot(hcd)
# triangular dendrogram
plot(hcd, type="triangle")
par(op)

# plot dendrogram with some cuts
op = par(mfrow = c(2,1))
plot(cut(hcd, h=75)$upper,
     main="Upper tree of cut at h=75")
plot(cut(hcd, h=75)$lower[[2]],
     main="Second branch of lower tree with cut at h=75")
par(op)

# vector of colors
labelColors = c("#CDB380", "#036564", "#EB6841", "#EDC951")
# cut dendrogram in 4 clusters
clusMember = cutree(hc, 4)
# function to get color labels (TODO: What's happened here??)
colLab {
  if(is.leaf(n)) {
    a     labCol     attr(n, "nodePar")   }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
plot(clusDendro, main = "Cool Dendrogram", type = "triangle")

# phylogenetic trees
# load package ape;
# remember to install it: install.packages("ape")
package.install("ape")
library(ape)
# plot basic tree
plot(as.phylo(hc), cex=0.9, label.offset=1)
# cladogram
plot(as.phylo(hc), type="cladogram", cex=0.9, label.offset=1)
# unrooted
plot(as.phylo(hc), type="unrooted")
# fan
plot(as.phylo(hc), type="fan")
# radial
plot(as.phylo(hc), type="radial")

# vector of colors
mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
# cutting dendrogram in 5 clusters
clus5 = cutree(hc, 5)
# plot
op = par(bg="#E8DDCB")
# Size reflects miles per gallon
plot(as.phylo(hc), type="fan", tip.color=mypal[clus5], label.offset=1,
     cex=log(mtcars$mpg,10), col="red")
par(op)

# Using ggdendro
# remember to install the package: install.packages("ggdendro")
library(ggplot2)
package.install("ggdendro")
library(ggdendro)
# basic option
ggdendrogram(hc, theme_dendro=FALSE)
# another option
ggdendrogram(hc, rotate=TRUE, size=4, theme_dendro=FALSE, color="tomato")

# Colored dendrogram
# load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
# colored dendrogram
op = par(bg="#EFEFEF")
A2Rplot(hc, k=3, boxes = FALSE,
        col.up = "gray50", col.down = c("#FF6B6B","#4ECDC4","#556270"))
par(op)

# Create a "Matrix" movie graph:
x_num = 100
y_num = 80

# x-axis locations in random order
x = sample(x=1:x_num, size=90, replace=TRUE)
# y-axis locations (from -1 to -80)
y = seq(-1, -y_num, length=90)

# set graphical parameters
op = par(bg="black", mar=c(0, 0.2, 0, 0.2))

# plotting window
plot(1:x_num, seq(-1,-x_num), type="n",
     xlim = c(1,x_num), ylim = c(-y_num+10,0))
# plot letters
for (i in seq_along(x))
{
  # sample to get vertical length
  aux = sample(1:y_num, 1)
  # x and y coordinates
  x_coords = rep(x[i],aux)
  y_coords  = y[1:aux]
  # add characters of different size and hues
  points(x_coords, y_coords,
         pch = sample(c(letters, toupper(letters)), aux, replace=TRUE),
         col = hsv(0.35, runif(aux,0.25,0.9), 1, runif(aux,.3)),
         cex = runif(aux,.3))
}

# reset graphical parameters
par(op)


# Using Random Forest for prediction
library(randomForest)
set.seed(1)
data(iris)
iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE, keep.forest=TRUE)
x <- MDSplot(iris.rf, iris$Species)
# Add a legend
legend("topleft", legend=levels(iris.rf$predicted), cex=.8,
       fill=brewer.pal(length(levels(iris.rf$predicted)), "Set1"))
# Just in case you need to identify points?
text(x$points,labels=attr(x$points,"dimnames")[[1]], cex=0.5)


# Using Spatial Data
# http://www.r-bloggers.com/introduction-to-spatial-data-and-ggplot2/

# TODO: Look at "finite impulse response filter" model for prediction:
# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/7574/shall-we-start-discussing-the-ideas/41420#post41420
# http://en.wikipedia.org/wiki/Finite_impulse_response
# Not sure if below is correctly understood...
N <- 10
i <- 0:N
n <- N + 1
x <- (1:N+1)*10
b <- abs(rnorm(N + 1))*10
# b <- -x
FIR.filter <- numeric(N + 1)
result <- sapply(i, function(counter) FIR.filter[counter + 1] <- sum(b[counter] * x[(n - counter)]))  
par(mfrow=c(1,2))
plot(b, col="red", type="b")
plot(result, type="b", col="blue")
par(mfrow=c(1,1))

# Find out which col classes a file has, for faster read into R:
dataFolder <- "C:/coding/Kaggle/AllstatePurchasePredictionChallenge/data/"
initial <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", nrows=2)
classes <- sapply(initial, class)
Sys.time()
full <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", colClasses=classes)
Sys.time()
full <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",")
Sys.time()
head(full)


  
