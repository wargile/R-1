# R links:
# https://class.coursera.org/stats1-002/forum/thread?thread_id=81

# Random stuff....
# This is a section ----
rm(stars)

my.file <- "C:\\tools\\utils\\sqlite\\starsdata\\hygxyz.csv"

if (file.exists(my.file))
  stars <- read.csv(my.file)

#viewData(stars "Balleklorin")
stars[1:2,1:2]
max(stars[1:13,4])
length(stars[1:13,4])
ls()

# Adjust how many rows get printed to screen
options(max.print=10000)
getOption("max.print")

remove(myArray)
myArray = array(1:10)

# This is another section ----
# Use manipulate package/library: http://www.rstudio.com/ide/docs/advanced/manipulate
require(graphics)
library(manipulate)
palette(rainbow(6))
manipulate(plot((1:x*y)^y, type="h", main="My first plot!", xlab="Resolution",
                ylab="Rise/fall", col=1:5), x = slider(1, 100), y = slider(1, 10))

barplot(as.matrix(1:10), beside=TRUE)

manipulate(barplot(as.matrix(1:x*y)^y, beside=TRUE, main="My first plot!", xlab="Resolution",
                   ylab="Rise/fall"), x = slider(1, 100), y = slider(1, 10))

# Install libraries and use them:
# install.packages("ggplot2", dependencies = TRUE)
# library(ggplot2)

# options(repos=c(RStudio='http://rstudio.org/_packages', getOption('repos')))
# install.packages('shiny')

# Call with foo() for default values, or foo(16, 166) for using input values
remove(foo) # or: rm(foo)

plot(sample.int(10))
sample(LETTERS, 10)

# This is a manipulate function ----
foo <- function(maxX=5, maxY=5) {
  grid()
  par(bg="cornsilk")
  box
  #axis(2, col.axis="blue", las=1)
  
  manipulate(
    barplot(as.matrix(1:x*y)^y, beside = TRUE, main = "My test plot!", xlab = "Resolution",
            ylab = "Rise/fall"), x = slider(1, maxX), y = slider(1, maxY)
  )
}

# Using factor ----
gender <- c(1,2,1,1,2,2,1,2)
names<-c("Harry","Dolly","Scoobie","Doobie","Brothers","Cher","Blair","Topsy")
mypeeps = data.frame(gender,names)
# Add a better description to numeric values
mypeeps$gender <- factor(mypeeps$gender, levels = c(1,2), labels = c("male", "female"))

x = c(1,1,1,10,6,1,5,4,2)
unique.and.sorted <- sort(unique(x))


# Legends with data set example
df1 <- data.frame(sex        = factor(c("Female","Female","Male","Male")),
                  time       = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
                  total_bill = c(13.53, 16.81, 16.24, 17.42))

# Reading csv file, using ggplot with text ----
# A basic graph
lp <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, shape=sex)) + geom_line() + geom_point(size=3)
lp

# Change the legend
lp + scale_shape_discrete(name  ="The Payer",
                          breaks=c("Female", "Male"),
                          labels=c("Woman", "Man"))


DoSomeBarPlotStuff <- function() {
  x <- matrix(
    c(
      200, 227, 196, 
      210, 279, 319, 
      220, 126, 111,
      230, 196, 123,
      240, 106, 94,
      250, 154, 233,
      260, 226, 218
    ),
    nrow = 3,
    ncol = 7
  )

  colnames(x) <- month.name[c(11:12, 1:5)]
  rownames(x) <- c("Horten", "Oslo", "Spring Grove")

  #par(mar = c(5, 4, 1.5, 0.5), ps = 12, cex  = 1, cex.main = 2, las = 1)

  barplot(
    x, 
    beside      = TRUE, 
    ylim        = c(0,350),
    xlab        = "Month", 
    axes        = TRUE,
    axis.lty    = 1, 
    ylab        = "Monthly Precipitation [mm]",
    col         = c("darkblue", "dodgerblue3", "deepskyblue1"),
    panel.first =  abline(
      h    =  seq.int(50, 300, 50), 
      col  =  "grey", 
      lty  =  2
    )
  )

  box()
}

DoMorePlotStuff <- function() {
  dfn <- read.table(header=T, text=
  '
    supp dose length
      OJ  0.5  13.23
      OJ  1.0  22.70
      OJ  2.0  26.06
      VC  0.5   7.98
      VC  1.0  16.77
      VC  2.0  26.14
  ')
  
  ggplot(data=dfn, aes(x=dose, y=length, group=supp, colour=supp)) + geom_line() + geom_point()
}

DoSomeMatrixStuff <- function() {
  a <- matrix(1:12)
  dim(a) <- c(3,4)
  dim(a)
  rownames(a) <- c(LETTERS[1:nrow(a)])
  colnames(a) <- c(paste("Col", LETTERS[1:ncol(a)]))
  complete.cases(a) # Returns TRUE TRUE TRUE because no NA values in rows
  a[,2] <- NA
  complete.cases(a) # Returns FALSE FALSE FALSE because NA values in all rows
  #a[,!complete.cases(a)] <- 99
  a
  b<-subset(a, a[,"Col D"]>=11)
  b
}

DoSomeApplyStuff <- function() {
  # Create a lit with a couple of matrices
  x <- list(a=matrix(1:4, 2, 2), b=matrix(1:6, 3, 2))
  # Use lapply with an ANONYMOUS function to extract
  # the second column from each matric
  a <- lapply(x, function(getstuff) getstuff[, 2]) # NOTE: Var can be name whatever! List is returned
  cat("lapply:", class(a), "\n")
  x <- list(a=matrix(1:6, 3, 2), b=matrix(1:6, 3, 2))
  a <- sapply(x, function(getstuff) getstuff[, 2]) # Same length, matrix is returned
  cat("sapply:", class(a), "\n")
  
  x <- list(2,3,4,5)
  a <-(sapply(x, function(getstuff) getstuff^2)) # Length == 1 for all element, vector is returned
  cat("sapply:", class(a))
  
  a <- apply(x, 1, quantile, probs=c(0.25, 0.75))
  a
  
  a <- array(rnorm(2*2*10), c(2, 2, 10))
  apply(a, c(1, 2), mean)
  # Use rowSums, colSums, rowMeans, colMeans for faster variants of apply(x, 1, sum) etc.
  # The speed improvement is noticeable on larger matrices only
  rowMeans(a, dims=2)
  
  # tapply, applying function to vectors in array
  x <- c(rnorm(10), runif(10), rnorm(10, 1))
  f <- gl(3, 10) # The levels/factors and length
  f
  tapply(x, f, mean)
  tapply(x, f, range, simplify=F)
  
  # split airquality dataset in months
  s <- split(airquality, airquality$Month)
  # use lapply to calculate means for some columns
  lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
  
}

DoSomeTimeSeries <- function() {
  # Use zoo/xts libraries
  prices <- c(132.45, 130.85, 130.00, 129.55, 130.85)
  dates <- as.Date(c("2010-01-04", "2010-01-07", "2010-01-06", "2010-01-05","2010-01-08"))
  ibm.daily <- zoo(prices, dates)
  plot(ibm.daily, type="l", col="blue")
  title("IBM Daily")
  grid()
  
  coredata(ibm.daily)
}

DoSomeGoogleMaps <- function(city) {
  # http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
  # library(ggmap)
  set.seed(500) # Why? For jitter amount being the same every time?
  
  # What does code below do? Works only for city=Houston. Lat/Long Coordinates?
  df <- round(data.frame(
    x = jitter(rep(-95.36, 50), amount = .3),
    y = jitter(rep( 29.76, 50), amount = .3)
  ), digits = 2)
  
  map <- get_googlemap(city, markers = df, path = df, scale = 2)
  ggmap(map, extent = "device")
  
  # ggmap(paris, extent = "normal")
}

DoCorrelogramAndMosaicPlots <- function() {
  library(corrgram)
  
  corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
           upper.panel=panel.pie, text.panel=panel.txt,
           main="Correlogram of mtcars intercorrelations")
  
  library(vcd)
  
  data <- ftable(Titanic)
  mosaic(~ Class + Sex + Age + Survived, data=Titanic, shade=TRUE, legend=TRUE)
}
