# Trying to simplify a trajectory
# -------------------------------
SetStandardOptions()

#package.install("pracma")
library(pracma) # for linspace

SimplifyPath <- function(data, times, segment.start, segment.end) {
  print(paste("Analyzing start ", segment.start, "to", segment.end, "for interval", interval))
  #interval <- 10  #times <- times - 1 # Global var instead?
  
  if (times == 0 | ((segment.end - segment.start) + 1) < interval)
    return()
  
  #print(c(times, segment.start, segment.end, segment.end - segment.start))
  # Get the Euclidean distance between the straight line segment and all points on that line segment,
  # and select the point that's furthest away from the straight line segment
  straight.line.x <- linspace(data$x[segment.start], data$x[segment.end], (segment.end - segment.start) + 1)
  #print(straight.line.x)
  straight.line.y <- linspace(data$y[segment.start], data$y[segment.end], (segment.end - segment.start) + 1)
  #print(straight.line.y)
  lines(c(data$x[segment.start], data$x[segment.end]), c(data$y[segment.start], data$y[segment.end]),
        col="green", lty=2)
  max.dist <- -Inf 
  new.point <- 0
  
  for (counter in (segment.start):(segment.end)) {
    # Find the greates distance between the point and the straight line segment
    dist <- MyEuclideanDistance(c(straight.line.x[(counter - segment.start) + 1],
                                  straight.line.y[(counter - segment.start) + 1]),
                                c(data$x[counter], data$y[counter]))
    print(c(round(counter, 0), round(dist, 2)))
    
    if (dist > max.dist) {
      max.dist <- dist
      new.point <- counter
    }
  }
  
  if (new.point > 0 & max.dist != 0) {
    print(paste0("New point:", new.point))
    df$new.point.x[new.point] <<- data$x[new.point] 
    df$new.point.y[new.point] <<- data$y[new.point]

    if (((new.point - segment.start) + 1) > interval) # TODO: constant
      SimplifyPath(data, times, segment.start, new.point)
    
    if (((segment.end - new.point) + 1) > interval) # TODO: constant
      SimplifyPath(data, times, new.point, segment.end)
  }
}

CreateRandomWalk <- function(n) {
  trajectory <- numeric(0)
  trajectory[1] <- 1
  for (counter in 2:n) {
    direction <- sample(-1:1,1)
    trajectory[counter] <- trajectory[counter - 1] + direction
  }
  return (trajectory)
}

GetRandomDrivers <- function() {
  file.from <- sample(200, 1)-4
  file.to <- file.from + 3
  op <- par()
  par(mar=c(3,3,2,.5))
  par(mfrow=c(2, 2))
  
  trajectories <- list()
  t.counter <- 1
  driverfolder <- sample(100:250, 1)
  dataFolder <- paste0("C:/coding/Kaggle/DriverTelematicsAnalysis/data/drivers/drivers/", driverfolder, "/")
  
  for (counter in file.from:file.to) {
    train <- read.csv(paste0(dataFolder, paste0(counter, ".csv")), header=T, sep=",", stringsAsFactors=F)
    # TODO: Wrong to normalize this way? Are some routes just partial, therefore on a smaller x/y scale?
    #train$x <- Normalize(min(train$x), max(train$x), 0, 100, train$x)
    #train$y <- Normalize(min(train$y), max(train$y), 0, 100, train$y)
    dim(train)
    head(train)
    summary(train)
    sapply(train, class)
    plot(train, type="o", col="blue", cex.lab=.7, cex.axis=.7, asp=1, main=paste("Trajectory", counter))
    trajectories[[t.counter]] <- train
    t.counter <- t.counter + 1
  }
  par(mar=op$mar)
  par(mfrow=op$mfrow)
  par(mfrow=c(1, 1))
  
  return(trajectories)
}


#points.y <- sin(seq(-pi,pi,.1))+runif(length(seq(-pi,pi,.1)))
#points.y <- CreateRandomWalk(30)
#points.y <- sin(seq(-pi,pi*sample(10,1),.1))+sample(5,1)
#points.x <- 1:length(points.y)
trajectories <- GetRandomDrivers()
points.x <- trajectories[[2]]$x
points.y <- trajectories[[2]]$y

n <- length(points.x)
plot(points.x, points.y, type="o", main="Trajectory, simplified", col="blue", asp=1)
df <- data.frame(x=points.x, y=points.y, new.point.x=NA, new.point.y=NA)
df$new.point.x[1] <- df$x[1]
df$new.point.y[1] <- df$y[1]
df$new.point.x[n] <- df$x[n]
df$new.point.y[n] <- df$y[n]
#df
times <- 4
interval <- round(nrow(df) / 14)

SimplifyPath(df, times, 1, n)
# TODO: Go through all segments, and just use the ones whereA the angle diff of <n-1,n> is greater than <threshold>
lines(df[!is.na(df$new.point.x),3:4], col="red", type="o", pch=19)

# ---------------------------------------------------------------------------------------------------------------------
