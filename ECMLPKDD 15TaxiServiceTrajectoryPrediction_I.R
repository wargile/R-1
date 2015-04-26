# ECML/PKDD 15: Taxi Service Trajectory Prediction (I)
# http://www.kaggle.com/c/pkdd-15-predict-taxi-service-trajectory-i
# Deadline: 01.07.2015

# See also: convolve, fft, filter, time series logic, arima, etc.

# General info:
# Competition affiliated with: http://www.ecmlpkdd2015.org/

# http://formulas.tutorvista.com/physics/kinematics-formulas.html
# http://mathworld.wolfram.com/VelocityVector.html
# en.wikipedia.org/wiki/Acceleration
# https://www.khanacademy.org/science/physics/one-dimensional-motion
# https://thecuriousastronomer.wordpress.com/2014/09/15/derivation-of-centripetal-acceleration-using-polar-coordinates/
# https://nb.khanacademy.org/science/physics/two-dimensional-motion
# http://www.thinkmind.org/download.php?articleid=adaptive_2014_3_30_50086
# http://178.22.59.152/documents/Driving%20Style%20Recognition%20Based.pdf
# http://cs229.stanford.edu/proj2013/DriverIdentification.pdf
# http://cs229.stanford.edu/proj2013/NieWuYu_Driving%20Behavior%20Improvement%20and%20Driver%20Recognition%20Based%20on%20Real-Time%20Driving%20Information.pdf
# https://thecuriousastronomer.wordpress.com/2014/09/15/derivation-of-centripetal-acceleration-using-polar-coordinates/
# http://math.stackexchange.com/questions/1115619/centripetal-acceleration-for-a-polyline
# http://en.wikipedia.org/wiki/Polar_coordinate_system
# ------------------------------------------------------------------------------------------------------------

library(randomForest)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html
library(stats) # For using fft() Fast Fourier Transform
library(kernlab)
library(e1071)

set.seed(16071962)

# Set some standard graphical params for plot
SetStandardOptions()

dataFolder <- "C:/coding/Kaggle/ECMLPKDD15TaxiServiceTrajectoryPrediction_I/data/"
codeFolder <- "C:/coding/Kaggle/ECMLPKDD15TaxiServiceTrajectoryPrediction_I/code/"
submissionsFolder <- "C:/coding/Kaggle/ECMLPKDD15TaxiServiceTrajectoryPrediction_I/submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- fread(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
}

dim(train)
str(train)

# Calculate distance stuff:
# https://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11299/score-0-66-with-logistic-regression
quantile(sqrt((diff(train$x)^2) + diff(train$y)^2), seq(.2, 1, by=.2))

# TODO: Speed calculations, average speed, etc.
# 200 seconds / 3.5715 = 60

# http://en.wikipedia.org/wiki/Radian
DegreesToRadians <- function(d) return(d * (pi / 180))
RadiansToDegrees <- function(r) return((r * 180) / pi)

# http://en.wikipedia.org/wiki/Rotation_matrix
RotateCurve <- function(data, rotation=90) {
  theta <- DegreesToRadians(rotation) # find radians for degrees to use in rotation matrix
  rotation.matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2, byrow=T) # rotation matrix for any angle
  #rotation.matrix.90 <- matrix(c(0, -1, 1, 0), ncol=2, byrow=T) # Rotation matrix for 90 degree rotation
  #rotation.matrix.180 <- matrix(c(-1, 0, 0, -1), ncol=2, byrow=T) # Rotation matrix for 180 degree rotation
  #rotation.matrix.270 <- matrix(c(0, 1, -1, 0), ncol=2, byrow=T) # Rotation matrix for 270 degree rotation
  #rotation.matrix <- rotation.matrix.90
  rotated.matrix <- t(rotation.matrix %*% t(data))
  min.x <- min(rotated.matrix[,1])
  max.x <- max(rotated.matrix[,1])
  min.y <- min(rotated.matrix[,2])
  max.y <- max(rotated.matrix[,2])
  if (min(data$x) < min.x) min.x <- min(data$x)
  if (min(data$y) < min.y) min.y <- min(data$y)
  if (max(data$x) > max.x) max.x <- max(data$x)
  if (max(data$y) > max.y) max.y <- max(data$y)
  plot(data$x, data$y, type="l", col="blue", xlim=c(min.x, max.x), ylim=c(min.y, max.y), asp=1,
       cex.lab=.7, cex.axis=.7, main=paste("Trajectories rotated", rotation, "degrees"), cex.main=.8)
  grid()
  abline(v=0, h=0, col="gray")
  lines(rotated.matrix, col="red")
}

# NOTE: Can also use rollapply() in zoo package for sliding window
CreateSMA <- function(data, sliding.window=10) {
  sma <- numeric(0)
  pos <- 1
  
  for (counter in round(sliding.window / 2):(length(data) - round(sliding.window / 2))) {
    sma[pos] <- mean(data[(counter - round(sliding.window / 2)):(counter + round(sliding.window / 2))])
    pos <- pos + 1
  }
  
  sma
}

PrepareData <- function(data) {
  distance <- sqrt(diff(data[, 1])^2 + diff(data[, 2])^2) # Distance pr. sec. in meters
  km.h <- (distance / 1000) * (60 * 60)
  message <- ""

  # If speed > 120 remove outlier(s) from data
  if (length(km.h[km.h > 120]) > 0) {
    remove.cols <- which(km.h > 120)
    data <- data[-remove.cols, ]
  }
  
  return(data)
}

# Get trip speed in km/h
CalculateSpeed <- function(data, remove.outliers=F, sliding.window=8, render.plot=T) {
  distance <- sqrt(diff(data[, 1])^2 + diff(data[, 2])^2) # Distance pr. sec. in meters
  km.h <- (distance / 1000) * (60 * 60)
  removed.outliers <- 0
  message <- ""
  high.speed <- 120
  # If speed > 120 remove outlier(s) from data
  if (length(km.h[km.h > high.speed]) > 0 & remove.outliers == T) {
    removed.outliers <- length(km.h[km.h > high.speed])
    message <- paste0(" (", removed.outliers, " outlier(s) removed)")
    km.h <- km.h[km.h <= high.speed]
  }
  
  if (render.plot) {
    plot(km.h, type="l", col="blue", main=paste("Speed km/h for a", round(nrow(data) / 60, 2), "minute drive", message),
         cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Seconds", ylab="km/h")
    lines(CreateSMA(km.h, sliding.window), col="red")
    abline(h=mean(km.h), col="green4")
    abline(h=median(km.h), col="green3")
  }

  return(km.h)
}

CalculateAccelerationChanges <- function(data, sliding.window=8, render.plot=T) {
  speed <- CalculateSpeed(data, T, 1, F)
  speed <- CreateSMA(speed, sliding.window)
  diff.speed <- diff(speed)
  q <- as.numeric(quantile(diff.speed, na.rm=T, seq(0, 1, by=.1)))
  diff.speed[(diff.speed < q[10] & diff.speed > 0) | (diff.speed > q[2] & diff.speed < 0)] <- 0
  
  if (render.plot == T)
    plot(diff.speed, type="l", main="Diff acceleration/de-acceleration", col="blue", xlab="")
  
  return(length(which(diff.speed != 0)))
}

# Get total trip distance in meters
CalculateDistance <- function(data, render.plot=T) {
  distance <- sqrt(diff(data[,1])^2 + diff(data[,2])^2) # Distance pr. sec. in meters
  
  if (render.plot == T) {
    plot(distance, type="l", col="blue", main=paste0("Distance (", round(sum(distance / 1000), 2), " km)"),
         cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Seconds", ylab="Distance")
    lines(CreateSMA(distance, 16), col="red")
    abline(h=mean(distance), col="green4")
    abline(h=median(distance), col="green3")
  }
  
  return(distance) # TODO: Round distance if using as predictor variable?
}

# Get total trip time in seconds
CalculateTime <- function(data) {
  return(nrow(data)) # Seconds, other measurements needed?
}

# Get average acceleration as meters pr. second
CalculateAverageAcceleration <- function(data, measure.points=1, render.plot=T) {
  # http://formulas.tutorvista.com/physics/average-acceleration-formula.html
  # NOTE: Result is rounded
  if (measure.points == 1)
    return (round(sum(CalculateDistance(data, F)) / 200)) # Return delta(V) / delta(T) in seconds
  else {
    # TODO: Split the data into <n> chunks, and divide by (200 / <n>)
    result.temp <- list()
    result <- numeric(0)
    d <- CalculateDistance(data, F)
    
    for (counter in 1:ceiling(length(d) / measure.points)) {
      result.temp[[counter]] <- d[(((counter - 1) *
                                      measure.points) + 1):(((counter - 1) * measure.points) + measure.points)]
      result[counter] <- sum(result.temp[[counter]] / measure.points)
    }
    
    if (render.plot == T) {
      plot(result, type="o", col="blue", main=paste0("Average acceleration, chunks of ", measure.points, " elements"),
           cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Distance", ylab="Acceleration")
      abline(h=mean(result, na.rm=T), col="green4")
      abline(h=median(result, na.rm=T), col="green3")
    }
    
    return(result)
  }
}

CalculateFFT <- function(data, render.plot=T) {
  data <- CalculateSpeed(data)
  plot(fft(data), type="l", col="red")
}

# TODO: What we might want here: The number of turns in a trip, moreso than where they are(?)
CalculateTurningPoints <- function(data, render.plot=T) {
  # Heading change:
  # http://gamedev.stackexchange.com/questions/13693/how-to-calculate-turn-heading-to-a-missile
  # http://stackoverflow.com/questions/7586063/how-to-calculate-the-angle-between-a-line-and-the-horizontal-axis
  # http://stackoverflow.com/questions/14631776/calculate-turning-points-pivot-points-in-trajectory-path
  # http://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm
  # http://stackoverflow.com/questions/3486172/angle-between-3-points?rq=1
  
  # B = arccos((a^2 + c^2 - b^2) / 2ac)
  # NOTE: a = point1, b = point2, c = point3
  
  turning.points <- numeric(0)
  
  for (counter in 2:nrow(data)) {
    deltaY <- data$y[counter] - data$y[counter - 1]
    deltaX <- data$x[counter] - data$x[counter - 1]
  
    angleInDegrees = atan2(deltaY, deltaX) * 180 / pi
    turning.points[counter - 1] <- angleInDegrees
  }
  
  if (render.plot == T) {
    op <- par()
    par(mfrow=c(2,2))
    par(mar=c(4,4,1.8,.8))
    plot(data, type="o", col="blue", main="Trip data")
    CalculateSpeed(data)
    plot(turning.points, type="l", col="red", main="Turning points (angle)")
    plot(diff(turning.points), type="l", main="Turning points (diff)")
    par <- op
  }
  
  return (turning.points)
}

CalculateAngles <- function(data, render.plot=T) {
  # Also: B = arccos((a^2 + c^2 - b^2) / 2ac)
  # NOTE: a = point1, b = point2, c = point3
  angles <- numeric(0)
  
  for (counter in 2:(nrow(data) - 1)) {
    a.x <- data$x[counter - 1]
    a.y <- data$y[counter - 1]
    b.x <- data$x[counter]
    b.y <- data$y[counter]
    c.x <- data$x[counter + 1]
    c.y <- data$y[counter + 1]
    
    ang.ba <- atan((a.y - b.y) / (a.x - b.x));
    ang.bc <- atan((c.y - b.y) / (c.x - b.y));
    rslt <- ang.ba - ang.bc;
    angles[counter] = (rslt * 180) / pi;    
  }
  
  # NOTE: NaN's happen because of div/0 above, so set these to 0 (minimal change in trajectory anyway)
  angles[which(is.nan(angles) == TRUE)] <- 0
  
  if (render.plot == T) {
    op <- par()
    par(mfrow=c(2,2))
    par(mar=c(4,4,1.8,.8))
    plot(data, type="o", col="blue", main="Trip data")
    points(data[1,], col="green4", cex=2.2)
    points(data[nrow(data),], col="red", cex=2.2)
    plot(angles, type="l", col="green2", main="Turning points (angle)")
    plot(diff(angles), type="l", main="Turning points (diff)")
    #plot(CreateSMA(diff(angles), 20), type="l", col="red", main="Turning points (diff + SMA)")
    par <- op
  }
  
  return (angles)
}

# https://www.khanacademy.org/math/linear-algebra/vectors_and_spaces/dot_cross_products/v/defining-the-angle-between-vectors
# http://www.ltcconline.net/greenl/courses/107/vectors/dotcros.htm
# http://www.vitutor.com/geometry/vec/angle_vectors.html
# http://www.helixsoft.nl/articles/circle/sincos.htm
CalculateAngles2 <- function(data, render.plot=T) {
  angles <- numeric(0)
  
  # Magnitude of vector<x,y>, (3,-5): sqrt(x^2 + y^2)
  # ((x1 * x2) + (y1 * y2)) / (sqrt(x1^2 + y1^2) * sqrt(x2^2 + y2^2))
  
  for (counter in 3:(nrow(data) - 1)) {
    P <- c(data[counter - 1, 1], data[counter - 1, 2])
    Q <- c(data[counter, 1], data[counter, 2])
    
    if (Q[1] - P[1] == 0)
      Q[1] <- Q[1] + .001 # Avoid div/0 and NaN error
    
    angle <- atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi) # OK!
    angles[counter - 2] <- angle 
  }
  
  #angles[which(is.nan(angles) == TRUE)] <- 0
  
  # TODO: First do some smoothing of trajectory curve??
  
  angles.filtered <- angles

  if (render.plot == T) {
    op <- par()
    par(mfrow=c(2,2))
    par(mar=c(3,4,1.8,.8))
    plot(data, type="l", main="Trajectory", xlab="")
    points(data[1,], col="green4", cex=2.2, main="Trajectory")
    points(data[nrow(data),], col="red", cex=2.2)
    plot(angles, type="l", col="blue", main="Trajectory angles", xlab="")
    plot(diff(angles), type="l", col="red", main="Trajectory angles (diff)", xlab="")
    angles.filtered[abs(diff(angles.filtered)) < 80] <- 0
    plot(angles.filtered, type="p", pch=19, col="green4", main="Trajectory angles filtered (diff)", xlab="")
    # plot(sort(abs(diff(angles))))
    par <- op
  }
  
  # return (angles)
  # return (length(unique(angles.filtered[angels.filtered > 0])) # Return the number of unique angles
  angles.filtered[abs(diff(angles.filtered)) < 80] <- 0
  return (length(angles.filtered[angles.filtered > 0])) # Return the number of larger angles
}

AngleInRadians <- function(x, y) {
  dot.prod <- x %*% y 
  norm.x <- norm(x, type="2")
  norm.y <- norm(y, type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
  # TEST:
  # x <- as.matrix(c(2,1))
  # y <- as.matrix(c(1,2))
  # angle(t(x),y)  
}

RotateCurveToXAxis <- function(data, render.plot=T) {
  endpoint <- c(data[nrow(data), 1], data[nrow(data), 2])
  # http://www.mathopenref.com/trigprobslantangle.html
  
  # Transform: Get the length of the Hypotenuse by getting the Euclidean distance:
  p.y <- endpoint[2]
  p.x <- endpoint[1]
  euclidean.dist <- sqrt((c(p.x, p.y) - c(0, 0)) %*% (c(p.x, p.y) - c(0, 0))) # Find the Hypotenuse (/) in a right-sided triangle
  rotation <- acos(p.x / euclidean.dist)
  
  if (p.y >= 0) {
    rotation <- rotation * -1
  }
  
  #round((rotation * 180) / pi) # Radians to degrees
  degrees <- RadiansToDegrees(rotation) # Find degrees for radians to use in rotation matrix plot header
  theta <- rotation
  rotation.matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2, byrow=T) # rotation matrix for any angle
  #rotation.matrix.90 <- matrix(c(0, -1, 1, 0), ncol=2, byrow=T) # Rotation matrix for 90 degree rotation
  #rotation.matrix.180 <- matrix(c(-1, 0, 0, -1), ncol=2, byrow=T) # Rotation matrix for 180 degree rotation
  #rotation.matrix.270 <- matrix(c(0, 1, -1, 0), ncol=2, byrow=T) # Rotation matrix for 270 degree rotation
  #rotation.matrix <- rotation.matrix.90
  rotated.matrix <- t(rotation.matrix %*% t(data))
  
  if (render.plot == T) {
    min.x <- min(rotated.matrix[,1])
    max.x <- max(rotated.matrix[,1])
    min.y <- min(rotated.matrix[,2])
    max.y <- max(rotated.matrix[,2])
    
    if (min(data$x) < min.x) min.x <- min(data$x)
    if (min(data$y) < min.y) min.y <- min(data$y)
    if (max(data$x) > max.x) max.x <- max(data$x)
    if (max(data$y) > max.y) max.y <- max(data$y)
    
    par(mfrow=c(1,1))
    plot(data$x, data$y, col="blue", bg="cyan", pch=21, xlim=c(min.x, max.x), ylim=c(min.y, max.y),
         asp=1, xlab="X", ylab="Y", main=paste("Trajectory rotated", round(degrees, 2), "degrees"))
    grid()
    
    endpoint <- c(data[nrow(data), 1], data[nrow(data), 2])
    
    abline(v=0, col="gray", lwd=2)
    abline(h=endpoint[2], col="cyan")
    abline(v=endpoint[1], col="cyan")
    abline(h=rotated.matrix[nrow(rotated.matrix),2], col="cyan")
    abline(v=rotated.matrix[nrow(rotated.matrix),1], col="cyan")
    
    lines(c(0, endpoint[1]), c(0, endpoint[2]), col="green2", lty=2)
    lines(data$x, data$y, type="o", col="blue", bg="cyan", pch=21) # Redo line for better display over ablines
    lines(rotated.matrix, pch=21, type="o", col="red", bg="yellow")
  }
  
  #result <- list()
  #result[["Euclidean.Distance"]] <- sum(sqrt(abs((rotated.matrix - as.matrix(data)) %*% t(rotated.matrix - as.matrix(data)))))
  #result[["Rotated.Matrix"]] <- rotated.matrix
  #return (result)
  
  # Return the Eucidean distance:
  return (sum(sqrt(abs((rotated.matrix - as.matrix(data)) %*% t(rotated.matrix - as.matrix(data))))))
}


# -----------------------------------------------------------------------------------------------------------------------

# Get predictors to use for model
# TODO: Do detection of TURNS?
train <- GetRandomDrivers()
oldpar <- par()
par(mfrow=c(2,2))
par(mar=c(4.5,4.3,2,.8))
sliding.window <- 24
s <- CalculateSpeed(train, T, sliding.window) # Remove outliers
a <- CalculateAverageAcceleration(train, sliding.window)
a <- a[!is.na(a)] # Get rid of NA's
plot(diff(a), type="o", col="blue", cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Distance", ylab="Acceleration",
     main="Diff on acceleration") # Doing diff on these data too??
min(diff(a), na.rm=T) # NOTE: Accelerating versus de-accelerating/breaking...
max(diff(a), na.rm=T)
par(mfrow=oldpar$mfrow)
par(mar=oldpar$mar)

quantile(s, seq(.2,1,by=.2))
quantile(s, seq(.1,1,by=.1))
MyStddev(s)

# -----------------------------------------------------------------------------------------------------------------------

# Smoothing trip
trip.smooth.x <- CreateSMA(trip$x, 20)
trip.smooth.y <- CreateSMA(trip$y, 20)
trip.smooth <- data.frame(x=trip.smooth.x, y=trip.smooth.y)
par(mfrow=c(1,1))
plot(trip, type="l", col="blue")
lines(trip.smooth, type="l", col="red")
CalculateAngles2(trip)
CalculateAngles2(trip.smooth)


euclidean.dist <- numeric(0)
for (counter in 2:nrow(train))
     euclidean.dist[counter - 1] <- sqrt((c(train$x[counter],train$y[counter]) - c(train$x[counter-1],train$y[counter-1])) %*%
                              (c(train$x[counter],train$y[counter]) - c(train$x[counter-1],train$y[counter-1])))
par(mfrow=c(2,1)) 
par(mar=c(4.2,4,2,.5))
plot(train, cex.lab=.7, cex.axis=.7, col="Blue", cex.main=.8, main="Trip")
plot(euclidean.dist, type="l", cex.lab=.7, cex.axis=.7, col="Blue", cex.main=.8, main="Euclidean Distance",
     xlab="Measurement Points", ylab="Distance")
# Do a simple moving average on the euclidean curve to simplify? 
lines(CreateSMA(euclidean.dist, length(euclidean.dist) / 25), col="red")
par(mar=op$mar)
par(mfrow=op$mfrow)
# Can also achieve the euclidean distance as above with:
plot((sqrt(diff(train[,1],1,1)^2 + diff(train[,2],1,1)^2)), col="green4", type="l")


# Make a histogram on the trip distances
trip.dist <- integer(0)
trip.time <- integer(0)
pos <- 1
for (counter in 1:200) {
  train <- read.csv(paste0(dataFolder, paste0(counter, ".csv")), header=T, sep=",", stringsAsFactors=F)
  trip.time[counter] <- nrow(train)
  euclidean.dist <- numeric(0)
  for (counter in 2:nrow(train))
    euclidean.dist[counter - 1] <- sqrt((c(train$x[counter],train$y[counter]) - c(train$x[counter-1],train$y[counter-1])) %*%
                                          (c(train$x[counter],train$y[counter]) - c(train$x[counter-1],train$y[counter-1])))
  trip.dist[pos] <- sum(euclidean.dist)
  pos <- pos + 1
}
par(mfrow=c(2,2)) 
par(mar=c(4.2,4,2,.5))
hist(trip.dist, col="wheat", main="Trip lengths", cex.lab=.7, cex.axis=.7, cex.main=.8)
abline(v=median(trip.dist), col="green4")
abline(v=mean(trip.dist), col="red")
plot(sort(trip.dist), type="l", cex.lab=.7, cex.axis=.7, col="Blue", cex.main=.8, main="Trip lengths")
plot(sort(trip.time), type="l", cex.lab=.7, cex.axis=.7, col="Blue", cex.main=.8, main="Trip time")
# Calculate Km/h (average pr. trip)
km.per.hour <- ((trip.dist / 1000) / (trip.time / 60 / 60)) # TODO: correct???
plot(sort(km.per.hour), type="l", cex.lab=.7, cex.axis=.7, col="Blue", cex.main=.8, main="Km/h")
par(mfrow=c(1,1)) 


# Plot all trips for a driver
# http://en.wikipedia.org/wiki/Trajectory
oldpar=par()
par(mar=c(4.2,4,2,.5))
n <- 200
min.y <- Inf
min.x <- Inf
max.x <- -Inf
max.y <- -Inf
for (counter in 1:n) {
  train <- read.csv(paste0(dataFolder, paste0(counter, ".csv")), header=T, sep=",", stringsAsFactors=F)
  if (min(train$x) < min.x) min.x <- min(train$x)
  if (max(train$x) > max.x) max.x <- max(train$x)
  if (min(train$y) < min.y) min.y <- min(train$y)
  if (max(train$y) > max.y) max.y <- max(train$y)
}      
for (counter in 1:n) {
  train <- read.csv(paste0(dataFolder, paste0(counter, ".csv")), header=T, sep=",", stringsAsFactors=F)
  if (counter == 1)
    plot(train$x, train$y, col=counter, type="l", xlim=c(min.x, max.x), ylim=c(min.y, max.y),
         cex.lab=.7, cex.axis=.7, main="Trajectories", cex.main=.8)
  else
    lines(train$x, train$y, col=counter, type="l")
}
par(mar=oldpar$mar)


# Check the speed quantiles for all files in a driver folder
q <- data.frame()
median.speed <- numeric(0)
trip.length <- integer(0)
for (counter in 1:200) {
  train <- read.csv(paste0(dataFolder, paste0(counter, ".csv")), header=T, sep=",", stringsAsFactors=F)
  s <- CalculateSpeed(train, T, 8, F)
  q <- rbind(q, round(quantile(s, seq(.2,1,by=.2))))
  trip.length[counter] <- nrow(train)
  median.speed[counter] <- median(s)
}
names(q) <- c('a','b','c','d','e')
stddev <- apply(q, 2, sd)

# START TEST rotate to x-axis function:
n <- 200
driver <- drivers[2]
dirPath <- paste0(driverFolder, "/", driver, "/") 
# First find the x/y-lim:
x.min.final <- Inf
y.min.final <- Inf
x.max.final <- -Inf
y.max.final <- -Inf
rotated.trips <- list()
max.trip.length <- -Inf

for (i in 1:n) {
  trip <- read.csv(paste0(dirPath, i, ".csv"))
  if (nrow(trip) > max.trip.length) max.trip.length <- nrow(trip)
  # TODO: Find the <max> trip length
  data <- RotateCurveToXAxis(trip, F)[["Rotated.Matrix"]]
  x.min <- min(data[,1])
  x.max <- max(data[,1])
  y.min <- min(data[,2])
  y.max <- max(data[,2])
  if (x.min < x.min.final) x.min.final <- x.min
  if (x.max > x.max.final) x.max.final <- x.max
  if (y.min < y.min.final) y.min.final <- y.min
  if (y.max > y.max.final) y.max.final <- y.max
}

for (i in 1:n) {
  print(paste("Trip", i))
  trip <- read.csv(paste0(dirPath, i, ".csv"))
  # Fill out the trip if it is shorter than <max> trip length found in previous loop
  if (nrow(trip) < max.trip.length) {
    for (p in 1:(max.trip.length - nrow(trip))) {
      trip <- rbind(trip, trip[nrow(trip), ])
    }
  }
  data <- RotateCurveToXAxis(trip, F)[["Rotated.Matrix"]]
  # TODO: If a trip is a mirror image of another (a == (b * -1)), flip it (a = a * -1)
  # Although this is not really necessary, because the AUC (Euclidean distance from the x-axis) will be the same
  rotated.trips[[i]] <- data
  if (i == 1) {
    plot(data, type="l", main="Trajectories", xlim=c(x.min.final, x.max.final), ylim=c(y.min.final, y.max.final),
         xlab="X", ylab="Y")
    abline(h=0, v=0, col="lightgray")
  } else {
    lines(data, col=i)
  }
}
# END TEST rotate to x-axis function:

# -------------------------------------------------------------------------------------------------------------------
# START Modified code from 
# https://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11299/score-0-66-with-logistic-regression?page=4

# TODO: Discrete Fourier Transform:
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/fft.html

SpeedDistribution <- function(trip) {
  speed <- 3.6 * sqrt(diff(trip$x, 20, 1)^2 + diff(trip$y, 20, 1)^2) / 20
  return(as.numeric(quantile(speed, seq(0.05, 1, by=0.05))))
}

AccelerationDistribution <- function(trip, sliding.window) {
  a <- CalculateAverageAcceleration(trip, sliding.window, F)
  return(as.numeric(quantile(diff(a), seq(0.05, 1, by=0.05), na.rm=T)))
}

drivers = list.files(driverFolder) # Get a list of all folders (= drivers)
# randomDrivers <- sample(drivers, size=5) # Get 5 random drivers
# randomDrivers <- sample(drivers, size=25) # Get 25 random drivers (improves result)
randomDrivers <- sample(drivers, size=15) # Get 15 random drivers (improves result?)
# randomDrivers <- sample(drivers, size=45) # TEST: Get 45 random drivers (does NOT improve result!)

refData <- NULL
target <- 0
names(target) <- "target"
sliding.window <- 24

# Get data from random drivers to compare against
# TODO: Try doParallel and forEach on this loop!
for (driver in randomDrivers) {
  dirPath = paste0(paste0(driverFolder, '/'), driver, '/')

  for (i in 1:200) {
    trip <- read.csv(paste0(dirPath, i, ".csv"))
    trip <- PrepareData(trip)
    features <- c(round(SpeedDistribution(trip)), target) # NOTE: Testing with rounded result (NEW)
    
    # TEST: Add extra predictors
    features <- c(round(AccelerationDistribution(trip, sliding.window)), features) # Add acceleration quantiles
    features <- c(round(sum(CalculateDistance(trip, F))), features) # Add total distance
    features <- c(round(CalculateTime(trip)), features) # Add total time
    features <- c(round(CalculateAngles2(trip, F)), features) # Add major trajectory turns/angles (NEW)
    features <- c(CalculateAccelerationChanges(trip, 8, F), features) # Add number of larger acceleration/de-acceleration points (NEW)

    temp <- RotateCurveToXAxis(trip, F)
    if (is.nan(temp)) temp = mean(temp, na.rm=T)
    features <- c(temp, features) # Add Euclidean Distance to x-axis for rotated trajectory (NEW)
    
    # How many seconds are we standing still? What's the ratio of stop/go?
    stop.or.go.ratio <- 1 - (nrow(unique(trip)) / nrow(trip))
    features <- c(stop.or.go.ratio, features) # Add stop/go ratio (NEW)

    #result <- CalculateSpeed(trip, T, 8, F) # NOTE: Speed returns weird zero-results on some trips (too short for window after removing outliers?)
    #result <- subset(result, result > 5) # Don't use speeds below 5 km/h for calculation
    #if (length(result) > 0) {
    #  averageSpeed <- (ifelse(mean(result) < 60, 1, 2)) 
    #} else {
    #  averageSpeed = 1
    #}
    #features <- c(averageSpeed, features)
    
    refData <- rbind(refData, features)
  }
}

# Check the correlation
# refData2 <- as.data.frame(refData)
# rownames(refData2) <- NULL # Needed for rf
# colnames(refData2) <- paste0("V", 1:ncol(refData2)) # Needed for rf
# CorrelationPlot(refData2[, -ncol(refData2)]) # Remove 'target' column

target <- 1
names(target) <- "target"
submission <- NULL
sliding.window <- 24

# Get data from all the drivers (NOTE: Replace drivers[1:3] below)
# for(driver in drivers[1:3]) {
# TODO: Try doParallel and forEach on this loop!
for(driver in drivers) {
  print(driver)
  dirPath <- paste0(paste0(driverFolder, '/'), driver, '/')
  currentData <- NULL

  for (i in 1:200) {
    trip <- read.csv(paste0(dirPath, i, ".csv"))
    trip <- PrepareData(trip)
    
    features <- c(round(SpeedDistribution(trip)), target) # NOTE: Testing with rounded result (NEW)
    
    # TEST: Add extra predictors
    features <- c(round(AccelerationDistribution(trip, sliding.window)), features) # Add acceleration quantiles
    features <- c(round(sum(CalculateDistance(trip, F))), features) # Add total distance
    features <- c(round(CalculateTime(trip)), features) # Add total time
    features <- c(round(CalculateAngles2(trip, F)), features) # Add major trajectory turns/angles (NEW)
    features <- c(CalculateAccelerationChanges(trip, 8, F), features) # Add number of larger acceleration/de-acceleration points (NEW)
    
    temp <- RotateCurveToXAxis(trip, F)
    if (is.nan(temp)) temp = mean(temp, na.rm=T)
    features <- c(temp, features) # Add Euclidean Distance to x-axis for rotated trajectory (NEW)

    # How many seconds are we standing still? What's the ratio of stop/go?
    stop.or.go.ratio <- 1 - (nrow(unique(trip)) / nrow(trip))
    features <- c(stop.or.go.ratio, features) # Add stop/go ratio (NEW)
    
    #result <- CalculateSpeed(trip, T, 8, F)
    #print(result[1:10])
    #result <- subset(result, result > 5) # Don't use speeds below 5 km/h for calculation
    #if (length(result) > 0) {
    #  averageSpeed <- (ifelse(mean(result) < 60, 1, 2)) 
    #} else {
    #  averageSpeed = 1
    #}
    #features <- c(averageSpeed, features)
    
    currentData <- rbind(currentData, features)
  }
  
  train <- rbind(currentData, refData)
  train <- as.data.frame(train)
  rownames(train) <- NULL
  
  # TODO: Test with new.model <- step(org.model) for explanatory variable improvement/simplification ??
  # Get summary(model)$r.squared, compare and assign the final model to the best R^2 ??
  g <- glm(target ~ ., data=train, family=binomial("logit"))
  # TODO: Try h2o.rf on this!
  strt <- Sys.time()
  g2 <- randomForest(target ~ ., data=train, ntrees=150)
  print(Sys.time() - strt)
  #g <- glm(target ~ V1+V2+V4+V15+V32+V43+V44+V45, data=train, family=binomial("logit"))
  
  # TODO: Can also manually tune by starting out with few predictors and adding, checking R2 and adj.R2 along the way.
  # Better R2 = better model. Reduced adj.R2: The new var did not improve the model
  # TEST START
  # new.g <- step(g, steps=5, trace=0) # AIC
  # if (summary(new.g)$r.squared > summary(g)$r.squared) g <- new.g
  # TEST END
  
  # SVM test:
  # rbf <- rbfdot(sigma=0.1)
  # target <- rep(1, nrow(train)) # Needed for ksvm
  # g <- ksvm(target ~ ., data=train, type="eps-svr", kernel=rbf, C=10, prob.model=TRUE) # C-bsvc
  # fitted(g)
  # result <- predict(g, newdata=currentData, type="probabilities")
  
  # GBM test:
  #library(gbm)
  #g <- gbm(target ~., data=train, n.trees=500, shrinkage=0.005, cv.folds=10, n.cores=4,
  #           distribution="bernoulli", bag.fraction=.5, train.fraction=1, n.minobsinnode=10, keep.data=T,
  #           verbose=T, var.monotone=NULL)
  #
  #best.iter <- gbm.perf(g, method="cv")
  #p <- predict.gbm(g, currentData, best.iter, type="response")
  
  
  colnames(train) <- paste0("V", 1:ncol(train)) # Needed for rf
  #g <- randomForest(target ~ ., data=train, proximity=F, keep.forest=T, nodesize=10, ntree=150) # 150, 80
  
  currentData <- as.data.frame(currentData)
  rownames(currentData) <- NULL # Needed for rf
  colnames(currentData) <- paste0("V", 1:ncol(currentData)) # Needed for rf
  p <- predict(g, currentData, type="response")
  print("Prediction done!")
  labels <- sapply(1:200, function(x) paste0(driver,'_', x))
  result <- cbind(labels, p)
  submission <- rbind(submission, result)
}

colnames(submission) <- c("driver_trip","prob")
head(submission)

# Save the submission data (TODO: Process/save chunks and then rbind later?)
save(submission, file=paste0(submissionsFolder, "submission.rda"))

KaggleSubmission(submission, submissionsFolder, "glm")                         
# write.csv(submission, paste0(submissionsFolder, "submission.csv"), row.names=F, quote=F)

# glm with 8 features types (SpeedDistribution, AccelerationDistribution, CalculateDistance, CalculateTime,
# CalculateAngles2, CalculateAccelerationChanges, RotateCurveToXAxis and stop/go ratio) and 25 random drivers
# gives best resuls: 0.73332

# END Modified code from 
# https://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11299/score-0-66-with-logistic-regression?page=4
# --------------------------------------------------------------------------------------------------------------------
