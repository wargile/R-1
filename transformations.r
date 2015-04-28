# http://www.utexas.edu/courses/schwab/sw388r7/SolvingProblems/ComputingTransformations.ppt

oldpar <- par()
par(cex=1)
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)
par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot
par(mar=c(3,3,2,1))

# Inverse Transformation of positively skewed variables:
A <- c(1,3,0,5,2)
A <- rnorm(100) * 10
A
B.inv <- 1/((max(A)+1)-A)
B.inv
# Log Transformation of positively skewed variables:
B.log <- log10(abs(A)+1)
B.log
# Square Root Transformation of positively skewed variables:
B.sqrt <- sqrt(abs(A))
B.sqrt

# Example of the square transformation:
A <- c(-10, 0, 10, 20, 30)
A
B.square <- (A + abs(min(A))) * (A + abs(min(A)))
B.square

# -------------------------------------------------------------------------------------------------------------------------------
# Rotate a curve down on the x-axis:
DegreesToRadians <- function(d) return(d * (pi / 180))
RadiansToDegrees <- function(r) return((r * 180) / pi)

# http://en.wikipedia.org/wiki/Rotation_matrix
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

  result <- list()
  result[["Euclidean.Distance"]] <- sum(sqrt(abs((rotated.matrix - as.matrix(data)) %*% t(rotated.matrix - as.matrix(data)))))
  result[["Rotated.Matrix"]] <- rotated.matrix
  return (result)
}

# Generate a random trajectory, and rotate a curve down on the x-axis:
par(mfrow=c(1,1))
#x <- (matrix(c(0,0,sort(abs(rnorm(30)))), ncol=2, byrow=T))

# Get the endpoint rotated down to the x-axis
# endpoint <- c(x[nrow(x),1],x[nrow(x),2])
# TODO: Convert vertical distance (euclidean distance) from <x,0> to <x,y> to <n> degrees. Rotate <n> degrees towards x-axis
# http://www.mathopenref.com/trigprobslantangle.html

# Transform: Get the length of the Hypotenuse by getting the Euclidean distance:
# p.y <- endpoint[2]
# p.x <- endpoint[1]
# euclidean.dist <- sqrt((c(p.x, p.y) - c(0, 0)) %*% (c(p.x, p.y) - c(0, 0))) # Find the Hypotenuse (/) in a right-sided triangle
# angle <- acos(p.x / euclidean.dist)
# angle
# round((angle * 180) / pi)
# x <- as.data.frame(x)
# names(x) <- c("x","y")

# if (p.y < 0) {
#   rotated.curve <- RotateCurve(x, RadiansToDegrees(angle))
# } else {
#   rotated.curve <- RotateCurve(x, RadiansToDegrees(-angle))
#}

trips <- GetRandomDrivers()
x <- trips[[4]]
sum(x / sum(x)) # Norm 1
rotated.curve <- RotateCurveToXAxis(x)

# TODO: Put in function, or return as RotateCurveToXAxis() result?
# Distance between these two curves (TODO: Normalize/center/etc. according to min/max? Can be very large values now):
euclidean.dist <- sum(sqrt(abs((rotated.curve - as.matrix(x)) %*% t(rotated.curve - as.matrix(x)))))
euclidean.dist

# http://www.matterandinteractions.org/VectorAppendix.pdf
# http://www.phy6.org/stargaze/Strig4.htm
# NOTE: Function assumes a 2x2 vector of x/y coordinates (line from-to)
FindDirectionOfVectorInDegrees <- function(data) {
  #if (nrow(data) != 2 | ncol(data) != 2 | sum(data) == 0)
  #  return (-1)
  if (nrow(data) != 2 | ncol(data) != 2)
    return (-1)
  
  #print(data)
  
  hypotenuse <- FindMagnitudeOfVector(data)
  adjacent <- abs(data[2, 1])
  angle <- acos(adjacent / hypotenuse) * 180 / pi
  
  if (data[2, 1] < 0 & data[2, 2] >= 0)
    angle <- 90 + (90 - angle)
  if (data[2, 1] <= 0 & data[2, 2] < 0)
    angle <- 180 + angle
  if (data[2, 1] > 0 & data[2, 2] < 0)
    angle <- 360 - angle
  
  return (angle)
}

FindDirectionOfVectorInDegrees2 <- function(data) {
  #if (nrow(data) != 2 | ncol(data) != 2 | sum(data) == 0)
  #  return (-1)
  if (nrow(data) != 2 | ncol(data) != 2)
    return (-1)
  
  Q <- data[nrow(data),]
  P <- data[1,]
  result <- as.numeric((atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi))) # OK!
  
  # TODO: Must be a simpler way than code below??
  # Regardless of angle/direction, the vector is always the hypotenuse in a right sided-triangle
  if (Q[1] < 0 & Q[2] >= 0)
    result <- 90 + (90 + result)
  if (Q[1] < 0 & Q[2] < 0)
    result <- 180 + result
  if (Q[1] >=0 & Q[2] < 0)
    result <- 360 + result
  
  return (result)
}

FindMagnitudeOfVector <- function(data) {
  Q <- data[nrow(data),]
  P <- data[1,]
  return (sqrt((Q[1]-P[1])^2 + (Q[2]-P[2])^2))
}

x <- matrix(c(0,0,-6,0), ncol=2, byrow=T)
x <- matrix(c(0,0,-6,-15), ncol=2, byrow=T) # OK
x <- matrix(c(0,0,6,-15), ncol=2, byrow=T)
x <- matrix(c(0,0,-6,15), ncol=2, byrow=T)
plot(x, type="b", col="blue", asp=1,
     main=paste("FindDirectionOfVectorInDegrees test. Angle:", round(FindDirectionOfVectorInDegrees(x), 2)),
     xlab="x", ylab="y")
abline(h=0, v=0, col="gray")

# http://www.mathsisfun.com/algebra/trig-finding-angle-right-triangle.html
# SOH-CAH-TOA rule:
# When we know Opposite and Hypothenuse, use Sin
# When we know Adjacent and Hypothenuse, use Cos
# When we know Opposite and Adjancent, use Tan
x <- matrix(c(0, 0, 2.1, -5.25), ncol=2, byrow=T)
x <- matrix(c(0, 0, -3.389, 2.967), ncol=2, byrow=T)
x <- matrix(c(0, 0, 5.511, 6.333), ncol=2, byrow=T)

plot(x, type="b", col="blue", asp=1, pch=21, bg="cyan",
     main=paste("FindDirectionOfVectorInDegrees test. Angle:", round(FindDirectionOfVectorInDegrees(x), 2), "degrees"),
     xlab="x", ylab="y")
abline(h=0, v=0, col="gray")

# -------------------------------------------------------------------------------------------------------------------------
# Find the angle difference between two line segments:
# trips <- GetRandomDrivers()
trip <- trips[[2]]
plot(trip, type="l")
plot(diff(trip$x, lag=30), type="l")
trip.smooth <- data.frame(x=CreateSMA(trip$x, 8), y=CreateSMA(trip$y, 8))
plot(trip.smooth, type="l")
plot(trip.smooth[140:160,], type="o")
segment1 <- trip.smooth[142:143,]
segment2 <- trip.smooth[164:165,]
segment1[2, ] <- segment1[1, ] - segment1[2, ]
segment1[1, ] <- c(0,0)
segment2[2, ] <- segment2[2, ] - segment2[1, ]
segment2[1, ] <- c(0,0)
seg1.mat <- as.matrix(segment1, ncol=2, byrow=T)
seg2.mat <- as.matrix(segment2, ncol=2, byrow=T)
angle1 <- round(FindDirectionOfVectorInDegrees(seg1.mat), 2)
angle2 <- round(FindDirectionOfVectorInDegrees(seg2.mat), 2)

if (angle1 > 180) {
  angle1 <- 360 - angle1
}
if (angle2 > 180) {
  angle2 <- 360 - angle2
}
angle.diff <- angle1 + angle2
angle.diff

par(mfrow=c(2,2))
plot(segment1, type="b", asp=1)
plot(segment2, type="b", asp=1)
plot(seg1.mat, type="b", col="blue", asp=1, pch=21, bg="cyan", main=paste("Seg1. Angle:", angle1, "degrees"), xlab="x", ylab="y")
abline(h=0, v=0, col="gray")
plot(seg2.mat, type="b", col="blue", asp=1, pch=21, bg="cyan", main=paste("Seg2. Angle:", angle2, "degrees"), xlab="x", ylab="y")
abline(h=0, v=0, col="gray")
par(mfrow=c(1,1))
# TODO: Find the hypotenuse (euclidean dist.), and find the angle opposite the hypotenuse (atan or acos? SOH CAH TOA) ??

GetAngleDiff <- function(trip, interval=8) {
  angles <- numeric(0)

  #trip <- round(trip, 2)
  #plot(trip, type="l")
  #plot(diff(trip$x, lag=30), type="l")
  trip.smooth <- trip
  #plot(trip.smooth, type="l")
  #plot(trip.smooth[140:160,], type="o")
  angles.org <- numeric(0)
  old.counter <- 2
  
  # TODO: Need to sample bits of trajectory with regular intervals!
  #intervals - seq(interval, nrow(trip.smooth), b=interval)
  sampled.segments <- data.frame(x=NA, y=NA)
  
  for (counter in 2:(nrow(trip.smooth)-2)) {
    if (counter %% interval != 0) {
      angles[pos] <- angles[pos - 1]
    } else {
      # We're moving, so sample the two line segments
      segment1 <- trip.smooth[old.counter:(old.counter + 1),]
      segment2 <- trip.smooth[counter:(counter + 1),]
      sampled.segments <- rbind(sampled.segments, segment1)
      sampled.segments <- rbind(sampled.segments, segment2)
      old.counter <- counter
      old.counter <- counter
      segment1[2, ] <- segment1[2, ] - segment1[1, ]
      segment1[1, ] <- c(0,0)
      segment2[2, ] <- segment2[2, ] - segment2[1, ]
      segment2[1, ] <- c(0,0)
      angle1 <- round(FindDirectionOfVectorInDegrees(segment1), 2)
      angle2 <- round(FindDirectionOfVectorInDegrees(segment2), 2)
      angles.org <- c(angles.org,angle1,angle2)
      #print(seg1.mat)
      #print(seg2.mat)
      #print(c(counter, ":", angle1, angle2))
    
      if (sum(seg1.mat) == 0) angle1 = 0
      if (sum(seg2.mat) == 0) angle2 = 0
      #if (angle1 != 0 & angle2 != 0) {    
      #  if (angle1 > 180) {
      #    angle1 <- 360 - angle1
      #  }
      #  if (angle2 > 180) {
      #    angle2 <- 360 - angle2
      #  }
      #}  
      angles[pos] <- abs(angle1 - angle2)
    }
    pos <- pos + 1
  }
  
  return (angles)
  #return(sampled.segments)
}

# http://stackoverflow.com/questions/14631776/calculate-turning-points-pivot-points-in-trajectory-path

# Use the Ramer-Douglas-Peucker algorithm to simplify the path
# http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm
# http://www.r-bloggers.com/simplifying-spatial-polygons-in-r/
package.install("rgeos")
library(rgeos)
library(sp)
gSimplify(select.spatial(trip), tol=5) # Needs sp
# http://cran.r-project.org/web/packages/sp/sp.pdf

# Convolve:
par(mfrow=c(1,2))
plot(trip.smooth, asp=1)
plot(convolve(trip.smooth$x, trip.smooth$y, type="open"))
par(mfrow=c(1,1))

interval <- 10
trip.smooth <- data.frame(x=CreateSMA(trip$x, 4), y=CreateSMA(trip$y, 4))
trip.smooth <- unique(trip.smooth)
#trip.smooth <- trip
angles <- GetAngleDiff(trip.smooth, interval)
plot(angles, type="l")
diff.angles <- diff(angles)
plot(diff.angles, type="l")
diff.angles.pos <- trip.smooth[round(which(abs(diff.angles) > 50)) - (interval / 2),]
plot(trip, pch=".", asp=1)
points(diff.angles.pos, col="red")

# -------------------------------------------------------------------------------------------------------------------------

# Draw a circle with radius line:
size <- 6
line <- seq(0, pi * 2, length=200)
curve_x <- cos(line)
curve_y <- sin(line)
curve_x <- curve_x * size
curve_y <- curve_y * size
plot(c(-1, 1), c(-1, 1), type = "n", ylim=c(-size, size), xlim=c(-size, size))
lines(curve_x, curve_y, col="blue", type="o", bg="lightgray", pch=21)

circumference <- (pi * 2)
diameter <- circumference / pi
radius <- diameter / 2
lines(c(0, size),c(0, 0), col="red")

# ------------------------------------------------------------------------------------------------------------
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

# Test our FindDirectionVectorInDegrees() function to find sharp turns in a trajectory
trips <- GetRandomDrivers()
trip <- trips[[2]]
angles <- numeric(0)

# First, go through the trip and remove duplicates
trip <- round(trip)
trip <- unique(trip)

# Smooth jagged measurements...
smooth.x <- CreateSMA(trip$x, 8)
smooth.y <- CreateSMA(trip$y, 8)
trip <- data.frame(x=smooth.x, y=smooth.y)

interval <- ceiling(nrow(trip) / 40)
if (interval < 5) {
  interval <- 5
}
if (nrow(trip) < 25) { # TODO....
  interval <- 1
}
for (counter in 2:(nrow(trip) - 1)) {
  data <- trip[(counter:(counter + 1)),]
  data[2, ] <- data[2, ] - data[1, ]
  data[1, ] <- c(0,0)
  # Try subsampling pairs of coords (from-to pairs with longer intervals) to get "sharper" turns
  if (counter %% interval != 0) {
    # We're not sampling, so just add previous coord
    angles[counter - 1] <- angles[counter - 2]
  } else {
    # We're moving, so sample it
    angles[counter - 1] <- FindDirectionOfVectorInDegrees(data)
  }
  #angles[counter - 1] <- FindDirectionOfVectorInDegrees(data)
}

threshold <- 50
#difference <- diff(angles, lag=interval) # NOTE: lag added!
difference <- diff(angles) # NOTE: lag added!
coords <- which(abs(difference) > threshold) + 1
x.coords <- trip[coords - floor(interval/2), 1]
y.coords <- trip[coords - floor(interval/2), 2]
#x.coords <- trip[coords+1, 1]
#y.coords <- trip[coords+1, 2]

if ((max(trip[,1]) - min(trip[,1])) > (max(trip[,2]) - min(trip[,2]))) {
  par(mfrow=c(3,1))
} else {
  par(mfrow=c(1,3))
}
plot(trip, type="l", col="blue", main=paste0("Trip (turns found: ", length(coords), ")"), asp=1)
points(trip[1,], col="green3", pch=19)
points(trip[nrow(trip),], col="red", pch=19)
points(x.coords, y.coords, col="blue")
plot(angles, type="l", col="violetred3", main="Angles", ylim=c(0, 360))
plot(difference, type="l", col="orange3", main="Diff(angles)", ylim=c(-360, 360))
par(mfrow=c(1,1))

# -----------------------------------------------------------------------------------------------------------------------------

# TIPS: http://www.mathopenref.com/trigangle.html

oldpar <- par()
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)
par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot

first_derivative <- function(x) {
  return (x[3:length(x)] - x[1:(length(x)-2)])
}
second_derivative <- function(x) {
  return (x[3:length(x)] - 2 * x[2:(length(x)-1)] + x[1:(length(x)-2)])
}

curvature <- function(x, y) {}
x_1 <- first_derivative(x)
x_2 <- second_derivative(x)
y_1 <- first_derivative(y)
y_2 <- second_derivative(y)
return (abs(x_1 * y_2 - y_1 * x_2) / sqrt((x_1**2 + y_1**2)**3))
}

trips <- GetRandomDrivers()
trip <- trips[[1]]
y <- trip$y
par(mfrow=c(1,2))
plot(y, type="l", col="blue")
plot(y[2:length(y)] - y[1:(length(y)-1)], type="l", col="red")
par(mfrow=c(1,1))
# http://stackoverflow.com/questions/22600576/r-how-do-i-find-the-second-derivative-of-a-numeric-array
deriv <- function(x, y) diff(y) / diff(x)
plot(deriv(trip$x, trip$y), type="l")

# Example :
# Find the direction of the vector  whose initial point P is at (2, 3) and end point is at Q is at (5, 8).
# The coordinates of the initial point and the terminal point are given. Substitute them in the formula.

P <- c(trip[89,1], trip[89,2])
Q <- c(trip[90,1], trip[90,2])
# http://hotmath.com/hotmath_help/topics/magnitude-and-direction-of-vectors.html

P <- c(2, 3)
Q <- c(5, 8)

(Q[2] - P[2]) / (Q[1] - P[1])
# Find the inverse Tan and convert to degrees:
atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi) # OK!

# --------------------------------------------------------------------

P <- c(trip[88,1], trip[88,2])
Q <- c(trip[89,1], trip[89,2])

diff1 <- atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi) # OK!

P <- c(trip[89,1], trip[89,2])
Q <- c(trip[90,1], trip[90,2])

diff2 <- atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi) # OK!

P <- c(trip[90,1], trip[90,2])
Q <- c(trip[91,1], trip[91,2])

diff3 <- atan((Q[2] - P[2]) / (Q[1] - P[1])) * (180 / pi) # OK!

c(diff1, diff2, diff3)
diff(c(diff1, diff2, diff3))


# -----------------------------------------------------------------------------------------------------

# REMEMBER: 2 * pi = 360 degrees expressed in radians, pi = 180 degrees expressed in radians

# https://www.youtube.com/watch?v=WxWJorOVIj8
# https://www.youtube.com/watch?v=98C7iv8OcnI
# https://www.youtube.com/watch?v=Wosggu8TPhM
# http://en.wikipedia.org/wiki/Trajectory
# http://www.euclideanspace.com/maths/algebra/vectors/angleBetween/
# http://www.mathworks.com/matlabcentral/newsreader/view_thread/151925
# http://www.vb-helper.com/howto_find_angles_law_of_cosines.html
# http://ubuntuforums.org/showthread.php?t=1035478
# http://www.teacherschoice.com.au/Maths_Library/Trigonometry/solve_trig_SSS.htm
v1 <- c(-3,4)
v2 <- c(0,4)
# Magnitude:
mag <- sqrt(v1[1]^2 + v1[2]^2)
mag
# Angle:
angle <- atan(abs(abs(v1[2]) / abs(v1[1])))
angle * 180 / pi # OK! ~53
plot(v1, type="o", asp=T, col="blue")
# Magnitude:
mag <- sqrt(v2[1]^2 + v2[2]^2)
mag
# Angle:
angle <- atan(abs(abs(v2[2]) / abs(v2[1])))
angle * 180 / pi
lines(v2, type="o", col="red")

a <- c(6,-2,-3)
b <- c(1,1,1)
dot.product <- sum(a * b) # Multiply each element of a with the corresponding element of b, and sum these
dot.product
angle <- acos(dot.product / (sqrt(a[1]^2 + a[2]^2 + a[3]^2) * sqrt(b[1]^2 + b[2]^2 + b[3]^2)))
angle * 180 / pi # OK! ~85

# -------------------------------------------------------------------------------------------------------------

a <- c(3,4)
b <- c(5,8)

a <- c(-8,1)
b <- c(-2,7)

plot(c(0,a[1]), c(0,a[2]), type="b", col="blue", xlim=c(0,10), ylim=c(0,10))
lines(c(0,b[1]), c(0,b[2]), type="b", col="red")

# Magnitudes:
mag.a <- sqrt(a[1]^2 + a[2]^2)
mag.a
mag.b <- sqrt(b[1]^2 + b[2]^2)
mag.b
# Angles in degrees (multiplying by 180 / pi to get from radians to degrees):
angle.a <- atan(abs(abs(a[2]) / abs(a[1])))
angle.a <- angle.a * 180 / pi
angle.a
angle.b <- atan(abs(abs(b[2]) / abs(b[1])))
angle.b <- angle.b * 180 / pi
angle.b
abs(angle.a - angle.b)

# Finding the angle between a and b directly (sum(a * b) = dot product between a and b):
acos(sum(a * b) / (mag.a * mag.b)) * 180 / pi


# Find the magnitude and compass bearing (direction) of a position vector:
# ------------------------------------------------------------------------
v <- c(-7,-6)
mag.v <- sqrt(v[1]^2 + v[2]^2)
mag.v
# tan(theta) = opposite side / adjacent side:
abs(v[2]/v[1])
# theta = atan(opp/adj):
# NOTE: abs() always gives a positive angle, see also below
atan(abs(v[2])/abs(v[1])) # Result in Radians
atan(abs(v[2])/abs(v[1])) * 180 / pi # Result in Degrees

# Example that gives a negative angle result:
v <- c(8,-4)
plot(v, type="b", col="blue")
mag.v <- sqrt(v[1]^2 + v[2]^2)
mag.v
# tan(theta) = opposite side / adjacent side:
(v[2]/v[1])
# theta = atan(opp/adj):
atan(v[2]/v[1]) # Result in Radians

if (v[2]/v[1] > 0) {
  atan(v[2]/v[1]) * 180 / pi # Result in Degrees
} else {
  # If the result is negative, add 360 degrees (or 2 * pi in radians) to get the result
  atan(v[2]/v[1]) * (180 / pi) + 360 # Result in Degrees
}

# Find the unit vector (a.k.a. normalized vector) of a vector:
# ------------------------------------------------------------
# http://en.wikipedia.org/wiki/Unit_vector
# https://www.khanacademy.org/math/linear-algebra/matrix_transformations/lin_trans_examples/v/unit-vectors

# Every nonzero vector has a corresponding unit vector, which has the same direction as that vector
# but a magnitude of 1. To find the unit vector you divide the vector by its magnitude (NOTE: magnitude = norm).
v <- c(-2,1)
u <- v / sqrt(v[1]^2 + v[2]^2)
u
sqrt(u[1]^2 + u[2]^2) # the magnitude if a unit vector should always be 1
