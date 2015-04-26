# Trying some K-means algo stuff....!
# http://en.wikipedia.org/wiki/K-means_clustering
# http://en.wikipedia.org/wiki/K-medians_clustering
# http://en.wikipedia.org/wiki/Lloyd%27s_algorithm
# http://en.wikipedia.org/wiki/Voronoi_diagram
# http://www.inference.phy.cam.ac.uk/mackay/itprnn/ps/284.292.pdf (see page 284 in PDF)
# TODO: http://en.wikipedia.org/wiki/DBSCAN
# Better separation for non-linearly separable clusters

library(scales)

# TODO: Make this generic to support <k> clusters
op <- par()
par(mar=c(4.5,4.5,2,.5))
m <- 1.325
sd <- 1
elements <- 500
x1 <- rnorm(elements, mean=-m, sd=sd)
y1 <- rnorm(elements, mean=-m, sd=sd)
x2 <- rnorm(elements, mean=m, sd=sd)
y2 <- rnorm(elements, mean=m, sd=sd)

#x1 <- c(seq(-10,-.1,.1), seq(.1,10,.1))
#y1 <- ((c(seq(-10,-.1,.1), seq(.1,10,.1)) + rnorm(200) * .8)^2)
#x2 <- c(seq(-10,-.1,.1), seq(.1,10,.1))
#y2 <- (((c(seq(-10,-.1,.1), seq(.1,10,.1)) + rnorm(200) * .8)^2) * -1) + 120
#plot(x1, y1)
#points(x2, y2, col="red")

x <- c(x1, x2)
y <- c(y1, y2)

plot(x, y, pch=19, col=alpha("gray", .4), xlim=c(min(x)-.5, max(x)+.5), ylim=c(min(y)-.5, max(y)+.5),
     cex.lab=.8, cex.axis=.7, cex.main=1)

# Random start:
# centroid1 <- c(rnorm(1, mean=-m, sd=sd), rnorm(1, mean=-m, sd=sd))
# centroid2 <- c(rnorm(1, mean=m, sd=sd), rnorm(1, mean=m, sd=sd))
# More random start:
centroid1 <- c(rnorm(1), rnorm(1))
centroid2 <- c(rnorm(1), rnorm(1))
# Even more random start:
centroid1 <- c(0,0)
centroid2 <- c(0,0)

# While(...) to make sure start point is not the same
while (centroid1[1] == centroid2[1] & centroid1[2] == centroid2[2]) {
  centroid1 <- c(sample(min(x):max(x), 1), sample(min(y):max(y), 1))
  centroid2 <- c(sample(min(x):max(x), 1), sample(min(y):max(y), 1))
  startpoint.c1 <- centroid1
  startpoint.c2 <- centroid2
}

points(x=centroid1[1], y=centroid1[2], col="red", pch=19, lwd=5)
points(x=centroid2[1], y=centroid2[2], col="blue", pch=19, lwd=5)

e.dist.c1 <- numeric(0)
e.dist.c2 <- numeric(0)

limit <- 0.0005
centroid1.old <- 0
centroid2.old <- 0
iteration <- 0
points.c1.x <- numeric(0)
points.c1.y <- numeric(0)
points.c2.x <- numeric(0)
points.c2.y <- numeric(0)

points.c1.x[iteration + 1] <- centroid1[1]
points.c1.y[iteration + 1] <- centroid1[2]
points.c2.x[iteration + 1] <- centroid2[1]
points.c2.y[iteration + 1] <- centroid2[2]

while (sum(abs(centroid1 - centroid1.old)) > limit & sum(abs(centroid2 - centroid2.old)) > limit) {
  centroid1.old <- centroid1
  centroid2.old <- centroid2

  iteration = iteration + 1
  if (iteration >= 100)
    break # Safety...
  
  for (counter in 1:length(x)) {
    # Get the Euclidean length:
    #   sqrt((c(x1,y1)-c(x2,y2)) %*% (c(x1,y1)-c(x2,y2)))
    #   sqrt(sum((c(x1,y1)-c(x2,y2))^2))
    e.dist.c1[counter] <- sqrt((c(x[counter],y[counter]) - centroid1) %*% (c(x[counter],y[counter]) - centroid1))
    e.dist.c2[counter] <- sqrt((c(x[counter],y[counter]) - centroid2) %*% (c(x[counter],y[counter]) - centroid2))
  }
  #plot(e.dist.c1, pch=16, main="K-means, Euclidean distance", cex.lab=.8, cex.axis=.8, cex.main=1,
  #  ylab="Distance to centroids")
  #points(e.dist.c2, col="red", pch=16)

  cluster <- numeric(0)
  for (counter in 1:length(x)) {
    if (e.dist.c1[counter] < e.dist.c2[counter]) # TODO: Do min(1..<n>) to support <n> clusters
      cluster[counter] = 1
    else
      cluster[counter] = 2
  }
  cluster
  #plot(cluster, type="l", col="blue", ylim=c(0.5, 2.5))

  df <- data.frame(x=x, y=y, clust=cluster)
  # The new means of the cluster points becomes the new centroids
  points.in.c1.x <- df$x[df$clust == 1]
  points.in.c1.y <- df$y[df$clust == 1]
  points.in.c2.x <- df$x[df$clust == 2]
  points.in.c2.y <- df$y[df$clust == 2]
  
  new.mean.c1.x <- mean(points.in.c1.x)
  new.mean.c1.y <- mean(points.in.c1.y)
  new.mean.c2.x <- mean(points.in.c2.x)
  new.mean.c2.y <- mean(points.in.c2.y)
  
  points(x=new.mean.c1.x, y=new.mean.c1.y, col="green", lwd=4)
  points(x=new.mean.c2.x, y=new.mean.c2.y, col="green", lwd=4)
  
  points.c1.x[iteration + 1] <- new.mean.c1.x
  points.c1.y[iteration + 1] <- new.mean.c1.y
  points.c2.x[iteration + 1] <- new.mean.c2.x
  points.c2.y[iteration + 1] <- new.mean.c2.y
  
  centroid1 = c(new.mean.c1.x, new.mean.c1.y)
  centroid2 = c(new.mean.c2.x, new.mean.c2.y)
}

endpoint.c1 <- centroid1
endpoint.c2 <- centroid2

points(points.in.c1.x, points.in.c1.y, col=alpha("red", .2))
points(points.in.c2.x, points.in.c2.y, col=alpha("blue", .2))
lines(points.c1.x, points.c1.y, col="black")
lines(points.c2.x, points.c2.y, col="black")

points(new.mean.c1.x, new.mean.c1.y, col="red", lwd=5)
points(new.mean.c2.x, new.mean.c2.y, col="blue", lwd=5)
title(paste0("K-means test (iterations=", iteration, ")"), cex.main=1)

text(x=startpoint.c1[1], y=startpoint.c1[2], labels="Start C1", adj=1.2, cex=.7)
text(x=endpoint.c1[1], y=endpoint.c1[2], labels="End C1", adj=-.3, cex=.7)
text(x=startpoint.c2[1], y=startpoint.c2[2], labels="Start C2", adj=1.2, cex=.7)
text(x=endpoint.c2[1], y=endpoint.c2[2], labels="End C2", adj=-.3, cex=.7)

par <- op

# ------------------------------------------------------------------
# Calculating Euclidian distance:
# http://en.wikipedia.org/wiki/Euclidean_distance
# Use method above, or:
# sqrt((a[1]-b[1])^2 + (a[2]-b[2])^2 + (a[3]-b[3])^2 + (a[4]-b[4])^2)
# Or just:
# sqrt(sum((a - b)^2))
# Distance between two points on the real line:
# x <- 5
# y <- -12
# sqrt((x - y)^2)
# ------------------------------------------------------------------

CreateClusters <- function(k, m=1.325, sd=1, elements=500) {
  if (k < 1)
    return (warning(paste(k, "is an invalid number!")))
  
  if (k %% sqrt(k) > 0)
    return (warning(paste(k, "is not a squareable number!")))
  
  x.coords <- list()
  y.coords <- list()
  min.x <- Inf
  min.y <- Inf
  max.x <- -Inf
  max.y <- -Inf
  pos <- 1

  for (counter in 1:sqrt(k)) {
    for (counter2 in 1:sqrt(k)) {
      x <- rnorm(elements, mean=m, sd=sd)
      y <- rnorm(elements, mean=m, sd=sd)
      x.coords[[pos]] <- x + (counter * 5)
      y.coords[[pos]] <- y + (counter2 * 5)
      
      if (min(x.coords[[pos]]) < min.x)
        min.x <- min(x.coords[[pos]])
      if (max(x.coords[[pos]]) > max.x)
        max.x <- max(x.coords[[pos]])
      if (min(y.coords[[pos]]) < min.y)
        min.y <- min(y.coords[[pos]])
      if (max(y.coords[[pos]]) > max.y)
        max.y <- max(y.coords[[pos]])
      
      pos <- pos + 1
    }
  }
  
  plot(x.coords[[1]], y.coords[[1]], xlim=c(min.x, max.x),
       ylim=c(min.y, max.y), col=alpha(1, .4), cex.main=1, cex.axis=.7, cex.lab=.8,
       main=paste(k, "k-means clusters"), xlab="X coords", ylab="Y coords")
  
  for (counter in 2:k)
    points(x.coords[[counter]], y.coords[[counter]], col=alpha(counter+1, .4))
}

# Get the clustroid (the point in a cluster nearest to all other points, the "centroid" in a non-Euclidean space)
# n = number of points in cluster
GetClustroid <- function(n) {
  n <- n * 2
  p <- matrix(runif(n), ncol=2, byrow=T)
  p
  distances <- list()
  min_temp_dist <- 0.0
  old_temp_dist <- Inf
  
  for (counter in 1:nrow(p)) {
    centroid <- c(p[counter,1],p[counter,2])
    temp_dist <- 0.0
    
    for (counter2 in 1:nrow(p)) {
      if (counter2 != counter) {
        temp_dist <- temp_dist + sqrt((c(p[counter2,1],p[counter2,2]) - centroid) %*%
                                        (c(p[counter2,1],p[counter2,2]) - centroid))
      }
    }
    
    if (temp_dist < old_temp_dist) {
      min_temp_dist <- c(p[counter,1],p[counter,2])
      old_temp_dist <- temp_dist
    }
    distances[[counter]] <- temp_dist
  }
  
  distances
  plot(p, pch=21, bg="cyan", col="black", main="Clustroid test", cex.main=1, cex.lab=.7,
       cex.axis=.7, xlab="X", ylab="Y", cex=1.8, xlim=c(min(p[,1])-.1, max(p[,1])+.1),
       ylim=c(min(p[,2])-.1, max(p[,2])+.1))
  points(min_temp_dist[1], min_temp_dist[2], pch=21, bg="orange", col="black", cex=3)
  points(min_temp_dist[1], min_temp_dist[2], pch=21, bg="cyan", col="cyan", cex=1.4)
}
