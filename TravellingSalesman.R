# Travelling Salesman Problem

# http://en.wikipedia.org/wiki/Travelling_salesman_problem
# http://en.wikipedia.org/wiki/Hamiltonian_path
# http://en.wikipedia.org/wiki/Cutting-plane_method
# http://en.wikipedia.org/wiki/Branch_and_bound

# TODO: Optimize by doing permutations of <n> closest nodes and checking all variants before progressing?
#       Can use combinat::permn(vector.size)

TravellingSalesman <- function(cities) {
  op <- par()
  n <- cities # Cities to visit
  starting.city <- sample(1:n, 1)
  city.status <- rep(0, n)
  distances <- numeric(0)
  city.visited <- 1
  city.start <- 2
  cities.visited <- integer(0)

  
  x <- sample(1:n, n, replace=F) + rnorm(n)
  y <- sample(1:n, n, replace=F) + rnorm(n)
  # Plot 'cities' on lat/lon 'map'
  par(mar=c(6, 4.5, 2.8, .8))
  plot(x, y, pch=21, col="blue", bg="cyan", main="Travelling salesman", cex.axis=.7, cex.lab=.7,
       cex.main=1, xlab="Latitude", ylab="Longitude", cex=2.6, ylim=c(min(y)-0.5, max(y)+0.5),
       xlim=c(min(x)-0.5, max(x)+0.5), asp=1)
  
  x.current <- x[starting.city]
  y.current <- y[starting.city]
  
  # Highlight starting city
  points(x[starting.city], y[starting.city], col="orange", pch=19, cex=1.8)
  
  cities.visited <- c(cities.visited, starting.city)
  cities.to.visit <- (1:n)[-cities.visited]
  distance <- numeric(0)
  
  # Visit all cities (that is, change starting point each time, and check remaining cities)
  for (i in 1:(n - 1)) {
    old.distance <- Inf
    shortest.distance <- 0
    shortest.distance.km <- numeric(0)
    
    for (counter in cities.to.visit) {
      distance <- sqrt((c(x[counter],y[counter]) - c(x.current,y.current)) %*%
                         (c(x[counter],y[counter]) - c(x.current,y.current)))
      
      if (distance < old.distance) {
        shortest.distance <- counter
        shortest.distance.km <- distance
        old.distance <- distance
      }
    }
  
    distances[i] <- shortest.distance.km
    cities.visited <- c(cities.visited, shortest.distance)
    cities.to.visit <- (1:n)[-cities.visited]
    
    # http://stackoverflow.com/questions/5731432/change-arrowhead-of-arrows
    arrows(x0=x.current,y0=y.current,x1=x[shortest.distance],y1=y[shortest.distance],
          code=2, angle=20, length=unit(0.12, "inches"), col="blue")
    x.current <- x[shortest.distance]
    y.current <- y[shortest.distance]
  }
  
  arrows(x0=x.current,y0=y.current,x1=x[starting.city],y1=y[starting.city],
         code=2, angle=20, length=unit(0.12, "inches"), col="blue")
  title(sub=paste0(n, " cities visited. Total distance: ", round(sum(distances), 2), " miles"), cex.sub=.8)
  
  ret <-list()
  ret$x <- x
  ret$y <- y
  ret$distance <- distances
  par <- op
  return (ret)
}

# Testing permutations of <n> nodes ahead
# NOTE: Permutations gives a "double" amount of possibilities, since it includes both distances regarding from/to!
DoPermutations <- function() {
  nodes <- 5
  df <- data.frame(x=c(8.1,1,4,6,2), y=c(10,8.3,12,13.3,5.5))
  distances <- list()
  total.distances <- list()
  nodes <- nrow(df)
  # permutations <- factorial(nodes)
  permutations <- combinat::permn(nodes) # library(combinat)

  for (counter in 1:length(permutations)) {
    nv <- permutations[[counter]]
    distance <- numeric(0)
    
    for (counter2 in 1:(nodes - 1)) {
      # Euclidean distance: sqrt(((a[1] - b[1])^2) + ((a[2] - b[2])^2))
      dist <- sqrt((c(df$x[nv[counter2 + 1]], df$y[nv[counter2 + 1]]) - c(df$x[nv[counter2]], df$y[nv[counter2]])) %*%
        (c(df$x[nv[counter2 + 1]], df$y[nv[counter2 + 1]]) - c(df$x[nv[counter2]], df$y[nv[counter2]])))
      distance[counter2] <- dist
    }

    distances[[counter]] <- distance
    total.distances[[counter]] <- sum(distance)
  }

  par(mfrow=c(2,1))
  par(mar=c(3,3,2,1))
  plot(df$x, df$y, pch=19, col="blue", cex.axis=.7, cex.main=.8, cex.lab=.7, main="Cities", xlab="", ylab="",
       xlim=c(min(df$x) - .5, max(df$x) + .5), ylim=c(min(df$y) - 2, max(df$y) + 1))
  text(df$x, df$y, 1:nodes, adj=c(1.5, 1.5), cex=.7)
  minimum.dist <- which(as.numeric(total.distances) == min(as.numeric(total.distances)))
  # NOTE: The two expressions below achieves the same formatting:
  main.title <- paste("Shortest route:", paste(as.integer(permutations[[minimum.dist[1]]]), collapse=","))
  main.title <- paste("Shortest route:", toString(as.integer(permutations[[minimum.dist[1]]])))
  plot(as.numeric(total.distances), col="blue", type="o", cex.axis=.7, cex.main=.8, cex.lab=.7, main=main.title,
       xlab="", ylab="", ylim=c(min(as.numeric(total.distances)) - 1, max(as.numeric(total.distances)) + 1))
  abline(v=minimum.dist, col="red")
  par(mfrow=c(1,1))
  par(mar=c(5,5,3,1))
  total.distances
}

ret <- list()

manipulate(
  ret <<- TravellingSalesman(cities),
  cities=slider(min=3, max=100, initial=7, step=1, label="Cities to visit")
)
