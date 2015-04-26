# Statistics 101:
# http://www.youtube.com/user/BCFoltz/videos

# Function to calculate expected value E(X) for dice rolls
# E(X) = 3.5
roll.dice <- function(times=6) {
  prob <- 0
  max.pips <- 6.0
  
  for (i in 1:times) {
    pips <- sample(1:max.pips, 1)
    #cat(pips, "\n")
    prob <- (prob + (pips * (1/max.pips)))
  }
  
  return(max.pips * (prob/times))
}

# Create a convergence graph for dice rolls
roll.dice.create.convergence.graph <- function(times=100) {
  result <- numeric()
  temp.result <- numeric()
  dice.ex <- sum(sapply(1:6, function(x) (x * (1/6))))

  for (i in seq(1, times, ceiling(times / 1000))) {
    result <- append(result, roll.dice(i))
  }
  
  data <- data.frame(rolls=seq(1, times, ceiling(times / 1000)), convergence=result)
  
  ggplot(data, mapping=aes(x=rolls, y=convergence)) +
    geom_line(mapping=aes(x=rolls), y=dice.ex, color="seagreen1", size=1.5, alpha=.4) + geom_line(color="blue") +
    ylim(min(data$convergence), max(data$convergence)) + ggtitle("Roll dice, convergence towards E(X)=3.5")
}


# Create a histogram from colorwheel spins
# Randomness increases as sample size increases = higher reliability in measurement
create.colorwheel.histogram <- function() {
  # http://stackoverflow.com/questions/6200088/multiple-histograms-in-ggplot2
  max.colors <- 10
  colors = c("red", "yellow", "green", "violet", "orange", 
             "blue", "pink", "cyan", "seagreen2", "brown")

  # Arrange histograms
  layout(matrix(1:10, 5, 2, byrow=TRUE))
  
  my.list <- list()
  pos <- 1
  #old.par <- par()
  #par(mfrow=c(5, 2))
  #par(mfcol=c(2, 5))
  par(bg="cornsilk")
  par(mar=c(2.8, 1.8, 1.8, 1.8))
  
  for (i in seq(10, 1010, 100)) {
    result <- numeric()
    cat(i, "\n")
    
    for (p in 1:i) {
      color.result <- sample(1:max.colors, 1)
      result <- append(result, color.result)
    }
    
    my.list[[pos]] <- hist(result, col=colors)
    hist(result, col=colors)
    pos <- pos + 1
  }
  
  #args.list <- c(my.list, "nrow = 5", "ncol = 2")
  args.list <- c(my.list, list(nrow=5, ncol=2))
  
  names(my.list) <- c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten")
  do.call(grid.arrange, args.list)
  
  #par(old.par)
}


# Get Expected Value E(X)
GetEX <- function(p=0, x=0) {
  #p <- c(0.1, 0.25, 0.35, 0.2, 0.1)
  #x <- c(1, 2, 3, 4, 5)
  
  
  ex <- sum(p * x)
  ex
}

# Get probability (in standardized z-score) of a sample from a normal distribution
GetProbabilityOfSample <- function(sample, dist) {
  # The result is in z-score(?)
  return((sample - mean(dist)) / sd(dist))
}
