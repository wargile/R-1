CreateMovingAverage <- function(data, window) {
  if (window %% 2 == 0) {
    window.start <- (window / 2) - 1
    window.end <- window / 2
  } else {
    window.start <- floor(window / 2)
    window.end <- floor(window / 2)
  }
 
  return (sapply(1:length(data), function(x) ifelse(x > window.start, mean(data[(x-window.start):(x+window.end)]), NA)))
}

a <- rnorm(50)
smooth1 <- CreateMovingAverage(a, 3)
smooth2 <- CreateMovingAverage(smooth1, 5)
plot(a, type="l", col="blue")
lines(smooth1, col="red", lwd=2)
lines(smooth2, col="green3", lwd=2)
legend("topright", legend=c("Data","Smoother1","Smoother2"), col=c("blue","red","green3"), lwd=2, cex=.7)


# Get first derivate...???
a1 <- abs(rnorm(100))
a2 <- abs(rnorm(100)) + a1
a3 <- abs(rnorm(100)) + a2
a4 <- abs(rnorm(100)) + a3
a5 <- abs(rnorm(100)) + a4
df <- data.frame(V1=a1, V2=a2, V3=a3, V4=a4, V5=a5)
df

df.der <- df - cbind(NA, df)[, -(dim(df)[2]+1)]
df.der[, -1]

plot(df$V1, type="l")
lines(df.der$V2, col="red")

x <- rnorm(100)
y <- numeric(0)
a <- .7
for (n in 2:100)
  y[n] = a * x[n] + (1 ??? a) * y[n ??? 1]
y
plot(x, type="l")
lines(y, col="red")

hann <- 0.5 * (1 - (cos((2*pi*x) / (length(x)-1))))
plot(x, type="l")
lines(hann, col="red")

# ----------------------------------------------------------------------------------------------------------
# Branch and Bound
# http://www.youtube.com/watch?v=slayHO7gKEQ

# State space:
# 0/1 Knapsack problem: Given n items with benefits b1,b2...bn and weights w1,w2...wn and maximum
# capacity of knapsack M. ("0/1" means "take or not take an item")

# Find the items that should be choosen that maximises benefit. NOTE: You can either choose or
# not choose an item. The constraint: The weight of the items should never be more than M.

M <- 16
b <- c(45,30,45,10)
w <- c(3,5,9,5)
bw <- b/w
# There are 16 leaves, 16 paths, 16 possible solutions.
# Can use 2^n where n = 4 in this esxample (2 because 0 or 1 is 2 possible outcomes)
n <- length(b)
2^n
# x = array of length n of 0 or 1
# maximize: sum(i = 1:n) for b<i> * x<i>
# constraint: sum(i = 1:n) for w<i> * x<i> <= M

# http://www.youtube.com/watch?v=R6BQ3gBrfjQ

# If b = 0 and w = 0 (nothing chosen, root node) the benefit is found by fractional knapsack problem:
# A greedy algorithm where we choose item based on the best benefit/weight ration (= bw).
# So, find sum(b)/sum(w)

