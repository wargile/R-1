# Just trying out some code for the MS "moving dots" animated bitmap

SetStandardOptions()

par(mfrow=c(4,1))
power <- 3
theTime <- seq(-pi, pi, .1)^power
plot(theTime, pch=16, col="red", ylab=NA, main="The coords...")

plot(theTime - min(theTime), rep(1, length(theTime)), pch=16, col="blue",
     ylab=NA, main="The dots with power 3!")

power <- 7
theTime <- seq(-pi, pi, .1)^power # Higher pow, bigger diff
plot(theTime - min(theTime), rep(1, length(theTime)), pch=16, col="blue",
     ylab=NA, main="The dots with power 7!")

# Yet another solution:
power <- 4
the_seq1 <- seq(0, pi, .1)^power
the_seq2 <- seq(-pi, 0, .1)^power * -1
the_seq <- c(the_seq1, the_seq2)
plot(the_seq, rep(1, length(the_seq)), pch=16)
min(the_seq)
max(the_seq)
theTime <- the_seq

par(mfrow=c(1,1))

MoveTheDots <- function(counter) {
  # TODO: Need to do (- min(theTime)) on these to ensure coordinate balance?
  if (counter >= 3) {
    dots <- (c(theTime[counter], theTime[counter - 1], theTime[counter - 2]))
    cat(dots)
    plot(dots + counter, rep(1, length(dots)), pch=16, col="blue", xlim=c(min(theTime), max(theTime)))
    grid()
    return (dots)
  }
  if (counter >= 2) {
    dots <- (c(theTime[counter], theTime[counter - 1]))
    cat(dots)
    plot(dots + counter, rep(1, length(dots)), pch=16, col="blue", xlim=c(min(theTime), max(theTime)))
    grid()
    return (dots)
  }
  if (counter >= 1) {
    dots <- (theTime[counter])
    cat(dots)
    plot(dots + counter, rep(1, length(dots)), pch=16, col="blue", xlim=c(min(theTime), max(theTime)))
    grid()
    return (dots)
  }
}

dots <- numeric(0)

manipulate(
  dots <<- MoveTheDots(counter),
  counter=slider(min=1, max=length(theTime)+2, initial=1, step=1, label="Dot positions")
)
