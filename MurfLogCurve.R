# Some MuRF curve testing with log stuff....

resolution <- 100
e <- exp(1)
#interval <- (e - 1) / (resolution - 1)
interval <- e / (resolution - 1)
volt <- 5
startPoint <- 1
endPoint <- e

# Note: interval * Constant - Constant can not be larger than resolution?

# Do some falling curves:
plot((1 - log(seq(startPoint, endPoint, interval))) * volt,
     ylim=c(-1, volt + 1), pch=21, col="black", bg="blue",
     main="MuRF Log Stuff...", ylab="Volt", xlab="Resolution")
points((1 - log(seq(startPoint, endPoint, interval * 2))) * volt, pch=21, col="black", bg="red")
points((1 - log(seq(startPoint, endPoint, interval * 4))) * volt, pch=21, col="black", bg="green")
points((1 - log(seq(startPoint, endPoint, interval / 2)[1:resolution])) * volt, pch=21,
       col="black", bg="cyan")

# Do a rising curve:
points((1 - log(seq(endPoint, startPoint, -interval * 2))) * volt, pch=21, col="black", bg="red")

grid()

DoCurve <- function(resolution, volt, startPoint, endPoint, curve.factor, pot.pos) {
  interval <- (e - 1) / resolution
  the.range <- seq(startPoint, endPoint, interval * curve.factor)[1:resolution]
  
  plot((1 - log(the.range)) * volt,
       ylim=c(-1, volt + 1), pch=21, col="black", bg="cyan",
       main="MuRF Log Stuff...", ylab="Volt", xlab="Resolution")
  #lines((1 - log(the.range)) * volt, pch=21, col="black")
  
  # Example: Find a single volt value in the curve at pot.pos:
  volt.value <- (1 - log(startPoint - (interval * curve.factor) + ((interval * curve.factor) * pot.pos))) * volt
  points(pot.pos, volt.value, col="red", pch=19)
  grid()
  abline(v=pot.pos, col="red", lty=3, lwd=2)
  return (volt.value)
}

DoCurve(resolution, volt, startPoint, endPoint, .5, 50)
DoCurve(resolution, volt, startPoint, endPoint, 1, 25)
DoCurve(resolution, volt, startPoint, endPoint, 2.2, 27)
DoCurve(resolution, volt, startPoint, endPoint, 4, 3)

# Curve for rise/fall through resolution?
rise.fall.interval <- (pi * 2) / resolution
sequence.rise <- cos(seq(-pi, pi, rise.fall.interval)) + 1 
sequence.fall <- (cos(seq(-pi, pi, rise.fall.interval)) + 1) * 1.5 

# Add a slight offset to the right side of the curve:
sequence.rise.offset <- sequence.rise
sequence.fall.offset <- sequence.fall

offset <- .01
for (counter in 1:resolution + 1) {
  pos.offset <- (offset * (counter - 1))
  sequence.rise.offset[counter] <- sequence.rise.offset[counter] + pos.offset
  sequence.fall.offset[counter] <- sequence.fall.offset[counter] + pos.offset
}

plot(sequence.rise, col="blue", ylim=c(0, 4),
     ylab="Rise/Fall", xlab="Chosen Resolution", main="Rise/fall curve")
points(sequence.fall, col="red")

points(sequence.rise.offset, col="green")
points(sequence.fall.offset, col="green")

grid()

