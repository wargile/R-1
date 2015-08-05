# edX DelftX Pre-university calulus
# ---------------------------------
# https://courses.edx.org/courses/DelftX/Calc001x

SetStandardOptions()

PlotPoly <- function(x, y, a, b, c) {
  plot(x, y, main="Quadratic function", xlab="x", ylim=c(-2, max(y)+.5), xlim=c(min(x)-.5, max(x)+.5),
       col="blue", type="o", pch=21, bg="cyan", frame=F)
  abline(v=0, col="lightgray", lwd=2)
  abline(h=0, col="lightgray", lwd=2)
  x.position.vertex <- -b/(2*a)
  abline(v=x.position.vertex, col="cyan")
  cat("x position vertex", x.position.vertex)
  y.intercept <- c
  abline(h=y.intercept, col="cyan")
  points(x, y, col="blue", type="o", pch=21, bg="cyan") # Just add the curve points again
}

# Polynomials
# -----------

# Degree 1: Constant functions: 2x^0 = 2 (remember: x^0 = 1). A constant line through the y-intercept (the coefficient 2 here)

# Degree 2: Quadratic functions:
# Standard form:
x <- seq(-1, 3, .1)
a <- -7 # a = Wideness and orientation. Bigger a = narrower curve. Negative a = downward orientation
b <- 13 # -b/2a = x-position vertex
c <- 4 # c = y-intercept
y <- a*x^2 + b*x + c
PlotPoly(x, y, a, b, c)

# NOTE: The highest degree also tells you the maximum points that a straight line can intersect the graph
x <- seq(-1.8, 1.8, .1)
a <- 1 # a = Wideness and orientation. Bigger a = narrower curve. Negative a = downward orientation
b <- -3 # -b/2a = x-position vertex
c <- 1 # c = y-intercept
y <- (a*x^4) + (b*x^2) + c + 2
PlotPoly(x, y, a, b, c)
