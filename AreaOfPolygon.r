# http://m.wikihow.com/Calculate-the-Area-of-a-Polygon

getpolyarea <- function(poly.x, poly.y) {
  mpoly <- matrix(c(poly.x, poly.y), length(poly.x), 2)
  mpoly

  a <- numeric(1)
  b <- numeric(1)
  i <- 0

  for (i in 1:(length(poly.x)-1)) {
    a[1] <- a[1] + (mpoly[i, 1] * mpoly[(i+1), 2])
    cat("a:", a[1], "\n")
  }


  for (i in 1:(length(poly.y)-1)) {
    b[1] <- b[1] + (mpoly[i, 2] * mpoly[(i+1), 1])
  }

  return ((a - b) / 2)
}

poly.x <- c(-3,-1,6,3,-4,-3)
poly.y <- c(-2,4,1,10,9,-2)

cat("The area of the polygon is: ", getpolyarea(poly.x, poly.y))
