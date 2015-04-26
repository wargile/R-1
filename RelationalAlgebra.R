# Relational algebra example

elements <- 12*12
x <- rbinom(elements, 0:2, prob=c(.075,.01,.15))
x
x <- sample(0:6, elements, replace=T, prob=c(.95,.01,.01,.01,.01,.005,.005))
x

a <- matrix(x, ncol=sqrt(elements))
a
b <- numeric()
#b

for (i in 1:nrow(a)) {
  for (j in 1:ncol(a)) {
    if (a[i,j] != 0) {
      b <- append(b,i) # or: b <- c(b,i)
      b <- append(b,j)
      b <- append(b,a[i,j])
    }
  }
}

#b
b <- matrix(b, ncol=3, byrow=T)
b

createMatrix <- function(old.matrix, rowcol.matrix) {
  new.a <- matrix(0, ncol=ncol(old.matrix), nrow=nrow(old.matrix))
  #new.a
  dim(new.a)

  for (counter in 1:nrow(rowcol.matrix)) {
    if (rowcol.matrix[counter, 3] != 0) {
      new.a[rowcol.matrix[counter, 1], rowcol.matrix[counter, 2]] <- rowcol.matrix[counter, 3]
    } else {
      new.a[rowcol.matrix[counter, 1], rowcol.matrix[counter, 2]] <- 0
    }
  }
  
  return(new.a)
}

createMatrix(a, b)

b1 <- c(1,1,2,2,2,2,3,3,3,4,4,5,5,
       4,5,1,3,4,5,2,3,4,3,5,1,5,
       55,78,19,21,3,81,48,50,1,33,67,95,31)
b1 <- matrix(b1, ncol=3)
b1
a1 <- createMatrix(matrix(0,5,5), b1)
a1

b2 <- c(1,1,2,3,3,3,4,4,4,4,5,5,5,
       2,5,3,1,2,4,1,2,3,4,1,2,5,
       73,42,82,83,13,57,48,85,18,24,98,7,3)
b2 <- matrix(b2, ncol=3)
b2
a2 <- createMatrix(matrix(0,5,5), b2)
a2
a1 %*% a2
