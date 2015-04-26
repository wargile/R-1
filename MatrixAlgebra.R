col1 <- c(3,3,2,4,4,5,2,3,5,3)
col2 <- c(2,2,4,3,4,4,5,3,3,5)
col3 <- c(3,3,4,4,3,3,4,2,4,4)
n <- length(col1)

# Use:
X <- matrix(c(col1, col2, col3), nrow=n, ncol=3)
X
# Or use:
X <- cbind(col1, col2, col3)
X

getColSums <- matrix(c(1,1,1,1,1,1,1,1,1,1), nrow=1)
rowVectorOfMeans <- (getColSums %*% X) * n^(-1) # NOTE: n^-1 = 1/n
rowVectorOfMeans

getMeanCols <- matrix(c(1,1,1,1,1,1,1,1,1,1))
matrixOfMeans <- getMeanCols %*% rowVectorOfMeans
matrixOfMeans

matrixOfDeviationScores <- (X - matrixOfMeans)
matrixOfDeviationScores

# Matrix of Sum of Squares and Sum of Cross Products
# SS is in the diagonal, and SP is in the off-diagonals (what is that??)
SS.and.SP.matrix <- t(matrixOfDeviationScores) %*% matrixOfDeviationScores
SS.and.SP.matrix
Var.and.Cov.matrix <- SS.and.SP.matrix * n^-1
Var.and.Cov.matrix

# Need to first multiply Var.and.Cov.matrix with sd matrix: Sxx = (Diag(Cxx))^1/2
# inverted SD matrix (results in 1/SD values on the diagonal, everything else is 0)
diag.SD.matrix <- diag((diag(Var.and.Cov.matrix))^-0.5, nrow=ncol(X))
diag.SD.matrix

# Get correlaton matrix: 
# library(matrixcalc) # For matrix.inverse, if needed
corr.matrix <- diag.SD.matrix %*% Var.and.Cov.matrix %*% diag.SD.matrix
corr.matrix
# 1,                cor(col1, col2),  cor(col1, col3)
# cor(col2, col1),  1,                cor(col2, col3)
# cor(col3, col1),  cor(col3, col2),  1
# Check:
cor(X)

# Check it: (??)
summary(lm(col1 ~ col2 + col3))
cor(col1, col2)
cor(col1, col3)
cor(col2, col3)

# -------------------------------------------------------------------------------------------------------------------
# Finding the inverse of a matrix
# https://www.khanacademy.org/math/precalculus/precalc-matrices/inverting_matrices/e/matrix_inverse_3x3
A <- matrix(c(-1,-2,2,2,1,1,3,4,5), ncol=3, byrow=T)
minor.matrices <- list()
determinants <- list()
pos <- 1

for (counter in 1:3) {
  for (counter2 in 1:3) {
    minor.matrices[[pos]] <- A[-counter, -counter2]
    determinants[[pos]] <- (minor.matrices[[pos]][1] * minor.matrices[[pos]][4]) -
      (minor.matrices[[pos]][2] * minor.matrices[[pos]][3])
    pos <- pos + 1
  }
}

minor.matrices
determinants
# Change sign on elements (+, -, +, -, etc.), this gives you the cofactor matrix:
cofactor.matrix <- numeric(0)
for (counter in 1:9) 
  cofactor.matrix <- c(cofactor.matrix, determinants[[counter]] * ifelse(counter %% 2 == 0, -1, 1))
cofactor.matrix
inverse.matrix.of.A <- matrix(cofactor.matrix, ncol=3, byrow=T)
# Transpose the matrix:
inverse.matrix.of.A <- t(inverse.matrix.of.A)
# Get the determinant of the original matrix (det(A)):
inverse.matrix.of.A <- (1/det(A)) * inverse.matrix.of.A 
inverse.matrix.of.A

MatrixAlgebra <- function() {
  # Find all combinations of two table rows:
  R <- matrix(c(1,2,3))
  S <- matrix(c(4,5,6))
  # Do cross product (each tuple in R1 with each tuple in R2): R x S:
  result <- R %*% t(S)
  as.vector(result)
}
