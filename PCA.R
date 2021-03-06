# PCA
# http://ufldl.stanford.edu/wiki/index.php/PCA
# http://www.theanalysisfactor.com/covariance-matrices/
# http://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues?newsletter=1&nlcode=329536%7c673f
# http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf

# Also look at: PCR (Principal Component Regression) and PLS (Partial Least Squares). See Kuhn.

# TODO: Find some good data to do PCA on. Images or other stuff from TheAnalyticsEdge? 

# NOTE: Features should be scaled before doing PCA (feature<i> - mean(features))

n <- 50
x1 <- rnorm(1:n, sd=2, mean=0)
x2 <- x1 + rnorm(1:n, sd=2, mean=0)
x3 <- x1 + rnorm(1:n, sd=2, mean=0)
x4 <- x1 + rnorm(1:n, sd=2, mean=0)
plot(x1, x2, pch=4, col="blue")
# Create a matrix where the predictors are the cols
A <- matrix(c(x1,x2,x3,x4), ncol=4)
image((1/(n-1)) * (A %*% t(A)))
means.vector <- colMeans(A) # The mean vector is often referred to as the centroid 

# Visual inspection: Principal direction = x1, secondary direction = x2
# Find covariance matrix:
# http://en.wikipedia.org/wiki/Covariance_matrix
cov.matrix.x1 <- (1/n) * (A %*% t(A))
cov.matrix.x2 <- (1/n) * (x2 %*% x2)
cov(x1, x2)

cov.matrix <- (1/(n-1)) * (x1 - mean(x1)) %*% t(x2 - mean(x2))
# cov matrix also known as dispersion (or dispersion matrix)

cov.x1.x2 <- (1/(n-1)) * (A[,1] - mean(A[,1])) %*% (A[,2] - mean(A[,2]))
# Same as:
cov(A[,1], A[,2])

cov.x1.x2 <- (1/(n-1)) * (t(A - mean(A)) %*% (A - mean(A))) 
# (Almost) same as:
cov(A, A)
cor(A)
var(A)

image(cov(A, A))
image(cov.x1.x2)
image(cov.matrix)

eigen(cov.matrix)[1]

(t(A) %*% A)^(-1) # Note: Matrix must be square to produce an identity matrix!

# Find the covariance matrix
ones <- matrix(c(1,1,1,1), ncol=1)
ones
mean_vector <- (1/n) * (A %*% ones)
mean_vector
mean_matrix <- ones %*% t(mean_vector);
mean_matrix
cov_matrix <- (1/(n-1)) * (t(A - t(mean_matrix)) %*% (A - t(mean_matrix)));
cov_matrix
image(cov_matrix, main="Covariance Matrix", cex.axis=.8, cex.main=1)
