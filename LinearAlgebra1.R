# Solve a system of equations:
# NOTE: A must be a square matrix (so fill out with 1's in missing columns?)
# https://www.youtube.com/user/NaaniNotes/videos
# https://www.youtube.com/watch?v=kpzIxQbLhME
# https://www.youtube.com/watch?v=JLpO5M9FCic
# # http://stackoverflow.com/questions/15385063/easiest-way-to-plot-inequalities-with-hatched-fill

SetStandardOptions()

# (x = 3)
# 2x + 6 = 12
# 3x + 4 = 13
# x + 9 = 12
# Solve for Ax = b:
A = matrix(c(1, 2, 6, 1, 3, 4, 1, 1, 9), ncol=3, byrow=T)
A # Ax - a matrix of the coefficients A
x = c(12, 13, 12) # b - a vector with the results
solve(A, x)

# (x = 5)
# 3x + 6 = 21
# -4x + 4 = -16 
# Solve for Ax = b:
A = matrix(c(3, 6, -4, 4), ncol=2, byrow=T)
A # Ax - a matrix of the coefficients A
x = c(21, -16) # b - a vector with the results
solve(A, x)

# (x = 2)
# 2x^2 + 6x + 4 = 24
# x^2 + 15x - 3 = 31
# -3x^2 + 6x + 4 = 4
A = matrix(c(2, 6, 4, 1, 15, -3, -3, 6, 4), ncol=3, byrow=T)
A # Ax - a matrix of the coefficients A
x = c(24, 31, 4) # b - a vector with the results
solve(A, x)

# Graph inequalities:
# https://www.youtube.com/watch?v=Xepsjs4AV3Y
# y <= x - 2
# y = x - 2
x <- seq(-10, 10, .5)
y <- x - 2
plot(x, y, col="blue", pch=21, type="o", bg="cyan", main="Inequalities")
abline(h=0, v=0, col="lightgray", lwd=2)
grid()
# Do testpoint 1: (x,y)=(0,0)
# 0 <= 0 - 2: False. So x = 0 is not part of the solution
# Do testpoint 2: (x,y)=(5,0)
# 0 <= 5 - 2: True. So x = 5 is part of the solution. Shade the part that contains x = 5

# (x = 3)
x <- seq(-10, 10, .5)
y1 = 2*x + 6
y2 = 3*x + 4
y3 = x + 9
plot(x, y1, col="blue", pch=21, type="o", bg="cyan", main="Inequalities")
points(x, y2, col="red", pch=21, type="o", bg="yellow")
points(x, y3, col="green4", pch=21, type="o", bg="lightgreen")
abline(h=0, v=0, col="lightgray", lwd=2)
grid()

# TODO: Try to create and image with inqualities:
totalCapacity <- 166
discountDemand <- 150
regularDemand <- 100
x <- seq(1, totalCapacity, 1)
y <- seq(1, totalCapacity, 1)
ineq1 <- (x + y <= 166)
ineq1 <- ifelse(ineq1 == T, 0.25, 0)
ineq2 <- (x <= discountDemand) & (y <= regularDemand)
ineq2 <- ifelse(ineq2 == T, 0.25, 0)
ineq3 <- (x >= 0) & (y >= 0)
ineq3 <- ifelse(ineq3 == T, 0.25, 0)
# http://stackoverflow.com/questions/15385063/easiest-way-to-plot-inequalities-with-hatched-fill

x <- seq(-2, 2, .1)
y = (2/3)*x + (4/7)
intercept <- 4/7
# Find common deniminator (21). Multiply numerator by 7 and 3, respectively:
y = ((2*7)*x + (4*3)) / 21 
# Result: 21y = 14x + 12, or: -14x + 21y = 12
plot(x, y, main=expression((2/3)*x + (4/7)))
abline(h=0, v=0, col="lightgray", lwd=4)
abline(h=intercept, col="cyan", lwd=2)
points(x, y, pch=21, type="o", bg="cyan", col="blue")
grid()

plot(0:totalCapacity, seq(totalCapacity, 0, -1), type="h", col="blue")
