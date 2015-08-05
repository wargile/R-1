# From: Operasjonsanalyse Kort og Godt, paragraph 2.4
# ---------------------------------------------------
SetStandardOptions()

x <- 2
y <- 3

# Find the corner points for the following inequalities (See GeoGebra file inequalities4.ggb):
# x + y >= 6 # Minumskrav om produsert mengde
# x + 4*y >= 12 # Minumskrav om produsert mengde
# x + 2*y <= 10 # Begrensing på produksjonskapasitet

x + y >= 6
x + 4*y >= 12
x + 2*y <= 10

# Get y on the left side:
y >= -x + 6
y >= (-1/4)*x + 3
y <= (-1/2)*x + 5

# Pair up the two lines that intersect in a corner:
# Corner 1)
# -x + 6 = (-1/4)*x + 3

# -x + 1/4*x = -6 + 3
# -4*x + x = -24 + 12
# -3*x = -12
# 3*x = 12
# x = 4
x <- 4
x
y <- (-1/4)*x + 3
y # y = 2
# So: x + y >= 6 has cornerpoint (4, 2)
x1 <- x
y1 <- y

# Corner 2)
# -x + 6 = (-1/2)*x + 5

# -x + 1/2*x = -6 + 5
# -2*x + x = -12 + 10
# -x = -2
# x = 2
x <- 2
y <- (-1/2)*x + 5
y # y = 4
# So: x + 4*y >= 12 has cornerpoint (2, 4)
x2 <- x
y2 <- y

# Corner 3)
# (-1/4)*x + 3 = (-1/2)*x + 5

# (-1/4)*x + (1/2)*x = -3 + 5
# -x + 2*x = -12 + 20
# x = -12 + 20
# x = 8
x <- 8
y <- (-1/2)*x + 5
y # y = 1
# So: x + 2*y <= 10 has cornerpoint (8, 1)
x3 <- x
y3 <- y

# Plot the inequalities:
plot(12:0, (0:12)/4, type="o", col="blue", main=expression("Inequalities of Z = 2"*"x"[1]*" + "*"x"[2]),
     xlab=expression("x"[1]), ylab=expression("x"[2]), ylim=c(0, 6))
grid()
lines(10:0, (0:10)/2, type="o", col="red")
lines(6:0, 0:6, type="o", col="green4")
points(c(x1, x2, x3), c(y1, y2, y3), pch=21, bg="cyan", col="black", cex=1.4)
# Find the intercepts and plot:
# x + y >= 6
# x + 4*y >= 12
# x + 2*y <= 10
intercept.x1.x <- 6
intercept.x1.y <- 6
intercept.x2.x <- 12
intercept.x2.y <- 12/4
intercept.x3.x <- 10
intercept.x3.y <- 10/2
points(c(0, 0, 0), c(intercept.x1.y, intercept.x2.y, intercept.x3.y), pch=21, bg="yellow", col="black", cex=1.4)
points(c(intercept.x1.x, intercept.x2.x, intercept.x3.x), c(0, 0, 0), pch=21, bg="yellow", col="black", cex=1.4)
