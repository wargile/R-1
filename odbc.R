# Just testing some SQLServer data munging...

package.install("RODBC")
library(RODBC)

# connect
ch <- odbcConnect("ML")

asusRepair <- sqlFetch(ch, "dbo.ASUS_Repair", max=100, rows_at_time=10)
summary(asusRepair)
asusSale <- sqlFetch(ch, "dbo.ASUS_Sale", max=80, rows_at_time=100)
summary(asusSale)
asusSale <- asusSale[asusSale$number_sale > 0, ]

asusSale.m <- as.matrix(asusSale[, "number_sale"])
km <- kmeans(asusSale.m, 5, 10)
km
plot(asusSale.m, pch=21, bg=km$cluster + 1)
n <- 15
ssq <- numeric(n)
for (i in 1:n) ssq[i] <- sum(kmeans(asusSale.m, centers=i)$withinss)
plot(1:n, ssq, type="b", pch=21, bg="cyan", lwd=1, xlab="Number of clusters",
     ylab="Sum of squares within group",
     main="kmeans clusters", col="blue", cex.axis=.8)
# Find 'knee' in curve: (http://people.cs.umass.edu/~irwin/simplex.pdf)
# Nice!
straight_line <- seq(ssq[1], ssq[n], length.out=n)
knee <- which.max(straight_line - ssq)
abline(v=knee, col="red", lty=3)
lines(straight_line, col="blue", type="b")

# close connection
odbcClose(ch)
