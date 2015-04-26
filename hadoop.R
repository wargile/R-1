# Hadoop stuff

# Data generated with pig script, see c:/coding/hadoop

# US goods and services import versus export:
data <- read.csv("c:/coding/R/testdata/export_import.csv", sep="\t", header=T)
plot(goods ~ year, type="l", data=data, col="blue", main="Trend  export/import US goods and services 1970-2012",
     xlab="Year", ylab="Goods and services",
     cex.lab=.8, cex.axis=.7, cex.main=.9, ylim=c(min(data$goods), max(data$services)))
grid()
lines(services ~ year, type="l", data=data, col="red")
lines(goods ~ year, type="l", data=data, col="blue")
legend("bottomleft", legend=c("Export versus import of goods","Export versus import of services"),
       col=c("blue","red"), lwd=2, cex=.8)

# Prepare IIS logs:
iisdata <- read.csv("C:/coding/R/TestData/ex090324_2.log", header=T, sep=" ")
head(iisdata, n=1)
