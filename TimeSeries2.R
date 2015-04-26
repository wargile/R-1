# Time series: Forecasting, principles and practice
# https://www.otexts.org/fpp
# http://www.stat.pitt.edu/stoffer/tsa3/ (Code for Book: Time Series analysis and its Applications)
# http://www.r-bloggers.com/forecasting-weekly-data/
# http://www.r-bloggers.com/time-series-analysis-and-mining-with-r/ (see also http://www.rdatamining.com/)

package.install("fpp")
library(fpp)

par(mar=c(3,3,2,.8))
par(cex.lab=.7)
par(cex.axis=.7)
par(cex.main=.8)
par(cex=1)
par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot

years <- 3
months <- 12

adding <- cumsum(seq(1, years*months, 1))/100
adding
x <- round(sin(seq(1, years * months, 1)), 2) + adding
x <- 1 - round(log(sin(seq(1, years * months, 1)) + 1), 2)
x

# Forecasting - nice one:
par(mfrow=c(2,1))
my.ts <- ts(x, start=c(2012,1),freq=12)
plot(x, type="l", col="blue", lwd=2, main="Forecast test - input data")
result <- forecast(my.ts)
plot(result, col="blue", lwd=2, xlab=NA)
par(mfrow=c(1,1))
par(mar=oldmar)

# -----------------------------------------------------------------------------------------
# Turn a weekly time range into a daily time series
IncreaseDate <- function(dateFrom, dateTo) {
  dateDiff <- as.Date(dateTo) - as.Date(dateFrom)
  datesArray <- character()

  for (pos in 0:(as.integer(dateDiff))) {
    datesArray[pos + 1] <- as.character(as.Date(dateFrom) + pos)
  }
  
  datesArray
}

theDates <- IncreaseDate("2012-01-24", "2012-03-01")
theDates

# -----------------------------------------------------------------------------------------
ts1 <- ts(x, start=c(2011,1), end=c(2013,12), frequency=months)
ts1

plot.ts(ts1, xlim=c(2011.1, 2015.0), axes=F, frame=T, ylim=c(-1, max(x)+1), cex.axis=.7,
        col="blue", lwd=2, main="Time series", ylab="Data")
axis(2, cex.axis=.8) 
axis(1, cex.axis=.8)
lines(meanf(ts1, h= 12)$mean, col="green4", lwd=2)
lines(naive(ts1, h=12)$mean, col="red", lwd=2)
lines(snaive(ts1, h=12)$mean, col="cornflowerblue", lwd=2)
lines(rwf(ts1, drift=T, h=12)$mean, col="gray", lwd=2)
legend("topleft",lty=1,lwd=3,cex=.8,col=c("green4", "red", "cornflowerblue","gray"),
       legend=c("Mean","Naive","Seasonal naive","Drift"))

# time series stuff...
x <- seq(1, 36, 1)
ts1 <- ts(x, start=c(1980, 1), end=c(1982, 12), frequency=12)
ts1

# BoxCox transformation:
# https://www.otexts.org/fpp/2/4
cat("BoxCox transform:\n")
y <- round(abs(rnorm(100)+1), 2)
y
cat("Param = 0:\n")
transformed <- log(y) # transform when param = 0
round(transformed, 2)
oldmar=par()$mar
par(mar=c(4.3,2,2.5,.8))
par(mfrow=c(2,1))
plot(y, type="l", ylim=c(min(transformed), max(y)), cex.axis=.8, main="Param = 0")
lines(transformed, col="blue")
exp(transformed) # back-transform when param = 0
param <- .30 # NOTE: Should always be <= 1?
cat("Param <> 0:\n")
transformed <- ((y^param) - 1) / param # transform when param <> 0
round(transformed, 2)
round((param * transformed + 1)^(1 / param), 2) # back-transform when param <> 0
plot(y, type="l",  ylim=c(min(transformed), max(y)), cex.axis=.8, main="Param <> 0")
lines(transformed, col="blue")
par(mfrow=c(1,1))
par(mar=oldmar)

# Population adjustment:
population <- 41200/1000
beds <- 2000
beds.per.thousand <- beds/population
beds.per.thousand
beds.per.thousand * population

# Inflation adjustment:
# http://en.wikipedia.org/wiki/Consumer_price_index
cpi.start <- 100
cpi.now <- 125
home.price <- 2e+5 # price in start year
home.price
new.home.price <- home.price / cpi.start * cpi.now
new.home.price # what you need to pay now for an identical house
new.home.price <- 2e+5
new.home.price.adj <- new.home.price / cpi.now * cpi.start
new.home.price.adj # what a 2e+5 price new home is worth now compared to start

# Calculate prediction interval:
# https://www.otexts.org/fpp/4/5
# prediction.interval <- TODO
n <- 10
x <- rnorm(n)
y <- rnorm(n) * x
model1 <- lm(y ~ x)
confint(model1)
conf.int.lower <- sum(model1$fitted) + 1.96 * MyResidualStandardError(model1, y, 2) *
  sqrt(1 + (1/n) + (((x - mean(x))^2) / ((n - 1)*sd(x)^2)))

# Using STL (Seasonal Decomposition of Time Series by Loess) (https://www.otexts.org/fpp/6/1):
par(mfrow=c(1,1))
fit <- stl(elecequip, s.window=5)
plot(elecequip, col="gray", main="Electrical equipment manufacturing", ylab="New orders index", xlab="")
lines(seasadj(fit),col="blue",ylab="Seasonally adjusted")
lines(fit$time.series[,2],col="red",ylab="Trend")
plot(fit)
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

# Try forecast on a sine curve:
my.ts <- ts(sin(seq(-pi,pi,.1)), frequency=12)
library(stats)
fit <- forecast(my.ts)
fit <- auto.arima(my.ts)
# TODO: From arima, to use n.ahead:
plot(fit, n.ahead=12)
# Same as:
plot(forecast(my.ts))

# Try a Bessel curve:
n <- 50
y <- besselJ(1:n, nu=2.5)
#plot(y, pch=19, col="blue", xlim=c(1, n*2))
#lines(y, col="blue")
x <- 1:n
my.ts <- ts(y, frequency=12)
fit <- forecast(my.ts)
fit <- auto.arima(my.ts)
#plot(forecast(my.ts))
plot(forecast(stl(my.ts, s.window=1))) # Much better!!
fit <- stl(my.ts, s.window=5)
lines(seasadj(fit),col="blue",ylab="Seasonally adjusted")
lines(fit$time.series[,2],col="red",ylab="Trend")

plot(fit)
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

# Try a sine curve with increase in min/max:
y <- sin(-10:10)*(1:21)
my.ts <- ts(y, frequency=12)
plot(y, pch=19, col="blue", xlim=c(1, 40))
lines(y, col="blue")

# Try a dataset from the testdata folder
data <- read.csv("C:/coding/R/TestData/US_Unemployment_NotSeasonallyAdjusted.txt", header=T, sep=",", dec=".")
head(data)
unemployment <- ts(a[,4], frequency=12, start=c(1983, 1))
plot(unemployment, col="blue", main="US Unemployment")
plot(forecast(unemployment, h=36), col="blue", main="US Unemployment Forecast")

# Some time series data sets:
# http://www.stat.wisc.edu/~reinsel/bjr-data/
# http://www.itl.nist.gov/div898/handbook/pmc/section4/pmc44a.r
# Airline passengers 1949-1960, monthly totals
airline.passengers <-
  c(112,118,132,129,121,135,148,148,136,119,104,118,
    115,126,141,135,125,149,170,170,158,133,114,140,
    145,150,178,163,172,178,199,199,184,162,146,166,
    171,180,193,181,183,218,230,242,209,191,172,194,
    196,196,236,235,229,243,264,272,237,211,180,201,
    204,188,235,227,234,264,302,293,259,229,203,229,
    242,233,267,269,270,315,364,347,312,274,237,278,
    284,277,317,313,318,374,413,405,355,306,271,306,
    315,301,356,348,355,422,465,467,404,347,305,336,
    340,318,362,348,363,435,491,505,404,359,310,337,
    360,342,406,396,420,472,548,559,463,407,362,405,
    417,391,419,461,472,535,622,606,508,461,390,432)

airline.passengers.ts <- ts(airline.passengers, start=c(1949,1), frequency=12)
plot(airline.passengers, type="o", col="blue", pch=21, bg="cyan", main="airline.passengers", cex.axis=.8)
plot(log(airline.passengers), type="o", col="green4", pch=21, bg="yellow", main="log(airline.passengers)", cex.axis=.8)
plot(diff(log(airline.passengers)), type="o", col="brown", pch=21, bg="yellow",
     main="diff(log(airline.passengers))", cex.axis=.8)

# Fit a MA model to original series. The arima function will perform the necessary differences.
ma = arima(log(airline.passengers.ts), order = c(0, 1, 1), 
           seasonal=list(order=c(0,1,1), period=12))
ma

plot(forecast(airline.passengers.ts), type="o", col="blue", pch=21, bg="cyan", main="airline.passengers.ts",
     cex.axis=.8, cex.lab=.8, cex.main=1)

Get_US_UnemploymentData <- function() {
  # http://www.bls.gov/data/
  # http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
  library(forecast)
  data <- read.csv("C:/coding/R/TestData/US_LaborForce_Unemployment.csv", header=T, sep=";")
  data.v <- as.vector(as.matrix(t(data)))
  
  data.ts <- ts(data.v[!is.na(data.v)], freq=12, start=c(2004,1))
  
  op <- par(cex.axis=.8, cex.lab=.8, cex.main=1, mfrow=c(2,1), mar=c(4,4.3,2,.8)) # NOTE: Override plot settings
  plot.ts(data.ts, ylab="Unemployment rate", cex.lab=.8, cex.axis=.8,
          cex.main=1, main="US Unemployment", col="blue")
  plot(forecast(HoltWinters(data.ts)), col="blue", ylab="Unemployment rate", main="US Unemployment (HoltWinters)")
  par(op)
}

ForecastOldFaithfulEruptions <- function() {
  data(faithful)
  faithful.order <- order(faithful$eruptions, by=faithful$waiting)
  plot(forecast(HoltWinters(ts(faithful.order[1:(2*24)], freq=24)), gamma=F, beta=F))
}

# ---------------------------------------------------------------------------------------------------------

urls = "http://freakonometrics.free.fr/report-headphones-2015.csv"
report <- read.table(urls, skip=4, header=TRUE, sep=",", nrows=585)
report.tstplot(report[,2], type="l", col="blue")
report$Date <- as.Date(report$Semaine)
report$Semaine <- NULL
head(report)

start <- min(report$Date)
end <- max(report$Date)
start <- c(year(start),month(start),4)
end <- c(year(end),month(end),15)

# as.numeric(format(xx, "%d"))
# as.numeric(format(xx, "%m"))
# as.numeric(format(xx, "%Y"))
# as.POSIXlt(xx)$mday
# as.POSIXlt(xx)$mon + 1
# as.POSIXlt(xx)$year + 1900

report.ts <- ts(report$headphones, start=start, end=end, freq=52)
plot(report.ts)
plot(forecast(report.ts))
