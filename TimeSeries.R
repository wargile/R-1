# Time Series (Springer)

# http://www.stat.pitt.edu/stoffer/tsa3/

package.install("astsa")
library(astsa)

download.file("http://www.stat.pitt.edu/stoffer/tsa3/tsa3.rda", destfile="c:/coding/R/testdata/tsa3.rda")
head(eqexp) # econ5, beamd, soiltemp, climhyd, etc.
plot(jj, type="o", ylab="Quarterly Earnings per Share") # Johnson&Johnson shares
plot(gtemp, type="o", ylab="Global Temperature Deviations")
plot(speech)
# Use ARCH and GARCH models to handle volatility:
plot(nyse, ylab="NYSE Returns")
# Transfer function modeling (comparing two time series), chapter 5:
par(mfrow = c(2,1))
oldmar=par()$mar
par(mar=c(3,2.5,2,1))
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")
par(mfrow = c(1,1))

# CH 1.3 - Linear filtering of time series, Moving Average:
w = rnorm(500,0,1) # 500 N(0,1) variates
v = filter(w, sides=2, rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")
plot.ts(v, main="moving average")
par(mfrow=c(1,1))

# CH 1.3 - Autoregressions:
w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main="autoregression")

# CH 1.3 - Random Walk:
set.seed(154) # so you can reproduce the results
w = rnorm(200,0,1); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk")
lines(x); lines(.2*(1:200), lty="dashed")

# CH 3.1 - Signal in noise:
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))

par(mar=oldmar)

# Rob Hyndman Lectures: https://www.otexts.org/fpp/
install.packages("fpp", dependencies=TRUE)
library(fpp)
# Average/Mean value forecasting (https://www.otexts.org/fpp/2/3):
# \hat{y}_{T+h|T} = \bar{y} = (y_{1}+\dots+y_{T})/T.
x <- rnorm(10)
meanf(x, h=10)
# Naïve forecasting:
# This method is only appropriate for time series data. All forecasts are simply set to be
# the value of the last observation. That is, the forecasts of all future values are set
# to be yT, where yT is the last observed value. This method works remarkably well for
# many economic and financial time series.
naive(x, h=10)
rwf(x, h=10)
# Seasonal naïve forecasting:
# y_{T+h-km} \text{ where $m=$ seasonal period, $k=\lfloor (h-1)/m\rfloor+1$,}
# A similar method is useful for highly seasonal data. In this case, we set each forecast
# to be equal to the last observed value from the same season of the year (e.g., the same
# month of the previous year). 
snaive(x, h=10)
# Drift:
# y_{T} + \frac{h}{T-1}\sum_{t=2}^n (y_{t}-y_{t-1}) = y_{n} + h \left( \frac{y_{T} -y_{1}}{T-1}\right).
rwf(x, drift=T, h=10)

# Example with methods above:
beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2,h=11)
beerfit2 <- naive(beer2,h=11)
beerfit3 <- snaive(beer2,h=11)
plot(beerfit1, plot.conf=FALSE,main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
legend("topright",lty=1,col=c(4,2,3),legend=c("Mean method","Naive method","Seasonal naive method"))

beer3 <- window(ausbeer, start=2006)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

dj2 <- window(dj,end=250)
plot(dj2,main="Dow Jones Index (daily ending 15 Jul 94)",ylab="",xlab="Day",xlim=c(2,290))
lines(meanf(dj2,h=42)$mean,col=4)
lines(rwf(dj2,h=42)$mean,col=2)
lines(rwf(dj2,drift=TRUE,h=42)$mean,col=3)
legend("topleft",lty=1,col=c(4,2,3), legend=c("Mean method","Naive method","Drift method"))

# 2.6 Residual diagnostics:
dj2 <- window(dj, end=250)
plot(dj2, main="Dow Jones Index (daily ending 15 Jul 94)", ylab="", xlab="Day")
res <- residuals(naive(dj2))
plot(res, main="Residuals from naive method", ylab="", xlab="Day")
Acf(res, main="ACF of residuals")
hist(res, nclass="FD", main="Histogram of residuals", col="cornflowerblue")

# Box test, lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
# Box-Ljung test:
Box.test(res,lag=10, fitdf=0, type="Lj")
