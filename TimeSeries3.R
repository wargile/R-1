# TimesSeries3.R
# Calculations taken from:
# https://www.youtube.com/watch?annotation_id=annotation_944828&feature=iv&src_vid=5C012eMSeIU&v=kcfiu-f88JQ

# Other timeseries/forecasting tips:
# https://www.otexts.org/fpp/
# http://analysights.wordpress.com/2010/05/06/forecast-friday-topic-moving-average-methods-2/
# http://www.personal.soton.ac.uk/rchc/Teaching/MATH6011/ForecastingChap3.htm

# Create a forecasting function
# Example: Simple moving average:
sales <- c(129, 134, 122)
sma <- sum(sales) / 3
# TODO: Add weighted moving average
weights <- c(.20, .30, .50)
sum(sales * weights) # [(122 * .50) + (134 * .30) + (129 * .20) = 127]

years <- 4
frequency <- 4 # TODO: Could this any frequency (4,12,52)?
# t = time series
t <- 1:(years * frequency)

# Sales data with a seasonal trend
sales <- seq(-pi*4, pi*4, by=pi/2)
sales <- sales[1:length(sales)-1]
sales <- sin(sales) * log(1:length(sales))
#plot(sales, type="b", col="blue")

sales <- c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 7.5, 7.8, 6.3, 5.9, 8.0, 8.4) # Upward trend
sales <- c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 5.9, 4.9, 4.3, 3.2, 4.0, 4.4) # Up, then down
sales <- c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 5.9, 4.9, 4.3, 3.2, 4.0, 4.4) * -1 # Down, then up
sales <- sales * -1.5 # Create a downward trend
#sales <- sales * ((years * frequency):1) # Modify, smooth it out
sales <- sales * (1:(years * frequency)) # Modify, increase

#sales <- besselJ(1:(years * frequency), nu=1)
#sales <- sin(1:(years * frequency)) * ((years * frequency):1)

df <- data.frame(t=t, frequency=rep(c(1:frequency), years), sales=sales)
df
plot(df$sales, type="b", col="blue", pch=21, bg="cyan", main="Car Sales Time Series",
     xlab="Frequency", ylab="Sales", xaxt="n", cex.axis=.6, cex.lab=.8, cex.main=1)
axis(side=1, at=seq(16), cex.axis=.6)

# Moving average
ma <- rep(NA, (years * frequency))
for (counter in 3:(length(sales)-1)) {
  ma[counter] <- mean(sales[(counter - 2):(counter + 1)])
}
ma
df$ma <- ma
lines(ma, col="red")

# Centered moving average
cma <- rep(NA, (years * frequency))
for (counter in 3:(length(sales)-1)) {
  cma[counter] <- mean(ma[counter:(counter + 1)])
}
cma
df$cma <- cma
lines(cma, col="green")

# Seasonal and Irregular component
df$StIt <- (df$sales / df$cma)

# The mean of the same quarter in all the years 
meanfrequency <- numeric(frequency)
meanfrequency[1] <- mean(df$StIt[df$frequency == 1], na.rm=T)
meanfrequency[2] <- mean(df$StIt[df$frequency == 2], na.rm=T)
meanfrequency[3] <- mean(df$StIt[df$frequency == 3], na.rm=T)
meanfrequency[4] <- mean(df$StIt[df$frequency == 4], na.rm=T)

# Seasonal component
df$St <- rep(meanfrequency, years)
# De-seasonalized
df$DS <- df$sales / df$St

# Get the trend by linear regression on DS and t
fit <- lm(df$DS ~ df$t)
summary(fit)

# Trend
df$Tt <- fit$coeff[1] + fit$coeff[2] * df$t
# Forecast
df$forecast <- df$St * df$Tt
df
lines(df$ma, col="red")
lines(df$cma, col="green")
lines(df$forecast, col="orange", lwd=2)

# All base data in place. Make a <N> year ahead forecast:
# Create new rows for the forecast. t, quarter, St, Tt and Forecast must have new values for the forecast period
years.new <- 2
t.new <- ((frequency * years) + 1):((frequency * years) + (years.new * frequency))
frequency.new <- rep(1:frequency, years.new)
sales.new <- rep(NA, frequency * years.new)
ma.new <- rep(NA, frequency * years.new)
cma.new <- rep(NA, frequency * years.new)
StIt.new <- rep(NA, frequency * years.new)
St.new <- rep(meanfrequency, years.new)
DS.new <- rep(NA, frequency * years.new)
Tt.new <- fit$coeff[1] + fit$coeff[2] * t.new
forecast.new <- St.new * Tt.new
df.new <- data.frame(t=t.new, frequency=frequency.new, sales=sales.new, ma=ma.new, cma=cma.new,
                     StIt=StIt.new, St=St.new, DS=DS.new, Tt=Tt.new, forecast=forecast.new)
df.new 
df <- rbind(df, df.new)

plot(df$sales, type="b", col="blue", pch=21, bg="cyan", main="Car Sales Seasonal Time Series", cex.axis=.6,
     cex.lab=.8, cex.main=1,
     xlab="frequency", ylab="Sales", xaxt="n", ylim=(c(min(df$forecast)-1, max(df$forecast))))
axis(side=1, at=seq(1,length(df$t), by=1), cex.axis=.6, labels=rep(1:frequency, years + years.new))
lines(df$ma, col="red")
lines(df$cma, col="green")
lines(df$forecast, col="orange", lwd=1)

ggplot(data=df, aes(x=t, y=sales)) + geom_line(col="blue") + geom_point(col="blue", size=3, , na.rm=T) + 
  geom_line(aes(x=t, y=cma), col="green4", sixe=.7, linetype=2) +
  geom_line(aes(x=t, y=ma), col="red", size=.7, linetype=2) +
  geom_line(aes(x=t, y=forecast), col="orange", size=1) +
  theme(plot.title=element_text(size=24, color="steelblue4")) + ylab("Sales") +
  ggtitle("Car Sales Seasonal Time Series") +
  scale_x_discrete("Frequency", labels=rep(1:frequency, years + years.new)) + theme_bw()

# Compare:
plot(forecast(ts(sales, start=2007, frequency=frequency)))
plot(forecast(HoltWinters(ts(sales, start=2007, frequency=frequency)), seasonal="additive"))
plot(forecast(HoltWinters(ts(sales, start=2007, frequency=frequency)), start.periods=frequency,
              seasonal="multiplicative"))
# TODO: Check https://www.otexts.org/fpp/7/5
