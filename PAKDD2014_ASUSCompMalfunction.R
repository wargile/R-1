# ASUS Kaggle Competition (deadline: 01.04.2014)
# http://www.kaggle.com/c/pakdd-cup-2014

# Keywords here: Time Series Forecasting, Arima? Forecast package, Rob Hyndman
# IMPORTANT RESOURCE: https://www.otexts.org/fpp/resources (Rob Hyndman!)

# http://www.statmethods.net/advstats/timeseries.html
# http://cran.r-project.org/web/packages/forecast/index.html
# Good online book (Rob Hyndman): https://www.otexts.org/fpp

# Matlab example: http://www.mathworks.se/machine-learning/examples.html?file=/products/demos/machine-learning/load_forecasting/load_forecasting.html

# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/7007/how-to-use-output-targetid-mapping
# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/6980/sample-submission-benchmark-in-leaderboard/38331#post38331
# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/6935/clarification-on-the-semantics-of-the-task
# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/6949/meaning-of-module-and-component
# http://www.anc.ed.ac.uk/rbf/intro/node5.html
# http://brainmass.com/business/business-management/508590
# http://money.howstuffworks.com/sales-forecasting2.htm
# http://shop.oreilly.com/product/0636920027164.do (Sequenctial Machine Learning book 2015)

# TIPS After deadline:
# http://www.kaggle.com/c/pakdd-cup-2014/forums/t/7561/quiet-forum
# http://www.kaggle.com/blobs/download/forum-message-attachment-files/1178/asusExampleSubmission.R

# Some people asked about the negative numbers in the sales record. Here is the explanation from ASUS:
# The negative sales quantity stands for sales return. For example, -2 means two models that purchased
# previously are returned during that month.

# TIP: It seems many contestants are going down the road of time-series forecasting and ARIMA type analyses.
# Component and equipment failure is often analysed using Weibull or Log-Normal distributions, a recognised
# engineering approach to failure analysis. While it is possible to analyse the provided repair data and get
# MTTF and failure distributions for each module component combination, because the sales data is not meaningful,
# and therefore you cannot determine the proportion of components that fail, you cannot then apply this
# Time-To-Fail pattern on sold components.

# There is another strange thing in the training repair set: One Computer seems to have been repaired
# 2 month before it was even sold: Line 349988 (if the first line/header is numbered 0).
# Since there are very few records of such kind, I would recommend simply assuming a small amount of
# them are sold before 2005.

# NOTE: The 'M0' is another module. This module does not have any record in the repair data. The participants
# do not need to make prediction about this module. One possible solution is to ignore the sale records
# of 'M0', if you don't have a good idea to exploit it.

# NOTE: If you add up the multiple sales logs for a month the negative numbers go away. (SAFE TO DO??)

# NOTE: Each module-component may have more than one sale log in a given month.

# NOTE: The sum of sales for all module/component pairs are all equal! Are sales data real??
#       Use just repair data to forecast, since they also contain the sale date?

# Autoregressive integrated moving average (ARIMA) models to handle timecorrelated modeling and forecasting.

set.seed(16071962)

install.packages("fpp", dependencies=TRUE)
library(fpp)

datafolder <- "C:/coding/Kaggle/PAKDD2014_ASUSCompMalfunction/data/"

if (file.exists(paste0(datafolder, "repair.rda")) == F) {
  repair.train <- read.csv(paste0(datafolder, "RepairTrain.csv"), header=T, sep=",") #colClasses=c(....) to set default classes
  sale.train <- read.csv(paste0(datafolder, "SaleTrain.csv"), header=T, sep=",")
  output.target.id.mapping <- read.csv(paste0(datafolder, "Output_TargetID_Mapping.csv"), , header=T, sep=",")
  # TODO: Store as .rda and load from .rda
  
  # Fix funky names:
  names(repair.train) <- c("module_category", "component_category", "year_month_sale", "year_month_repair", "number_repair")
  names(sale.train) <- c("module_category", "component_category", "year_month_sale", "number_sale")
  
  head(repair.train)
  dim(repair.train)
  summary(repair.train)
  describe(repair.train)
  sapply(repair.train, class)
  table(repair.train$year_month_sale)
  table(repair.train$year_month_repair)
  table(repair.train$module_category)
  
  head(sale.train)
  dim(sale.train)
  sapply(sale.train, class)

  head(output.target.id.mapping)

  # Try to merge the two sets initially:
  merged.train <- merge(repair.train, sale.train,
                         by=c("year_month_sale", "module_category", "component_category"))
  save(repair.train, file=paste0(datafolder, "repair.rda"))
  save(sale.train, file=paste0(datafolder, "sale.rda"))
  save(merged.train, file=paste0(datafolder, "merged.rda"))
} else {
  load(paste0(datafolder, "repair.rda"))
  load(paste0(datafolder, "sale.rda"))
  load(paste0(datafolder, "merged.rda"))
}

# There seems to be rows with number repair > 0 and number_sale = 0. Remove:
error.rows <- merged.train[merged.train$number_sale == 0 & merged.train$number_repair > 0, ]
dim(error.rows)
merged.train <- merged.train[merged.train$number_sale > 0,]
# Module category M0 has never been repaired. Remove:
error.rows <- merged.train[merged.train$module_category == "M0", ]
dim(error.rows)
merged.train <- merged.train[merged.train$module_category != "M0",]
head(merged.train)

# Test: Aggregate sales and repair numbers:
sale.agg <- sale.train[sale.train$module_category == "M1" &
                         sale.train$component_category == "P02" & sale.train$year_month_sale == "2006/10", ]
head(sale.agg)
sale.agg2 <- aggregate(number_sale ~ ., data=sale.agg, sum)
head(sale.agg2)
sale.agg3 <- aggregate(number_sale ~ ., data=sale.train, sum)
head(sale.agg3)
# It seems to be correct that sale numbers for all components for a particular model in a particular month are equal!
repair.agg <- repair.train[repair.train$module_category == "M1" &
                             repair.train$component_category == "P02" & repair.train$year_month_sale == "2006/10", ]
head(repair.agg)
repair.agg2 <- aggregate(number_repair ~ ., data=repair.agg, sum)
head(repair.agg2)
# Convert sale and repair dates to Date columns and add datediff
repair.agg2$year_month_sale_c <- as.Date(paste0(repair.agg2$year_month_sale, "/01"))
repair.agg2$year_month_repair_c <- as.Date(paste0(repair.agg2$year_month_repair, "/01"))
repair.agg2$datediff <- as.integer(repair.agg2$year_month_repair_c - repair.agg2$year_month_sale_c)
head(repair.agg2)
# TEST: Could it be a linear relationship between (time repair - time sale) + number_sale and number_repair?
# fit <- lm(number_repair ~ (time repair - time sale) + number_sale) 
fit <- lm(number_repair ~ datediff, data=repair.agg2)
summary(fit)
# TODO: Get newdata from diff between max(sale_date) in train and repair_date in submission mapping table
df <- data.frame(datediff=sample(1:600, 20, replace=F))
df
result <- predict(fit, newdata=df)
round(result)
my.ts <- ts(repair.agg2$number_repair, freq=12)
plot(forecast(my.ts)) # Much better!!


# Plot repairs pr. module
repair.agg <- aggregate(number_repair ~ module_category, data=repair.train, sum)
barplot(height=repair.agg$number_repair, axes=T, legend=repair.agg$module_category, col=1:9)
par(mfrow=c(1,2))
barplot(table(repair.train$module_category), col="powderblue", main="Repairs pr. category",
        cex.axis=.8) #, xaxt="n")
#axis(1, at=1:9, lab=sort(unique(repair.train$module_category)), cex.axis=.7) 
barplot(table(sale.train$module_category), col="wheat", main="Sales pr. category", cex.axis=.8)
par(mfrow=c(1,1))

# Remove the sale.train rows that have 0 sales, are they even interesting?
dim(sale.train)
sale.train <- sale.train[sale.train$number_sale > 0, ]
dim(sale.train)

# Plot sales pr. module
sale.agg <- aggregate(number_sale ~ module_category, data=sale.train, sum)
barplot(height=sale.agg$number_sale, axes=T, legend=sale.agg$module_category, col=1:9)

# Aggregate the number of repairs (monthly average)
# Ordering rows first, any effect?
rows <- order(repair.train$module_category,
              repair.train$component_category, repair.train$year_month_sale,
              repair.train$year_month_repair)
repair.train <- repair.train[rows,]

repair.agg.by.month <- aggregate(repair.train$number_repair,
                                 by=list(repair.train$year_month_repair, repair.train$component_category,
                                         repair.train$module_category),
                                 data=repair.train, mean)
repair.agg.by.month <- aggregate(repair.train$number_repair ~ ., data=repair.train, sum)
head(repair.agg.by.month)

# Using Order:
# rows <- order(sale.train.subset$module_category, sale.train.subset$component_category)
# sale.train.subset[rows,]

# Convert to timeseries:
repair.train$year_month_repair.ts <- as.ts(sub("/", ".", repair.train$year_month_repair))
repair.train$year_month_sale.ts <- as.ts(sub("/", ".", repair.train$year_month_sale))
plot.ts(sort(repair.train$year_month_repair.ts[1:100]), col="blue")

# TODO: Merge sale and repair rows for the same module and component category first?
# Trying a subset of category "M1":
repair.train.subset <- subset(repair.train, repair.train$module_category == "M1")
sale.train.subset <- subset(sale.train, sale.train$module_category == "M1")

# Using Order:
rows <- order(sale.train.subset$module_category,
              sale.train.subset$component_category, sale.train.subset$year_month_sale)
sale.train.subset <- sale.train.subset[rows,]
rows <- order(repair.train.subset$module_category,
              repair.train.subset$component_category, repair.train.subset$year_month_sale,
              repair.train.subset$year_month_repair)
repair.train.subset <- repair.train.subset[rows,]

# Aggregate the sales number by month
# sale.train.subset.agg <- aggregate(number_sale ~., data=sale.train.subset, sum)
sale.train.subset.agg <- aggregate(sale.train.subset$number_sale,
                                   by=list(sale.train.subset$year_month,
                                           sale.train.subset$component_category), FUN=sum)
# Aggregate the repair number by month
repair.train.subset.agg <- aggregate(repair.train.subset$number_repair,
                                   by=list(repair.train.subset$component_category,
                                           repair.train.subset$year_month_repair,
                                           repair.train.subset$year_month_sale), FUN=sum)

# NOTE: The sum of sales for all module/component pairs are all equal! Are sales data real??
#       Use just repair data to forecast, since they also contain the sale date?

names(sale.train.subset.agg)
names(repair.train.subset.agg)
merged.subset <- merge(repair.train.subset, sale.train.subset.agg,
                       by=c("year_month_sale", "module_category", "component_category"))

repair.subset <- repair.train[repair.train$module_category == "M6" & repair.train$component_category == "P26", ]
sale.subset <- sale.train[sale.train$module_category == "M6" & sale.train$component_category == "P26", ]
rownames(repair.subset) <- NULL
rownames(sale.subset) <- NULL
colnames(sale.subset)[3] <- "year_month_sale"
colnames(repair.subset)[3] <- "year_month_sale"
colnames(repair.subset)[4] <- "year_month_repair"
merged.subset <- merge(sale.subset, repair.subset,
                       by=c("year_month_sale", "module_category", "component_category"))

# Add a datediff col to merged.subset:
merged.subset$datediff <- as.Date(paste0(merged.subset$year_month_repair, "/01")) -
  as.Date(paste0(merged.subset$year_month_sale, "/01"))
head(merged.subset)

head(repair.train[repair.train$module_category == "M6" & repair.train$component_category == "P16", ], n=10)

# Testing the date column conversion on a category subset:
repair.train.per.module <- repair.train[repair.train$module_category == "M3", ]
# Reorder the rows
rows <- order(repair.train.per.module$module_category,
              repair.train.per.module$component_category, repair.train.per.module$year_month_sale,
              repair.train.per.module$year_month_repair)
repair.train.per.module <- repair.train.per.module[rows, ]

# Convert col to date:
repair.train.per.module$year_month_repair <- as.character(repair.train.per.module$year_month_repair)
repair.train.per.module$year_month_sale <- as.character(repair.train.per.module$year_month_sale)

# Convert date to YYYY/MM/01:
repair.train.per.module$year_month_repair <- paste0(repair.train.per.module$year_month_repair, "/1")
repair.train.per.module$year_month_repair <- as.Date(as.character(repair.train.per.module$year_month_repair)) #, format="%Y.%m.%d")
repair.train.per.module$year_month_sale <- paste0(repair.train.per.module$year_month_sale, "/1")
repair.train.per.module$year_month_sale <- as.Date(as.character(repair.train.per.module$year_month_sale)) #, format="%Y.%m.%d")

# Create a new col with month diff between sale and repair:
repair.train.per.module$year.daydiff <- difftime(repair.train.per.module$year_month_repair, 
                                                   repair.train.per.module$year_month_sale, units="days")
hist(table(repair.train.per.module$year.daydiff), freq=T, col="powderblue", border="blue",
     xlab="Days between sale and repair", main="Sales and repairs")

par(mfrow=c(1,2))
plot(density(as.integer(repair.train.per.module$year.daydiff)), col="blue", lwd=2, main="Repair Density")
barplot(table(repair.train.per.module$year.daydiff), col="cyan")
par(mfrow=c(1,1))

plot(year_month_repair ~ year_month_sale, data=repair.train.per.module, pch=21, bg="cyan")

par(mfrow=c(1,2))
hist(table(repair.train.per.module$year_month_repair), col="orange", main="Repair month", xlab="", cex.axis=.7)
hist(table(repair.train.per.module$year_month_sale), col="cornflowerblue", main="Sale month", xlab="", cex.axis=.7)
par(mfrow=c(1,1))

# Aggregate:
# aggregate(y ~ x, data=df, sum)
repair.train.per.module.agg <- aggregate(number_repair ~ year_month_repair, data=repair.train.per.module, sum)
# NOTE: The sum of sales for all module/component pairs are all equal! Are sales data real??
#       Use just repair data to forecast, since they also contain the sale date?
repair.train.per.module.agg2 <- aggregate(number_repair ~ module_category + component_category + 
                                          year_month_sale + year_month_repair,
                                          data=repair.train.per.module, sum)
# Reorder:
rows <- order(repair.train.per.module.agg2$module_category,
              repair.train.per.module.agg2$component_category, repair.train.per.module.agg2$year_month_sale,
              repair.train.per.module.agg2$year_month_repair)
repair.train.per.module.agg2 <- repair.train.per.module.agg2[rows, ]

# Test 1: Create a time series for testing/plotting:
test <- matrix(repair.train.per.module.agg$number_repair)
rownames(test) <- substring(repair.train.per.module.agg$year_month_repair, 1, 4)
colnames(test)[1] <- "year_month_repair"
ts1 <- ts(test, frequency=12)

# Test 1a:
# - Get the diff between sale date and repair date for all records for a particular mod/comp pair:
data <- repair.train.per.module[repair.train.per.module$component_category == "P02", ]
dim(data)
fit <- lm(data$number_repair ~ data$year.daydiff)
summary(fit)
# -Getting the diff between last sale date for mod/comp pair as newdata for prediction

# Test 1b: Survival Analysis?
# http://cran.r-project.org/web/packages/survival/survival.pdf
# http://stat.ethz.ch/R-manual/R-devel/library/survival/html/00Index.html
library(survival)
head(lung)
lfit <- aareg(Surv(time, status) ~ age + sex + ph.ecog, data=lung, nmin=1)
plot(lfit, ylim=c(-4,4))  # Draw a plot of the function for ph.ecog
# http://stat.ethz.ch/R-manual/R-devel/library/survival/html/Surv.html
with(lung, Surv(time, status))
Surv(heart$start, heart$stop, heart$event) 

# Test 1c: Hidden Markov Models?
# http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/latest/src/chapter10.html

# Test 2, get from repair.train:
# TODO: We can not have duplicate dates in time series. Aggregate repair volumes for a particular sale month
# for a particular module and a particular component?
# Any point using lag() for prediction of later repapir period for later sale period??

repair.sub <- repair.train[repair.train$module_category == "M1" & repair.train$component_category == "P16", ]
repair.train.per.module.agg <- aggregate(number_repair ~ year_month_repair, data=repair.sub, sum)
startYear <- as.integer(substring(min(as.character(repair.train.per.module.agg$year_month_repair)), 1, 4))
endYear <- as.integer(substring(max(as.character(repair.train.per.module.agg$year_month_repair)), 1, 4))
startMonth <- as.integer(substring(min(as.character(repair.train.per.module.agg$year_month_repair)), 6))
endMonth <- as.integer(substring(max(as.character(repair.train.per.module.agg$year_month_repair)), 6))
ts2 <- ts(repair.train.per.module.agg$number_repair, start=c(startYear, startMonth),
          end=c(endYear, endMonth), frequency=12)
plot.ts(ts2, col="blue", main="ts aggregate: M1/P16")

# vignette("zoo-faq", package = "zoo")
zoo1 <- zoo(ts2, frequency=12)
window(ts2, start=2006, end=2007)
fit <- stl(ts2, s.window=2)
plot(ts2, col="gray", main="ts2", lwd=2, ylab="Seasonally adjusted + trend", xlab="Year/month", cex.axis=.8)
lines(seasadj(fit), col="blue") # seasadj from forecast package
lines(fit$time.series[,2], col="red") # display trend

library(zoo)
ZOO <- zoo(repair.train.per.module.agg$number_repair, repair.train.per.module.agg$year_month_repair)
#ZOO2 <- zoo(repair.train.per.module.agg$number_repair, repair.train.per.module.agg$year_month_sale)
par(mfrow=c(1,2))
#plot(ZOO, type="l", lwd=2, col="blue", xlim=as.POSIXct(c("2006-01-01","2010-12-31")))
plot(ZOO, type="l", lwd=2, col="blue", xlim=as.Date(c("2006-01-01","2010-12-31")))
title(main="ZOO repair ts plot", col.main="steelblue4")
lines(meanf(ZOO, h=365)$mean, col="green3", lwd=2)
lines(naive(ZOO, h=365)$mean, col="red", lwd=2)
lines(snaive(ZOO, h=365)$mean, col="cornflowerblue", lwd=2)

library(xts)
XTS <- xts(repair.train.per.module.agg$number_repair, repair.train.per.module.agg$year_month_repair)
#plot(XTS, type="l", lwd=2, col="blue", xlim=as.POSIXct(c("2006-01-01","2010-12-31")))
plot.ts(XTS, type="l", lwd=2, col="red")
title(main="XTS repair ts plot", col.main="steelblue4")
lines(meanf(XTS, h=12)$mean, col="green3", lwd=2)
lines(naive(XTS, h=365)$mean, col="red", lwd=2)
lines(snaive(XTS, h=365)$mean, col="cornflowerblue", lwd=2)

coredata(XTS)
par(mfrow=c(1,1))

# Random walk
t <- ts(abs((1:100)*rnorm(100)*20), freq=12)
t
plot(rwf(log(t), drift=T, h=20))

# TODO: Look at Log Decay, as in Excel sheet (not mine) example
# https://www.khanacademy.org/science/chemistry/radioactive-decay/v/introduction-to-exponential-decay
k1 <- .05
k2 <- .08
start <- 12
t <- 1:100
the.curve1 <- start * exp(-k1 * t)
the.curve2 <- start * exp(-k2 * t)
plot(the.curve1, type="b", col="blue", pch=21, bg="cyan", main="Log Decay",
     xlab="Time", ylab="Decay", cex.axis=.8)
lines(the.curve2, col="red")

# Log decay example:
# http://www.mathsisfun.com/algebra/exponential-growth.html
# Example: Atmospheric pressure (the pressure of air around you) decreases as you go higher.
# It decreases about 12% for every 1000 m: an exponential decay.
# The pressure at sea level is about 1013 hPa (depending on weather).
# Write the formula (with its "k" value),
# What would the pressure be on the roof of the Empire State Building (381 m),
# and at the top of Mount Everest (8848 m)?

