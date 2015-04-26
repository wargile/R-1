# Walmart Recruiting - Predicting Sales in Stormy Weather
# Deadline: 25.05.2015
# http://www.kaggle.com/c/walmart-recruiting-sales-in-stormy-weather/data

# decryption key on test set file: Work4WalmarT

# NOTE: For the purposes of this competition, we have defined a weather event as any day in which more than an inch
# of rain or two inches of snow was observed

library(randomForest)
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html
library(stats) # For using fft() Fast Fourier Transform
library(kernlab)
library(e1071)
library(lubridate)
library(foreach)
library(doParallel)

set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
SetStandardOptions()

dataFolder <- "C:/coding/Kaggle/WalmartRecruitingSalesInStormyWeather/Data/"
submissionsFolder <- "C:/coding/Kaggle/WalmartRecruitingSalesInStormyWeather/Submissions/"

if (file.exists(paste0(dataFolder, "train.rda")) == F) {
  train <- read.csv(paste0(dataFolder, "train.csv"), header=T, sep=",", stringsAsFactors=F)
  test <- read.csv(paste0(dataFolder, "test.csv"), header=T, sep=",", stringsAsFactors=F)
  weather <- read.csv(paste0(dataFolder, "weather.csv"), header=T, sep=",", stringsAsFactors=F)
  key <- read.csv(paste0(dataFolder, "key.csv"), header=T, sep=",", stringsAsFactors=F)
  save(train, file=paste0(dataFolder, "train.rda"))
  save(test, file=paste0(dataFolder, "test.rda"))
  save(weather, file=paste0(dataFolder, "weather.rda"))
  save(key, file=paste0(dataFolder, "key.rda"))
} else {
  load(paste0(dataFolder, "train.rda"))
  load(paste0(dataFolder, "test.rda"))
  load(paste0(dataFolder, "weather.rda"))
  load(paste0(dataFolder, "key.rda"))
  load(paste0(dataFolder, "train.merged.rda"))
  load(paste0(dataFolder, "train.merged.db.rda"))
}

dim(train)
head(train)
plot(sort(train$units), type="l", main="Sold units") # Strange curve...
train[which(train$units > 1000),]
train[which(train$units > 400),]
dim(test)
head(test)
dim(key)
head(key)
dim(weather)
head(weather)
str(train)
summary(train)
#pairs(train)
sapply(train, class)
describe(train) # library(psych)
View(train[1:500,])

train.merged <- merge(x=train, y=key, by="store_nbr", all.x=T)
train.merged <- merge(x=train.merged, y=weather, by=c("station_nbr","date"), all.x=T) # NOTE: 'by' here!

dim(train)
dim(train.merged) # Looks good!
summary(train.merged)
sapply(train.merged, class)
save(train.merged, file=paste0(dataFolder, "train.merged.rda"))
train.merged.db <- as.data.table(train.merged)
save(train.merged.db, file=paste0(dataFolder, "train.merged.db.rda"))

# Plot some data:
barplot(table(train$store_nbr), main="Store:store_nbr", col="wheat", las=2)
barplot(table(weather$station_nbr), main="Weather:station_nbr", col="wheat", las=2)
barplot(table(train.merged$station_nbr), main="Store.merged:station_nbr", col="wheat", las=2)
barplot(table(weather$avgspeed), las=2, main="Average wind speed")
barplot(table(weather$preciptotal), las=2, main="Total precipitation")
hist(as.numeric(weather$preciptotal), main="Total precipitation")
with(train.merged, boxplot(units ~ item_nbr), main="Units by item_nbr")
with(train.merged, boxplot(preciptotal ~ station_nbr), main="Total precipitation")
     
unique(complete.cases(train.merged)) # true
# unique(is.na(train.merged)) # Takes a looong time, so avoid!

# [1] "station_nbr" "date"        "store_nbr"   "item_nbr"    "units"       "tmax"       
# [7] "tmin"        "tavg"        "depart"      "dewpoint"    "wetbulb"     "heat"       
# [13] "cool"        "sunrise"     "sunset"      "codesum"     "snowfall"    "preciptotal"
# [19] "stnpressure" "sealevel"    "resultspeed" "resultdir"   "avgspeed" 

events <- table(weather$date[nchar(weather$codesum) > 0])
# try to create plot from data page:
weather.plot <- weather

# NOTE: For the purposes of this competition, we have defined a weather event as any day in which more than an inch
# of rain or two inches of snow was observed

# Impute M (missing) and convert T (trace) values! Convert to numeric for rain and snowfall cols
weather.plot$preciptotal[weather.plot$preciptotal %in% c("M","  T")] <- "-1" # TODO: temp workaround-value
weather.plot$snowfall[weather.plot$snowfall %in% c("M","  T")] <- "-1" # TODO: temp workaround-value
weather.plot$preciptotal <- as.numeric(weather.plot$preciptotal)
weather.plot$snowfall <- as.numeric(weather.plot$snowfall)

for (counter in 1:20) { # Weather stations
  #weather.plot$preciptotal[which((weather.plot$station_nbr == counter) & (weather.plot$preciptotal == -1))] <-
  #  mean(weather.plot$total.precipitation[which((weather.plot$station_nbr == counter) & (weather.plot$total.precipitation != -1))])
  #weather.plot$snowfall[which((weather.plot$station_nbr == counter) & (weather.plot$total.snowfall == -1))] <-
  #  mean(weather.plot$total.snowfall[which((weather.plot$station_nbr == counter) & (weather.plot$total.snowfall != -1))])
  weather.plot$preciptotal[which((weather.plot$station_nbr == counter) & (weather.plot$preciptotal == -1))] <- 0
  weather.plot$snowfall[which((weather.plot$station_nbr == counter) & (weather.plot$total.snowfall == -1))] <- 0
}

table(weather.plot$codesum %in% c("TS","SS","DS"))
weather.plot$event <- ifelse(weather.plot$preciptotal > 1 | weather.plot$snowfall > 2, T, F)
library(scales) # to access breaks/formatting functions
ggplot(data=weather.plot, aes(x=as.Date(date), y=event, color=event)) + geom_point(shape=16) +
  facet_grid(station_nbr ~ .) + scale_x_date()
# http://stackoverflow.com/questions/6638696/breaks-for-scale-x-date-in-ggplot2-and-r
# http://docs.ggplot2.org/current/scale_date.html


table(is.na(train))
table(complete.cases(train)) # Only complete cases
unique(is.na(train))
table(is.na(test))

head(train, n=1)
sapply(train, class)

CorrelationPlot(train[,2:4])
cor(train[,2:4])

# ----------------------------------------------------------------------------------------------------------------------
submission <- data.frame(Id=0:(nrow(test)-1), Prediction=p) # TODO: different format (store id + date?)
head(submission)
# ----------------------------------------------------------------------------------------------------------------------
#save(submission.gbm, file=paste0(submissionsFolder, "submission.rda"))
KaggleSubmission(submission, submissionsFolder, "SVM")   

# ---------------------------------------------------------------------------------------------------------------------------
