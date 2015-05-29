# The Analytics Edge week 1 - Introduction to R
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/introduction-r/
# ------------------------------------------------------------------------------------

# The lectures:
# https://www.framinghamheartstudy.org/
# http://www.ncbi.nlm.nih.gov/gap
# http://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?id=phs000007

# R help:
# www.statmethods.met
# www.rseek.org
# finzi.psych.upenn.edu/search

# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

# -----------------------------------------------------------------------------------------------------------------
# From the lecture videos:

# The WHO dataset:
WHO <- read.csv("C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe <- subset(WHO, Region == "Europe")
head(WHO_Europe)
plot(WHO$Under15)
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO[which.min(WHO$Under15), ] # Japan has min population under 15
WHO[which.max(WHO$Under15), ] # Nigeria has max population under 15

plot(FertilityRate ~ GNI, data=WHO, col="blue", pch=21, bg="cyan",
     main="FertilityRate versus GNI (Gross National Income)")
abline(lm(FertilityRate ~ GNI, data=WHO), col="red")
model <- lm(FertilityRate ~ I(GNI^2), data=WHO)
abline(model$coeff[1], model$coeff[2], col="green4")

plot(FertilityRate ~ log(GNI), data=WHO, col="blue", pch=21, bg="cyan",
     main="FertilityRate versus GNI (Gross National Income)")
abline(lm(FertilityRate ~ log(GNI), data=WHO), col="red") # Better fit with log() on x

Outliers <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]
mean(WHO$Over60)
WHO[which.min(WHO$Over60),]
WHO[which.max(WHO$Literacy),]

hist(WHO$CellularSubscribers, col="wheat")

boxplot(WHO$LifeExpectancy ~ WHO$Region, col="darkcyan", xlab="", ylab="Life expectancy",
        main="Life expectancy of countries by region", las=2, pch=21, bg="gray")
# Any point greater than the 3rd quartile PLUS the IQR is considered an outlier
# Any point smaller than the 1st quartile MINUS the IQR is considered an outlier

table(WHO$Region)

par(mar=c(7,3,2,.8))
barplot(tapply(WHO$Over60, WHO$Region, mean), col="cornflowerblue", main="Population over 60 by region", las=2)
barplot(tapply(WHO$LiteracyRate, WHO$Region, mean, na.rm=T), col=2:7, main="Literacy rate by region", las=2)
barplot(tapply(WHO$ChildMortality, WHO$Region, mean), col="cornflowerblue", main="Child mortality by region", las=2)
par(mar=oldpar$mar)

# USDA (United States Department of Agriculture) dataset (http://ndb.nal.usda.gov/):
USDA <- read.csv("C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/USDA.csv")
dim(USDA)
str(USDA)
summary(USDA) # Look at max(sodium) and max(potassioum)... In general, a lot of max() values are startling...
names(USDA)
USDA$Description[which.max(USDA$Sodium)] # SALT. Makes sense...
USDA$Description[which.max(USDA$Potassium)]
HighSodium <- subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm=T)
mean(USDA$Sodium, na.rm=T) + sd(USDA$Sodium, na.rm=T)
# Since we can sum the mean + the sd and get approx. 1370 grams, we see that sodium in caviar (1500) is still above
hist(USDA$Sodium, main="Sodium content in milligrams", breaks=150, col="wheat", xlim=c(0, 3000))
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main="Protein versus Fat", col="blue",
     pch=21, bg="cyan")
hist(USDA$VitaminC, main="Vitamin C content in milligrams", col="wheat", xlim=c(0, 100), breaks=2000)
boxplot(USDA$Sugar, main="Sugar levels", ylab="Sugar in grams", col="goldenrod1")
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=T))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=T))
USDA$HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=T))
str(USDA)
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=T)
tapply(USDA$VitaminC, USDA$HighCarbs, summary)


# ------------------------------------------------------------------------------------------------------------------
# Homework
# ------------------------------------------------------------------------------------------------------------------
# 1) AN ANALYTICAL DETECTIVE

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/"
df <- read.csv(paste0(folder, "mvtWeek1.csv"), header=T)
head(df)
dim(df)
sapply(df, class)

str(df)
summary(df)
#pairs(df, col="blue")

max(df$ID)
min(df$Beat)
table(df$Arrest)
table(df$LocationDescription)[10]

locations <- as.data.frame(table(df$LocationDescription))
names(locations) <- c("Location", "Freq")
par(mar=c(5,11,2,1))
barplot(sort(locations$Freq[locations$Freq > 300]), axisnames=T, main="Crime location frequency",
        names.arg=locations$Location[locations$Freq > 300], horiz=T, col="wheat", las=2, cex.names=.6)
par(mar=c(3,3,2,1))

hist(df$District, col="wheat", main="District")
barplot(table(df$Arrest), col=c("red","green"), main="Was an arrest made?")

DateConvert = as.Date(strptime(df$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

df$Year = year(DateConvert)
df$Month = months(DateConvert)
df$Weekday = weekdays(DateConvert)
df$Date2 <- DateConvert

par(mar=c(5,5,3,1))
boxplot(Date2 ~ Arrest, data=df, main="Dates by arrest yes/no", col=c("red","green3"), ylab="Date", xlab="Arrest or not")


# PROBLEM 3.2 - VISUALIZING CRIME TRENDS. Answer: First half
df2 <- df[df$Arrest == 1, ]
df3 <- aggregate(Arrest ~ Year, FUN=sum, data=df2)
barplot(df3$Arrest, df3$Year, las=2, cex.names=.6, names.arg=df3$Year, col="wheat", main="Arrests by year")

# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/discussion/forum/ee58629402db47adb6dff61bad13e372/threads/54f5e2b52a472d21f00014e4

# http://www.cyclismo.org/tutorial/R/tables.html

# PROBLEM 3.3 - VISUALIZING CRIME TRENDS. Answer: 0.1162 (WRONG!) 2152 / (18517 + 2152) = 0.1041
# TODO: Can also use prop.table(...) How?
result <- table(df$Arrest, df$Year)[1:2,1]
result[2] / (result[1] + result[2)] # Proportion of arrests being made in 2001

# PROBLEM 3.4 - VISUALIZING CRIME TRENDS. Answer: 0.09275 (ERROR!) 0.504 (ERROR!) 1212 / (1212 + 13068) = 0.08487
# TODO: Can also use prop.table(...) How?
result <- table(df$Arrest, df$Year)[1:2,7]
result[2] / (result[1] + result[2]) # Proportion of arrests being made in 2007

# PROBLEM 3.5 - VISUALIZING CRIME TRENDS. Answer: 0.04061 (ERROR!) 550 / (550 + 13542) = 0.03903
# TODO: Can also use prop.table(...) How?
result <- table(df$Arrest, df$Year)[1:2,12]
result[2] / (result[1] + result[2]) # Proportion of arrests being made in 2012

# PROBLEM 4.1 - POPULAR LOCATIONS. Answers:
# 1) STREET 2) PARKING LOT/GARAGE(NON.RESID.) 3) ALLEY 4) GAS STATION 5) DRIVEWAY - RESIDENTIAL   
df2 <- df$LocationDescription[df$LocationDescription != "OTHER"]
result <- sort(table(df2), decreasing=T)[1:5]
df2 <- data.frame(location=names(result), thefts=as.integer(result))
par(mar=c(5,11,2,1))
barplot(sort(result), las=2, horiz=T, col="powderblue", main="Top 5 locations for theft")
par(mar=c(5,5,3,1))

# PROBLEM 4.2 - POPULAR LOCATIONS. Answer: 177510
Top5 <- df[(df$LocationDescription %in% names(result)), ]
Top5$LocationDescription <- factor(Top5$LocationDescription)
unique(Top5$LocationDescription)
table(Top5$Arrest)
nrow(Top5)

# PROBLEM 4.3 - POPULAR LOCATIONS. Answer: Street (Wrong!) Gas Station
table(Top5$LocationDescription) # NOTE: Also shows all the other LocationDescription values! 
str(Top5) # Just shows the top 5
dfArrestRate <- (table(Top5$LocationDescription, Top5$Arrest))
arrests <- data.frame(Location=rownames(dfArrestRate), non.arrest=dfArrestRate[,1], arrest=dfArrestRate[,2])
arrests$arrest.rate <- arrests$arrest / (arrests$arrest + arrests$non.arrest)
head(arrests)
arrests[which.max(arrests$arrest.rate),]

# PROBLEM 4.4 - POPULAR LOCATIONS. Answer: Friday (Wrong!) Saturday
df2 <- df[df$LocationDescription == "GAS STATION", ]
table(df2$Weekday)

df3 <- aggregate(Arrest ~ Weekday, FUN=sum, data=df2)
df3

# PROBLEM 4.5 - POPULAR LOCATIONS. Answer: Thursday (Wrong!) Saturday
# Can also use subset(df, LocationDescription == "DRIVEWAY - RESIDENTIAL") here
df2 <- df[df$LocationDescription == "DRIVEWAY - RESIDENTIAL", ]
table(df2$Weekday)
df3 <- aggregate(Arrest ~ Weekday, FUN=sum, data=df2)
df3


# In a boxplot, the bold horizontal line is the median value of the data, the box shows the range of values between the
# first quartile and third quartile, and the whiskers (the dotted lines extending outside the box) show the minimum and
# maximum values, excluding any outliers (which are plotted as circles)
# Outliers are defined by first computing the difference between the first and third quartile values, or the height of the box.
# This number is called the Inter-Quartile Range (IQR). Any point that is greater than the third quartile plus the IQR or less
# than the first quartile minus the IQR is considered an outlier.

dfMotorVehicle <- df[df$LocationDescription %in% c("VEHICLE-COMMERCIAL","VEHICLE NON-COMMERCIAL"), ]

barplot(sort(table(df$Month)), las=2, main="Grand theft auto by month", col="wheat")
barplot(sort(table(df$Weekday)), las=2, main="Grand theft auto by weekday", col="cornflowerblue")
barplot(sort(table(df$Month[df$Arrest == T])), las=2, main="Grand theft auto arrests by month", col="wheat")

hist(df$Year, breaks=100, col="orange")

# Plot in lat/long on Google Chicago map
library(ggmap)
library(ggplot2)

# TODO: Show just a certain type of location for the crime (manipulate?)
map.chicago <- qmap("chicago", source="stamen", zoom=11, maptype="toner", darken=c(.3, "#BBBBBB")) 
map.chicago +
  geom_point(data=dfMotorVehicle, aes(x=Longitude, y=Latitude, size=2,
                                      colour=LocationDescription), alpha=.9, na.rm=T) +
  #scale_color_discrete(color=c("red","blue")) +
  scale_size(range=c(1, 5), guide="none") + 
  ggtitle("Grand Theft Auto") +
  theme(text = element_text(family="Verdana", color="#666666")) +
  theme(plot.title = element_text( size=12, face="bold", hjust=0, vjust=.5))


# TODO: ts not working...
d8 <- df[which(df$District == 8), ]
ts.d8 <- ts(start=min(as.Date(strptime(d8$Date, "%m/%d/%y %H:%M"))),
                      end=max(as.Date(strptime(d8$Date, "%m/%d/%y %H:%M"))), frequency=1, data=as.integer(df$Arrest))
plot(ts)

# ------------------------------------------------------------------------------------------------------------------------
# 2) STOCK DYNAMICS

# Tip on problem 2.2:
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/discussion/forum/b822f40bb6c2476fa7eb3b7d6df9ae55/threads/54f6847a2a472d1103001463

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/"

IBM <- read.csv(paste0(folder, "IBMStock.csv"), header=T)
GE <- read.csv(paste0(folder, "GEStock.csv"), header=T)
ProcterGamble <- read.csv(paste0(folder, "ProcterGambleStock.csv"), header=T)
CocaCola <- read.csv(paste0(folder, "CocaColaStock.csv"), header=T)
Boeing <- read.csv(paste0(folder, "BoeingStock.csv"), header=T)

dim(IBM)
summary(IBM)
dim(GE)
summary(GE)
dim(ProcterGamble)
summary(ProcterGamble)
dim(CocaCola)
summary(CocaCola)
dim(Boeing)
summary(Boeing)

# Date: the date of the stock price, always given as the first of the month.
# StockPrice: the average stock price of the company in the given month.

# Change from factor to Date class:
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

heading <- paste0("CocaCola stock price (max: ", CocaCola$Date[which(CocaCola$StockPrice == max(CocaCola$StockPrice))], ")")
plot(StockPrice ~ Date, data=CocaCola, type="l", col="Blue", main=heading)
abline(v=CocaCola$Date[which(CocaCola$StockPrice == max(CocaCola$StockPrice))], col="green4") # NOTE abline Date offset!
abline(v=CocaCola$Date[which(CocaCola$StockPrice == min(CocaCola$StockPrice))], col="red") # NOTE abline Date offset!
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="red", lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=1, col="gray", lty=2)
abline(v=as.Date(c("1983-01-01")), lwd=1, col="cyan", lty=2)

# TODO: Add legends!
par(mar=c(3,3,2,1))
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210), main="Stocks",
     xlab="Date", ylab="Price")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="green4", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="cyan", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="pink", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=1, col="gray", lty=2) # NOTE abline Date offset!
abline(v=as.Date(c("1997-09-01")), lwd=1, col="gray", lty=2)
abline(v=as.Date(c("1997-11-01")), lwd=1, col="gray", lty=2)
abline(v=as.Date(c("2004-01-01")), lwd=1, col="gray", lty=2)
abline(v=as.Date(c("2005-12-01")), lwd=1, col="gray", lty=2)

# Over the mean: april, februar, januar, may, march
par(mar=c(4,3,2,1))
mean.stock.price.IBM <- tapply(X=IBM$StockPrice, IND=months(IBM$Date), FUN=mean) # NOTE! Useful!
plot(mean.stock.price.IBM, type="o", xaxt="n", xlab=NA, main="Stock price IBM")
axis(1, at=1:12, labels=names(mean.stock.price.IBM), las=2)
names(mean.stock.price.IBM)
abline(h=mean(IBM$StockPrice), col="green1")
grid()
par(mar=c(3,3,2,1))

mean.stock.price.IBM <- tapply(X=IBM$StockPrice, IND=months(IBM$Date), FUN=mean) # NOTE! Useful!
mean.stock.price.GE <- tapply(X=GE$StockPrice, IND=months(GE$Date), FUN=mean) # NOTE! Useful!
mean.stock.price.CocaCola <- tapply(X=CocaCola$StockPrice, IND=months(CocaCola$Date), FUN=mean) # NOTE! Useful!
mean.stock.price.ProcterGamble <- tapply(X=ProcterGamble$StockPrice, IND=months(ProcterGamble$Date), FUN=mean) # NOTE! Useful!
mean.stock.price.Boeing <- tapply(X=Boeing$StockPrice, IND=months(Boeing$Date), FUN=mean) # NOTE! Useful!
plot(mean.stock.price.IBM, type="o", xaxt="n", xlab=NA, ylim=c(41, 160), main="Stocks by month")
axis(1, at=1:12, labels=names(mean.stock.price.IBM), las=2)
lines(mean.stock.price.GE, col="red")
lines(mean.stock.price.CocaCola, col="green4")
lines(mean.stock.price.ProcterGamble, col="blue")
lines(mean.stock.price.Boeing, col="cyan")
grid()

plot(mean.stock.price.CocaCola, type="o", xaxt="n", xlab=NA, ylim=c(50, 75), main="Stocks by month, CocaCola and GE")
axis(1, at=1:12, labels=names(mean.stock.price.CocaCola), las=2)
lines(mean.stock.price.GE, col="red") # TODO: Ablines on max for both
grid()


# ------------------------------------------------------------------------------------------------------------------------
# 3) DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/e0d9ca1c350d42e5a8d6fd6a8162c1ab/a5915d0492804dada5feb1926ba5be7a/
# Get the 2013 census (full set of the one loaded) here: http://thedataweb.rm.census.gov/ftp/cps_ftp.html

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/"

CountryMap <- read.csv(paste0(folder, "CountryCodes.csv"), header=T)
CPS <- read.csv(paste0(folder, "CPSData.csv"), header=T)
MetroAreaMap <- read.csv(paste0(folder, "MetroAreaCodes.csv"), header=T)
dim(CountryMap)
dim(CPS)
summary(CPS)
str(CPS)
dim(MetroAreaMap)

# PROBLEM 1.1 - LOADING AND SUMMARIZING THE DATASET. Answer: 131302

# PROBLEM 1.2 - LOADING AND SUMMARIZING THE DATASET. Answer: Educational and health services
sort(table(CPS$Industry), decreasing=T)[1:5]

# PROBLEM 1.3 - LOADING AND SUMMARIZING THE DATASET. Answer: Fewest: New Mexico. Most: California
sort(table(CPS$State), decreasing=F)[1:5]
sort(table(CPS$State), decreasing=T)[1:5]

# PROBLEM 1.4 - LOADING AND SUMMARIZING THE DATASET. Answer: 0.8883 (Wrong!) 0.9422
citizen <- subset(CPS, Citizenship == "Citizen, Native")
citizen.naturalized <- subset(CPS, Citizenship == "Citizen, Naturalized")

(nrow(citizen) + nrow(citizen.naturalized)) / nrow(CPS)

# PROBLEM 1.5 - LOADING AND SUMMARIZING THE DATASET. Answer: American Indian, Black, Multiracial, White
unique(CPS$Race)
unique(CPS$Hispanic)
table(CPS$Race[CPS$Hispanic == 1])
table(CPS$Race[CPS$Hispanic == 0])

# PROBLEM 2.1 - EVALUATING MISSING VALUES. Answer: Industry, EmploymentStatus, Education, Married, MetroAreaCode,  
lapply(CPS, function(x) unique(is.na(x)))

# PROBLEM 2.2 - EVALUATING MISSING VALUES. Answer: Age is related to Marriage variable missing or not
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
boxplot(CPS$Age[!is.na(CPS$Married)] ~ CPS$Married[!is.na(CPS$Married)],
        col="violetred4", main="Marriage status", ylab="Age")
table(CPS$Citizenship, is.na(CPS$Married))

# PROBLEM 2.3 - EVALUATING MISSING VALUES.
# Answer 1: Alaska, Wyoming (2 states)
# Answer 2: Rhode Island, New Jersey, District of Columbia (3 states)
result <- table(CPS$State, is.na(CPS$MetroAreaCode))
result

# PROBLEM 2.4 - EVALUATING MISSING VALUES. Answer: Midwest
result <- table(CPS$Region, is.na(CPS$MetroAreaCode))
result

# PROBLEM 2.5 - EVALUATING MISSING VALUES. Answer: 1) Wisconsin 2) Montana
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean) # Easiest way

CPS$NotLivingInMetroArea <- is.na(CPS$MetroAreaCode)
table(CPS$NotLivingInMetroArea)
result <- tapply(CPS$NotLivingInMetroArea, CPS$State, mean) * 100
subset(result, result > 28 & result < 32)

CPS$NotLivingInMetroArea <- is.na(CPS$MetroAreaCode)
table(CPS$NotLivingInMetroArea)
result <- tapply(CPS$NotLivingInMetroArea, CPS$State, mean) * 100
sort(subset(result, result < 100))

# PROBLEM 3.1 - INTEGRATING METROPOLITAN AREA DATA. Answer: 1) 271 2) 149
dim(MetroAreaMap)
dim(CountryMap)

# PROBLEM 3.2 - INTEGRATING METROPOLITAN AREA DATA. Answer: 1) MetroArea 2) 34238
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
head(CPS, n=1)
summary(CPS)
str(CPS)
table(is.na(CPS$MetroArea))

# PROBLEM 3.3 - INTEGRATING METROPOLITAN AREA DATA. Answer: Boston-Cambridge-Quincy, MA-NH
result <- table(CPS$MetroArea)
result[rownames(result) %in% c("Atlanta-Sandy Springs-Marietta, GA","Baltimore-Towson, MD",
                                "Boston-Cambridge-Quincy, MA-NH","San Francisco-Oakland-Fremont, CA")]

# PROBLEM 3.4 - INTEGRATING METROPOLITAN AREA DATA. Answer: Laredo, TX
par(mar=c(10,4,2,1))
result <- sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing=T)
barplot(result[1:15], las=2, col="wheat", main="Hispanic population by MetroArea")
which.max(tapply(CPS$Hispanic, CPS$MetroArea, mean)) # Easiest way
par(mar=c(3,3,2,1))
result[1]

# PROBLEM 3.5 - INTEGRATING METROPOLITAN AREA DATA. Answer: 4
result <- sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean), decreasing=T) * 100
result[result >= 20]

# PROBLEM 3.6 - INTEGRATING METROPOLITAN AREA DATA. Answer: Iowa City, IA
result <- sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T)) * 100
result[1:10]

# PROBLEM 4.1 - INTEGRATING COUNTRY OF BIRTH DATA. Answer: 1) Country 2) 176
CPS2 <- CPS # Keep a copy...
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
head(CPS)
str(CPS)
table(is.na(CPS$Country))

# PROBLEM 4.2 - INTEGRATING COUNTRY OF BIRTH DATA. Answer: Philippines
# TIP: http://en.wikipedia.org/wiki/North_America#Countries.2C_territories.2C_and_dependencies
North.America <- c("Canada","Mexico","United States","Bermuda","") 
CPS.outside.US <- subset(CPS, !(Country %in% North.America))
dim(CPS.outside.US)
sort(table(CPS.outside.US$Country), decreasing=T)[1:6]

# PROBLEM 4.3 - INTEGRATING COUNTRY OF BIRTH DATA. Answer: 0.6891 (Wrong!) 0.1157 (Wrong!) 0.3087
CPS2 <- CPS[!is.na(CPS$Country),]
unique(is.na(CPS2$Country)) # only false
table(CPS2$Country, CPS2$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
in.metro.area <- table(CPS2$Country, CPS2$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")[,2]
# Same as below (except that tapply result below get NA's. Why, since we have na.rm=T ??):
result <- tapply(CPS2$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS2$Country, sum, na.rm=T)
result["United States"]
result["United States"] / sum(result, na.rm=T)
(sum(in.metro.area) - in.metro.area["United States"]) / sum(in.metro.area)

# PROBLEM 4.4 - INTEGRATING COUNTRY OF BIRTH DATA. Answer:
# 1) New York-Northern New Jersey-Long Island, NY-NJ-PA
# 2) Boston-Cambridge-Quincy, MA-NH
# 3) Minneapolis-St Paul-Bloomington, MN-WI
which.max(tapply((CPS$Country == "India"), CPS$MetroArea, sum, na.rm=T))
which.max(tapply((CPS$Country == "Brazil"), CPS$MetroArea, sum, na.rm=T))
which.max(tapply((CPS$Country == "Somalia"), CPS$MetroArea, sum, na.rm=T))


# ------------------------------------------------------------------------------------------------------------------------
# 4) INTERNET PRIVACY POLL (OPTIONAL)
# Full dataset here: http://pewinternet.org/Shared-Content/Data-Sets/2013/July-2013--Anonymity-(Omnibus).aspx

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 1/Assignment/"

# PROBLEM 1.2 - LOADING AND SUMMARIZING THE DATASET. Answer: 1) 487 2) 472 3) 43
poll <- read.csv(paste0(folder, "AnonymityPoll.csv"), header=T)
dim(poll)
summary(poll)
str(poll)
names(poll)
nrow(poll[poll$Smartphone == 1 & !is.na(poll$Smartphone), ])
# NOTE: gives wrong answer without NA check, counts NA rows too!
nrow(poll[poll$Smartphone == 0 & !is.na(poll$Smartphone), ])
# NOTE: gives wrong answer without is.na here!
nrow(poll[is.na(poll$Smartphone), ])
table(poll$Smartphone)

# PROBLEM 1.3 - LOADING AND SUMMARIZING THE DATASET. Answer: 1) Kansas, Missouri, Ohio 2) Texas
table(poll$Sex, poll$Region)
table(poll$Region, poll$State)
plot(poll$State ~ poll$Region)
tapply(poll$State, poll$Region)
south <- subset(poll, Region == "South")
unique(south$State)
table(poll$Region[poll$Region == "South"], poll$State[poll$Region == "South"])

# PROBLEM 2.1 - INTERNET AND SMARTPHONE USERS. Answer: 1) 186 2) 470 3) 285 4) 17
# NOTE: Must use !is.na() on these since there are NA values!
nrow(poll[poll$Internet.Use == 0 & poll$Smartphone == 0 & !is.na(poll$Internet.Use) & !is.na(poll$Smartphone),])
nrow(poll[poll$Internet.Use == 1 & poll$Smartphone == 1 & !is.na(poll$Internet.Use) & !is.na(poll$Smartphone),])
nrow(poll[poll$Internet.Use == 1 & poll$Smartphone == 0 & !is.na(poll$Internet.Use) & !is.na(poll$Smartphone),])
nrow(poll[poll$Internet.Use == 0 & poll$Smartphone == 1 & !is.na(poll$Internet.Use) & !is.na(poll$Smartphone),])

# PROBLEM 2.2 - INTERNET AND SMARTPHONE USERS. Answer 1) 1  2) 43
table(is.na(poll$Internet.Use))
table(is.na(poll$Smartphone))

# PROBLEM 2.3 - INTERNET AND SMARTPHONE USERS. Answer: 792
limited <- subset(poll, (Internet.Use == 1 | Smartphone == 1))
nrow(limited)

# PROBLEM 3.1 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: Smartphone, Age, Conservativeness, 
# Worry.About.Info, Privacy.Importance, anonymity.Possible, Tried.Masking.Identity, Privacy.Laws.Effective 
lapply(limited, function(x) unique(is.na(x)))

# PROBLEM 3.2 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 3.795
# NOTE: Should use limited df on all remaining questions (Although poll gives same result where used)!
mean(poll$Info.On.Internet, na.rm=T)

# PROBLEM 3.3 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 1) 105 2) 8
nrow(poll[poll$Info.On.Internet == 0 & !is.na(poll$Info.On.Internet), ])
nrow(poll[poll$Info.On.Internet == 11 & !is.na(poll$Info.On.Internet), ])

# PROBLEM 3.4 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 0.4486
nrow(poll[poll$Worry.About.Info == 1 & !is.na(poll$Worry.About.Info), ]) / nrow(poll[!is.na(poll$Worry.About.Info), ])

# PROBLEM 3.5 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 0.3692
nrow(poll[poll$Anonymity.Possible == 1 & !is.na(poll$Anonymity.Possible), ]) / nrow(poll[!is.na(poll$Anonymity.Possible), ])

# PROBLEM 3.6 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 0.1633
# Wrong: nrow(poll[poll$Tried.Masking.Identity == 1, ]) / nrow(poll[!is.na(poll$Tried.Masking.Identity),])
# NOTE: Test also with !is.na on first!
nrow(poll[poll$Tried.Masking.Identity == 1 & !is.na(poll$Tried.Masking.Identity), ]) /
  nrow(poll[!is.na(poll$Tried.Masking.Identity), ])

# PROBLEM 3.7 - SUMMARIZING OPINIONS ABOUT INTERNET PRIVACY. Answer: 0.2558
nrow(poll[poll$Privacy.Laws.Effective == 1 & !is.na(poll$Privacy.Laws.Effective), ]) /
  nrow(poll[!is.na(poll$Privacy.Laws.Effective), ])
# NOTE: They mean to use limited df here!
nrow(limited[limited$Privacy.Laws.Effective == 1 & !is.na(limited$Privacy.Laws.Effective), ]) /
  nrow(limited[!is.na(limited$Privacy.Laws.Effective), ])

# PROBLEM 4.1 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: Popele around 60 year old are best represented in the study.
hist(limited$Age[!is.na(limited$Age)], col="wheat")

# PROBLEM 4.2 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: 6 interviewees (look for the largest number in the table!)
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

result <- table(limited$Info.On.Internet, limited$Age)
par(mar=c(.5,2,.5,.5))
image(as.matrix(result[,1:ncol(result)]), las=2)
#image(as.matrix(result[,1:ncol(result)]), las=2, yaxt="n", xaxt="n")
#axis(side=2, at=seq(1,ncol(result)), labels=seq(1,ncol(result)))
#text(expand.grid(x=nrow(result), y=ncol(result)), labels=t(result), cex=.5)
table(limited$Age == 18, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet)) # As easy as this!
par(mar=c(3,3,2,1))

# PROBLEM 4.3 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: jitter adds or subtracts a small amount of
# random noise to the values passed to it, and two runs will yield different results.

# PROBLEM 4.4 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: Older age seems moderately associated
# with a smaller value for Info.On.Internet
plot(jitter(limited$Age), jitter(limited$Info.On.Internet), col="blue", main="Info.On.Internet ~ Age")
model <- lm(Info.On.Internet ~ Age, data=limited)
summary(model)
abline(model, col="red")

# PROBLEM 4.5 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: 1) 4.368 for Smartphone users 2) 2.923 for non-S.U.
tapply(poll$Info.On.Internet, poll$Smartphone, mean, na.rm=T)
tapply(limited$Info.On.Internet, limited$Smartphone, mean, na.rm=T)

# PROBLEM 4.6 - RELATING DEMOGRAPHICS TO POLLING RESULTS. Answer: 1) 0.1925  2) 0.1174
tapply(poll$Tried.Masking.Identity, poll$Smartphone, mean, na.rm=T)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm=T)

