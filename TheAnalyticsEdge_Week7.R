# The Analytics Edge week 7 - Visualization
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/visualization/
# ------------------------------------------------------------------------------------

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(ggmap)

# Set locale to US, to ensure that there aren't formatting issues in assignments/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 7/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) VISUALIZING THE WORLD: AN INTRODUCTION TO VISUALIZATION
# ----------------------------------------------------------

# http://ggplot2.org/
# http://docs.ggplot2.org/current/

# Lecture 1
# Video 4:
# Data from: http://apps.who.int/gho/data/node.main
WHO <- read.csv(paste0(folder, "WHO.csv"))
str(WHO)
# Plot from Week 1
plot(WHO$GNI, WHO$FertilityRate, pch=19, col="blue", main="FertilityRate versus GNI (Gross National Income)")
# Let's redo this using ggplot 
# Create the ggplot object with the data and the aesthetic mapping:
scatterplot <- ggplot(WHO, aes(x=GNI, y=FertilityRate))
# Add the geom_point geometry
scatterplot + geom_point(color="blue")
# Make a line graph instead:
scatterplot + geom_line(color="blue")
# Redo the plot with blue triangles instead of circles:
scatterplot + geom_point(color="blue", size=3, shape=17) 
# Another option:
scatterplot + geom_point(color="darkred", size=3, shape=8) 
# Add a title to the plot:
scatterplot + geom_point(colour="blue", size=3, shape=17) + ggtitle("Fertility Rate vs. Gross National Income")
# Save our plot:
fertilityGNIplot <- scatterplot + geom_point(colour="blue", size=3, shape=17) +
  ggtitle("Fertility Rate vs. Gross National Income")
pdf(paste0(folder, "MyPlot.pdf"))
print(fertilityGNIplot)
dev.off()

# Video 5:
# Color the points by region: 
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point(size=3)
# Color the points according to life expectancy:
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point(size=3)
# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point() # Curve looks logarithmic, so...
# Let's try a log transformation:
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point()
# Simple linear regression model to predict the percentage of the population under 15, using the log of the fertility rate:
mod1 <- lm(Under15 ~ FertilityRate, data=WHO)
summary(mod1)
mod2 <- lm(Under15 ~ log(FertilityRate), data=WHO)
summary(mod2) # Better!
# Add this regression line to our plot:
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm")
# 99% confidence interval
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", level=0.99)
# No confidence interval in the plot
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", se=FALSE)
# Change the color of the regression line:
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method="lm", colour="orange")


# 1) THE ANALYTICAL POLICEMAN: VISUALIZATION FOR LAW AND ORDER
# ------------------------------------------------------------

# Data from: http://gis.chicagopolice.org/

# Lecture 2, Predictive Policing
# VIDEO 3 - A Basic Line Plot
# Load our data:
mvt = read.csv(paste0(folder, "mvt.csv"), stringsAsFactors=FALSE)
str(mvt)
# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
# Let's take a look at the structure of our data again:
str(mvt)
# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)
# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts) 
# Load the ggplot2 library:
library(ggplot2)
# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), color="blue")  
# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))
# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), color="blue")
# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), color="blue") + xlab("Day of the Week") +
  ylab("Total Motor Vehicle Thefts")

# VIDEO 4 - Adding the Hour of the Day
# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)
# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=1.5)
# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)
# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")
# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2) 
# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5) 
# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE,
                            levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))
# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())

# VIDEO 5 - Maps
# Install and load two new packages:
#install.packages("maps")
#install.packages("ggmap")
library(maps)
library(ggmap)
# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)
# Look at the map
ggmap(chicago)
# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude), color="red", size=2)
# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)
# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))
# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))
# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) +
  scale_colour_gradient(low="yellow", high="red")
# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")
# Remove lat/lon points out on the water (Freq == 0)
LatLonCounts2 <- subset(LatLonCounts, Freq > 0)
ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")

# VIDEO 6 - Geographical Map on US
# Load our data:
murders = read.csv(paste0(folder, "murders.csv"))
str(murders)
# Load the map of the US
statesMap = map_data("state")
str(statesMap)
ggplot(data=murders, aes(y=GunOwnership, x=State, fill=Population)) + geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Gun ownership by state and population size")
# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 
# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)
# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)
# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "black", high = "red", guide = "legend") + ggtitle("US murders by state")
# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "black", high = "red", guide = "legend") + ggtitle("US population by state")
# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = (murderMap$Murders / murderMap$Population) * 100000
# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "black", high = "red", guide = "legend") + ggtitle("US murder rate by state")
# It's just Washington DC that has a murder rate above 10, and it's so small it's not really visible here. So:
# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10)) + ggtitle("US murder rate by state")

# Unit 7 - Recitation
# VIDEO 3 - Bar Charts
# Load ggplot library
library(ggplot2)
# Load our data, which lives in intl.csv
# Data from: http://web.mit.edu/iso/
intl = read.csv(paste0(folder, "intl.csv"))
str(intl)
# We want to make a bar plot with region on the X axis and Percentage on the y-axis.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity", fill="orange") + # "identity" = use the y-variable as-is
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Percent of international students at MIT by region") +
  geom_text(aes(label=PercentOfIntl))
# Make Region an ordered factor in descending order
# We can do this with the re-order command and transform command.
# NOTE: Behaves differently for factors, so use as.character(Region)! 
intl = transform(intl, Region = reorder(as.character(Region), -PercentOfIntl)) # - minus: decreasing order
# Look at the structure
str(intl)
# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100
# Make the plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity", fill="dark blue") +
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
  ggtitle("Percent of international students at MIT by region") +
  ylab("Percent of International Students") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
# VIDEO 5 - World map
# Load the ggmap package
library(ggmap)
# Load in the international student data
# Data from: http://web.mit.edu/iso/
intlall = read.csv(paste0(folder, "intlall.csv"), stringsAsFactors=FALSE)
# Lets look at the first few rows
head(intlall)
# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0
# Now lets look again
head(intlall) 
# Load the world map
world_map = map_data("world")
str(world_map)
# Lets merge intlall into world_map using the merge command
world_map = merge(world_map, intlall, by.x ="region", by.y = "Citizenship")
str(world_map)
# Plot the map
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") + coord_map("mercator")
# Reorder the data
world_map = world_map[order(world_map$group, world_map$order),]
# Redo the plot
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") + coord_map("mercator")
# Lets look for China
table(intlall$Citizenship) 
# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"
# We'll repeat our merge and order from before
world_map = merge(map_data("world"), intlall, by.x ="region", by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")
# We can try other projections - this one is visually interesting
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(-37, 175, 0))
# VIDEO 7 - Line Charts
# First, lets make sure we have ggplot2 loaded
library(ggplot2)
# Now lets load our dataframe
# Data from: http://www.census.gov/
households = read.csv(paste0(folder, "households.csv"))
str(households)
# Load reshape2
library(reshape2)
# Lets look at the first two columns of our households dataframe
households[,1:2]
# First few rows of our melted households dataframe
head(melt(households, id="Year"))
households[,1:3]
melt(households, id="Year")[1:10,3]
melt(households, id="Year")[1:10,]
# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) +  
  ylab("Percentage of Households") + ggtitle("US Households")

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (VISUALIZING THE WORLD)
# -----------------------------------------

# 1) Normally, a scatterplot only allows us to visualize two dimensions - one on the x-axis, and one on the y-axis.
# In the previous video we were able to show a third dimension on the scatterplot using what attribute?
# Answer: Color

# 2) Why is it particularly helpful for WHO to provide data visualizations?
# Answer 1): When communicating information to the general public, a visualization like the Energy Consumption one
# is much easier to absorb than a table of numbers would be.
# Answer 2): Visualizations can easily be used by policymakers and others who wish to present data from WHO. 

# 3) In the Scatterplot, what are the geometric objects? Answer: Points
#    In the Histogram, what are the geometric objects? Answer: Bars
#    In the US Map, what are the geometric objects? Answer: Polygons
#    All three of these plots defined a particular aesthetic property. What is it? Answer: Color

# 4) Answer: Shape 15 is: A square
scatterplot + geom_point(colour="blue", size=3, shape=15) + ggtitle("Fertility Rate vs. Gross National Income")

# 5) One region in particular has a lot of countries with a very low fertility rate and a very low percentage of the
# population under 15. Which region is it? Answer: Europe
ggplot(WHO, aes(x=FertilityRate, y=Under15, color=Region)) + geom_point(size=5) +
  scale_color_brewer(palette="Dark2") # Good palette for colrblind people

# Quick questions 2 (THE ANALYTICAL POLICEMAN)
# --------------------------------------------

# 1) The Los Angeles Police Department sees the benefits of predictive policing as which of the following?
#    Answer: Allowing more intelligent officer deployment, Preventing crime, Using resources more effectively

# 2) For which of the following situations would a heat map be an appropriate visualization choice?
#    Answer: 1) Visualizing the areas on a geographical map with the most crime
#            2) Comparing crime counts by police district and time throughout a city

# 3) geom_line(aes(group=1), linetype=2): Answer: Makes the line dashed 
#    alpha=0.3: Answer: Makes the line lighter in color
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), color="blue", linetype=2, alpha=.3) +
  xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# 4)
# Change the color scheme (this the original plot from the video)
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())
# Switch x and y values
ggplot(DayHourCounts, aes(y = Hour, x = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())
# Get grayscale, change high
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(name="Total MV Thefts", low="white", high="black") + theme(axis.title.y = element_blank())
# Which argument(s) did we change to get Plot (2)? Answer: x and y
# Which argument(s) did we change to get Plot (3)? Answer: high

# 5) Answer: We removed 952 observations
# Remove the lat/lon coords out on the water (Freq = 0)
LatLonCounts2 <- subset(LatLonCounts, Freq > 0)
nrow(LatLonCounts) - nrow(LatLonCounts2)
ggmap(chicago) + geom_point(data = LatLonCounts2, aes(x = Long, y = Lat, color = Freq, size=Freq)) +
  scale_colour_gradient(low="yellow", high="red")

# 6) Montana has the highest gun ownership rate in the US (among the states in the list, Wyoming has the highest)
barplot(sort(tapply(murderMap$GunOwnership, murderMap$State, mean)), las=2, col="wheat", main="Gun ownership by state")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "black", high = "red", guide = "legend") +
  ggtitle("US gun ownership rate by state")

# ---------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK:

# 1) ELECTION FORECASTING REVISITED
# ---------------------------------

# PROBLEM 1.1 - DRAWING A MAP OF THE US. Answer: There are 63 groups
statesMap <- map_data("state")
str(statesMap)
table(statesMap$group)
# The variable "order" defines the order to connect the points within each group
# The variable "region" gives the name of the state.
# The variable "group" efines the different shapes or polygons on the map.
# (Sometimes a state may have multiple groups, for example, if it includes islands)

# PROBLEM 1.2 - DRAWING A MAP OF THE US. Answer: color
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")
# We specified two colors in geom_polygon -- fill and color. Which one defined the color of the outline of the states?

# PROBLEM 2.1 - COLORING THE STATES BY PREDICTIONS.
# Answer: 1) 22 predictions in TestPredictionBinary for Republican (outcome 1)
# Answer: 2) 0.4853 is the average predicted probability of our model (on the Test set, for 2012)
polling <- read.csv(paste0(folder, "PollingImputed.csv"))
str(polling)
head(polling)
Train <- subset(polling, Year == 2004 | Year == 2008)
Test <- subset(polling, Year == 2012)
mod2 <- glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction <- predict(mod2, newdata=Test, type="response")
# TestPrediction gives the predicted probabilities for each state,
# but let's also create a vector of Republican/Democrat predictions:
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)
# For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(TestPredictionBinary)
mean(TestPrediction)

# PROBLEM 2.2 - COLORING THE STATES BY PREDICTIONS. Answer: 15034 rows in predictionMap, 15537 in statesMap
# Convert the Test.State variable to lowercase, so that it matches the region variable in statesMap:
predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)
# Now, merge the two data frames using the following command:
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
# Lastly, we need to make sure the observations are in order so that the map is drawn properly:
predictionMap <- predictionMap[order(predictionMap$order),]
nrow(predictionMap)
nrow(statesMap)

# PROBLEM 2.3 - COLORING THE STATES BY PREDICTIONS. Answer: Because we only make predictions for 45 states,
# we no longer have observations for some of the states. These observations were removed in the merging process.
table(predictionDataFrame$region)
table(statesMap$region)

# PROBLEM 2.4 - COLORING THE STATES BY PREDICTIONS. Answer: Light blue (republican = 1)
# The states appear light blue and dark blue in this map. Which color represents a Republican prediction?
# Color the states according to our binary predictions: 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black")

# PROBLEM 2.5 - COLORING THE STATES BY PREDICTIONS. Answer: The two maps look very similar.
#   This is because most of our predicted probabilities are close to 0 or close to 1. 
# Replot the map with discrete outcomes (since prediction was binary):
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")
# Color by probability:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")
# Prob and binary are very similar:
par(mfrow=c(2,1))
plot(sort(TestPrediction), pch=21, bg="cyan", main="TestPrediction", col="blue")
plot(sort(TestPredictionBinary), pch=21, bg="cyan", main="TestPredictionBinary", col="blue")
par(mfrow=c(1,1))

# PROBLEM 3.1 - UNDERSTANDING THE PREDICTIONS. Answer:
# We incorrectly predicted this state by predicting that it would be won by the Republican party.
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

# PROBLEM 3.2 - UNDERSTANDING THE PREDICTIONS.
# Answer: 1) Prediction for Florida: 0.964
# Answer: 2) It implies that our prediction model did not do a very good job of correctly predicting
# the state of Florida, and we were very confident in our incorrect prediction
unique(predictionMap[predictionMap$region=="florida","TestPrediction"])

# PROBLEM 4.1 - PARAMETER SETTINGS. Answer: 1) linetype 2) size
# Dashed polygon outline (linetype)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black", linetype=3) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")
# Thick polygon line (size)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black", size=2) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

# PROBLEM 4.2 - PARAMETER SETTINGS. Answer: 1) alpha
# Transparent color (alpha)
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black", alpha=.3) +
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1),
                      labels = c("Democrat", "Republican"), name = "Prediction 2012")

# 2) VISUALIZING NETWORK DATA
# ---------------------------

# Data from: http://i.stanford.edu/~julian/pdfs/nips2012.pdf

# The first file we will use, edges.csv, contains variables V1 and V2, which label the endpoints of edges in our network.
# Each row represents a pair of users in our graph who are Facebook friends. For a pair of friends A and B, edges.csv will
# only contain a single row -- the smaller identifier will be listed first in this row. From this row, we will know that A
# is friends with B and B is friends with A.

# The second file, users.csv, contains information about the Facebook users, who are the vertices in our network.
# This file contains the following variables:
# - id: A unique identifier for this user; this is the value that appears in the rows of edges.csv
# - gender: An identifier for the gender of a user taking the values A and B. Because the data is anonymized, we don't know
#   which value refers to males and which value refers to females.
# - school: An identifier for the school the user attended taking the values A and AB (users with AB attended school A as well
#   as another school B). Because the data is anonymized, we don't know the schools represented by A and B.
# - locale: An identifier for the locale of the user taking the values A and B. Because the data is anonymized, we don't know
#   which value refers to what locale.

# PROBLEM 1.1 - SUMMARIZING THE DATA. Answer: 1) 59 Facebook users in "users" data frame 2) TODO...
edges <- read.csv(paste0(folder, "edges.csv"))
users <- read.csv(paste0(folder, "users.csv"))
str(edges)
str(users)
nrow(users)
tapply(edges$V1, edges$V2, mean)
mean(table(edges$V1, edges$V2) %*% t(table(edges$V1, edges$V2))) # Nope...
Heatmap(as.table((table(edges$V1, edges$V2) %*% t(table(edges$V1, edges$V2)))))

# PROBLEM 1.2 - SUMMARIZING THE DATA. Answer: Locale B
# Out of all the students who listed a school, what was the most common locale?
table(users$locale)

# PROBLEM 1.3 - SUMMARIZING THE DATA. Answer: No
# Is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school, deparse.level=2)

# PROBLEM 2.1 - CREATING A NETWORK. Answer: g <- graph.data.frame(edges, FALSE, users)
# Which of the following commands will create a graph g describing our social network, with the attributes of each user
# correctly loaded?
library(igraph)
?graph.data.frame()
# This function creates an igraph graph from one or two data frames containing the (symbolic) edge list (d)
# and edge/vertex attributes (vertices).
# Usage: graph.data.frame(d, directed=TRUE, vertices=NULL)
igraph.options(label.cex=1)
g <- graph.data.frame(edges, FALSE, users)
g <- set.graph.attribute(g, "layout", layout.kamada.kawai(g))
plot(g, vertex.size=8, vertex.color="green") #??plot.igraph
# From ?graph.data.frame, we can see that the function expects the first two columns of parameter d to specify the edges in the graph -- our edges object fits this description.
# Our edges are undirected -- if A is a Facebook friend of B then B is a Facebook friend of A. Therefore, we set the directed parameter to FALSE.
# The vertices parameter expects a data frame where the first column is a vertex id and the remaining columns are properties of vertices in our graph. This is the case with our users data frame.

# PROBLEM 2.2 - CREATING A NETWORK. Answer: See below.
g <- graph.data.frame(edges, FALSE, users) # NOTE: USEFUL!
plot(g, vertex.size=5, vertex.label=NA, main="Facebook friends network")
# 1) In this graph, there are a number of groups of nodes where all the nodes in each group are connected but the groups are
# disjoint from one another, forming "islands" in the graph. Such groups are called "connected components," or "components" 
# for short. How many connected components with at least 2 nodes are there in the graph? Answer: 4 "connected components"
# 2) How many users are there with no friends in the network? Answer: 7 users

# PROBLEM 2.3 - CREATING A NETWORK. Answer: 9 users have 10 or more friends
# In our graph, the "degree" of a node is its number of friends
friends <- degree(g)
friends
length(friends[friends >= 10])
plot(sort(friends), pch=16, col="blue", main="Facebook Friends per user")
abline(h=9, col="red") # 10 or more

# PROBLEM 2.4 - CREATING A NETWORK. Answer: See below,
# 1) What is the largest size we assigned to any node in our graph? answer: 11
# 2) What is the smallest size we assigned to any node in our graph? answer: 2
V(g)$size <- degree(g)/2+2
plot(g, vertex.label=NA, main="Facebook friends network")
sort(V(g)$size)

# PROBLEM 3.1 - COLORING VERTICES. Answer: What is the gender of the users with the highest degree in the graph? Gender B (gray)
V(g)$color <- "black" # set default black for all first (so black will be the missing gender values in the end)
V(g)$color[V(g)$gender == "A"] <- "red"
V(g)$color[V(g)$gender == "B"] <- "gray"
plot(g, vertex.label=NA, main="Facebook friends network, colored by gender")

# PROBLEM 3.2 - COLORING VERTICES. Answer: See below.
# 1) Are the two users who attended both schools A and B Facebook friends with each other? Answer: Yes (two yellow vertices)
# 2) What best describes the users with highest degree? Answer: Some, but not all, of the high-degree users attended school A
table(users$school)
V(g)$color <- "black" # set default black for all first (so black will be the missing gender values in the end)
V(g)$color[V(g)$school == "A"] <- "red"
V(g)$color[V(g)$school == "AB"] <- "yellow"
plot(g, vertex.label=NA, main="Facebook friends network, colored by school")

# PROBLEM 3.3 - COLORING VERTICE. Answer: See below.
# 1) The large connected component is most associated with which locale? Answer: locale B (yellow vertices)
# 2) The 4-user connected component is most associated with which locale? Answer: locale A (red vertices)
table(users$locale)
V(g)$color <- "black" # set default black for all first (so black will be the missing gender values in the end)
V(g)$color[V(g)$locale == "A"] <- "red"
V(g)$color[V(g)$locale == "B"] <- "yellow"
plot(g, vertex.label=NA, main="Facebook friends network, colored by locale")

# PROBLEM 4 - OTHER PLOTTING OPTIONS. Answer: See below.
?igraph.plotting
# 1) Which igraph plotting function would enable us to plot our graph in 3-D?
#    Answer: rglplot is an experimental function to draw graphs in 3D using OpenGL.
#    See rglplot for some more information.
rglplot(g, vertex.label=NA, main="Facebook friends network, colored by locale")
# 2) What parameter to the plot() function would we use to change the edge width when plotting g?
#    Answer: edge.width
plot(g, vertex.label=NA, main="Facebook friends network, colored by locale", edge.width=2)


# 3) VISUALIZING TEXT DATA USING WORD CLOUDS 
# ------------------------------------------

# Data from: https://courses.edx.org/c4x/MITx/15.071x_2/asset/tweets.csv
tweets <- read.csv(paste0(folder, "tweets.csv"), stringsAsFactors=F)
str(tweets)
# Tweet -- the text of the tweet
# Avg -- the sentiment of the tweet, as assigned by users of Amazon Mechanical Turk. The score ranges on a scale from -2 to 2,
# where 2 means highly positive sentiment, -2 means highly negative sentiment, and 0 means neutral sentiment.

# PROBLEM 1.1 - PREPARING THE DATA. Answer: There are 3780 unique words across all documents
# See unit 5
# Video 5
# Read in the data
# Create dependent variable
tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)
# Install new packages
# install.packages("tm")
library(tm)
# install.packages("SnowballC")
library(SnowballC)
# Create corpus
corpus <- Corpus(VectorSource(tweets$Tweet))
# Look at corpus
corpus[[1]]
# Convert to lower-case
corpus <- tm_map(corpus, tolower)
corpus[[1]]
# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing
# (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred
# after this video was recorded.
corpus <- tm_map(corpus, PlainTextDocument)
# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]
# Remove english stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]
# Stem document 
#corpus <- tm_map(corpus, stemDocument)
#corpus[[1]]
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
unique(allTweets)
dim(allTweets)

# PROBLEM 1.2 - PREPARING THE DATA. Answer: It will be easier to read and understand the word cloud if it includes full words
# instead of just the word stems
# Although we typically stem words during the text preprocessing step, we did not do so here. What is the most compelling
# rationale for skipping this step when visualizing text data?

# PROBLEM 2.1 - BUILDING A WORD CLOUD. Answer: colnames
# Which function can we apply to allTweets to get a vector of the words in our dataset, which we'll pass as the first
# argument to wordcloud()?
library(wordcloud)

# PROBLEM 2.2 - BUILDING A WORD CLOUD. Answer: colSums
# Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
words <- colnames(allTweets)
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2.5, 0.35), random.color=T)

# PROBLEM 2.3 - BUILDING A WORD CLOUD. Answer: The most frequent word is "apple" (the company)

# PROBLEM 2.4 - BUILDING A WORD CLOUD. Answer: The most frequent word is now "iphone"
corpus <- tm_map(corpus, removeWords, "apple") # Remove the most frequennt word from the corpus
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
words <- colnames(allTweets)
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2.5, 0.35), colors="blue")

# PROBLEM 3.1 - SIZE AND COLOR. Answer: Word Cloud C
# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2.5, 0.35), random.order=F)

# PROBLEM 3.2 - SIZE AND COLOR. Answer: Word Cloud A
# Only one word cloud was created without modifying parameters min.freq or max.words. Which word cloud is this?

# PROBLEM 3.3 - SIZE AND COLOR. Answer: Word Cloud B and D
# Which word clouds were created with parameter random.order set to FALSE?

# PROBLEM 3.4 - SIZE AND COLOR. Answer: Word Cloud A
# Which word cloud was built with a non-default value for parameter rot.per?
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2.5, 0.35), rot.per=-.5)
# rot.per controls the proportion of words that are rotated to be vertical in the word cloud. By default 10% of
# words are rotated. However in Word Cloud A a much higher proportion (50%) are rotated, which was achieved by
# setting rot.per=0.5.

# PROBLEM 3.5 - SIZE AND COLOR. Answer: Word Cloud D
# For which word cloud was the parameter random.color set to TRUE?

# PROBLEM 4.1 - SELECTING A COLOR PALETTE. Answer: YIOrRed (a range from ligh yellow to dark red)
# See: colorbrewer.org, colorbrewer2.org
# Which color palette would be most appropriate for use in a word cloud for which we want to use color to
# indicate word frequency?
library(RColorBrewer)
display.brewer.all()
brewer.pal(name="Accent", n=8)

# PROBLEM 4.2 - SELECTING A COLOR PALETTE. Answer: "Greys"
# Which RColorBrewer palette name would be most appropriate to use when preparing an image for a document that
# must be in grayscale?
brewer.pal.info
greys <- brewer.pal(name="Greys", n=8)
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2, 0.35), colors=greys)

# PROBLEM 4.3 - SELECTING A COLOR PALETTE. Answer: rewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2, 0.35), colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordCloud <- wordcloud(words, colSums(allTweets), scale=c(2, 0.35), colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
brewer.pal(9, "Blues")


# 4) VISUALIZING ATTRIBUTES OF PAROLE VIOLATORS (OPTIONAL)
# --------------------------------------------------------
# Data from Unit 3:
parole <- read.csv(paste0(folder, "parole.csv"))
str(parole)
# male = 1 if the parolee is male, 0 if female
# race = 1 if the parolee is white, 2 otherwise
# age = the parolee's age in years at the time of release from prison
# state = a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. These three states were selected due to having a high representation in the dataset.
# time.served = the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence = the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses = 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime = a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator = 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

# PROBLEM 1.1 - LOADING THE DATA. Answer: 14 females (fraction = 0.1795)
parole$male <- as.factor(parole$male)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
table(parole$violator)
parole.violated <- subset(parole, violator == 1)
table(parole.violated$male)
table(parole.violated$male)[1] / nrow(parole.violated) # Female 0 = 130

# PROBLEM 1.2 - LOADING THE DATA. Answer: 3 (drug-related crime, category 3) is the most common crime in Kntucky (state = 2)
kentucky <- subset(parole, state == 2)
table(kentucky$crime)

# PROBLEM 2.1 - CREATING A BASIC HISTOGRAM. Answer: Most frequent age bracket: 20-24
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, fill="wheat", color="black") + theme_bw() +
  ggtitle("Age")

# PROBLEM 2.2 - CREATING A BASIC HISTOGRAM. Answer: color="blue" changes the outline color of the bars
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, color="blue") + theme_bw() +
  ggtitle("Age")

# PROBLEM 3.1 - ADDING ANOTHER DIMENSION. What is the age bracket with the most female parolees? Answer: 35-39
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, aes(fill=male), color="black") +
  facet_grid(male ~ ., scales="free") + scale_fill_discrete(name="Sex", labels=c("Female","Male")) +
  ggtitle("Parolees - Age distribution per sex")
# Better with no scales=free for this one:
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, aes(fill=male), color="black") +
  facet_grid(male ~ .) + scale_fill_discrete(name="Sex", labels=c("Female","Male")) +
  ggtitle("Parolees - Age distribution per sex")

# PROBLEM 3.2 - ADDING ANOTHER DIMENSION. Answer: facet_grid(. ~ male) puts the histograms side-by-side instead of on top of each other. 
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, aes(fill=male), color="black") +
  facet_grid(. ~ male) + scale_fill_discrete(name="Sex", labels=c("Female","Male")) +
  ggtitle("Parolees - Age distribution per sex")

# PROBLEM 3.3 - ADDING ANOTHER DIMENSION. Answer: color of female parolees is black
# NOTE: The histograms are STACKED on top of each other here, not put in front of one another!
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, color="black")
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values=colorPalette)

# PROBLEM 3.4 - ADDING ANOTHER DIMENSION. Which of the following buckets contain no female paroles? Answer: 15-19, 55-59, 65-69
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# position="identity": Don't stack histogram bars
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, alpha=.5, position="identity") +
  scale_fill_manual(values=colorPalette)

# PROBLEM 4.1 - TIME SERVED. What is the most common length of time served, according to this histogram? Answer: Between 4 and 5 months
ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 1, alpha=.5, position="identity") +
  scale_fill_manual(values=colorPalette) + xlab("Time served in months") + ggtitle("Time served in months")

# PROBLEM 4.2 - TIME SERVED. Now what is the most common length of time served, according to the histogram? Answer: 3.0-3.1 months
ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 0.1, alpha=.5, position="identity") +
  scale_fill_manual(values=colorPalette) + xlab("Time served in months") + ggtitle("Time served in months/10")

# PROBLEM 4.3 - TIME SERVED. Answer: See below.
# Which crime type has no observations where time served is less than one month? Recall that crime type #2 is larceny,
# #3 is drug-related crime, #4 is driving-related crime, and #1 is any other crime.
# 1) Answer: 4 (driving related crime)
# For which crime does the frequency of 5-6 month prison terms exceed the frequencies of each other term length?
# 2) Answer: 3 (drug-related crime)
colorPalette = c("#FF8800", "#0088FF")
ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = .5, alpha=.6, position="identity") +
  facet_grid(. ~ crime) +
  scale_fill_manual(values=colorPalette) + xlab("Time served in months") + ggtitle("Time served in months, per crime category")

# PROBLEM 4.4 - TIME SERVED. Answer: With four different groups, it can be hard to tell them apart when they are overlayed.
# n this case, faceting seems like a better alternative. Why?
colorPalette = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")
ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = .5, alpha=.6, position="identity") +
  scale_fill_manual(values=colorPalette, name="Crime category", labels=c("Other","Larceny","Drug-related","Driving")) +
  xlab("Time served in months") + ggtitle("Time served in months, per crime category")
# 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime
