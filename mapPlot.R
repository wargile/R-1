# Map plot tips/code: https://github.com/alexsingleton/Transport-Map-Book/blob/master/Map%20Book%20Code/Map_Book_Code.R

# State Indicator Report on Physical Activity, 2014, Behavioral Indicators
# http://www.cdc.gov/physicalactivity/downloads/pa_state_indicator_report_2014.pdf
library(maps)
library(ggplot2)
#library(RColorBrewer)
#library(scales)

file <- "C:/coding/R/TestData/pa_state_indicator_report_2014_RAW.txt"

n <- 7
data <- readLines(file)
data
header <- data[1:n] # Get the column names
header <- gsub(" ", ".", header) # Fix the column names
header <- gsub("-", ".", header) # Fix the column names
header

df <- data.frame(matrix(data[(n + 1):length(data)], ncol=7, byrow=T), stringsAsFactors=F)
names(df) <- header
head(df)
dim(df)
df$State <- as.factor(df$State)
df[, 2:7] <- apply(df[, 2:7], 2, function(x) as.numeric(as.character(x)))
sapply(df, class)
names(df)

#df <- sapply(df[, 2:7], function(x) x[which(is.na(x))] <- 0)

# Create a choropleth map:
# http://en.wikipedia.org/wiki/Choropleth_map

# http://www.cdc.gov/brfss/
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html
# http://www.mapsofworld.com/usa/usa-state-and-capital-map.html
df <- df[-1, ] # Skip National row
df$State <- tolower(df$State)
all_states <- map_data("state")
choropleth <- merge(x=df, y=all_states, by.x="State", by.y="region")
col <- 2
data <- choropleth[, col]
#data <- rowSums(choropleth[, 2:7])
colors <- rgb(Normalize(min(data), max(data), .1, .9, data), Normalize(min(data), max(data), .1, .9, data),
              Normalize(min(data), max(data), .25, .9, data),)
colors <- rgb(0.3, Normalize(min(data), max(data), .3, 1, data), .8)
colors <- rgb(0, Normalize(min(data), max(data), 0, 1, data), 0)
colors <- rgb(0.5, 0.5, Normalize(min(data), max(data), 0.5, 1, data))
colors <- rgb(0.3, 0.3, Normalize(min(data), max(data), 0.3, 1, data))
colors <- rgb(Normalize(min(data), max(data), .7, 1, data), Normalize(min(data), max(data), .2, .9, data), 0)
ggplot() + geom_polygon(data=choropleth, aes(x=long, y=lat, group=group), colour="gray", fill=colors) +
  ggtitle(paste0("US State Indicator Report on Physical Activity, 2014, Behavioral Indicators, Adults\n(",
                 names(df)[col], ")"))

