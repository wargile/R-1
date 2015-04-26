# MapToolbox.R -- Various map/geospatial functions
# ------------------------------------------------

# TODO: Get coords lat/lon data for Europe and U.S.
# 
# Data sources:
# http://ec.europa.eu/eurostat/web/main/home (Eurostats, EU Statistics Office)

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
library(mapproj)
library(ggvis)
library(dplyr)

# Set locale to US, to ensure that there aren't formatting issues in assignments/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

# Load the map of the US
states.Map <- map_data("state")
str(states.Map)
US.Map = map_data("usa")
str(US.Map)
World.Map <- map_data("world")
str(World.Map)

# Only US locations?
chicago <- get_map(location="chicago", zoom=11)
# Look at the map
ggmap(chicago)

# http://www.milanor.net/blog/?p=594
Europe.map <- get_map(location='Europe', zoom=4)
str(Europe.map)
ggmap(Europe.map) + ggtitle("Map of Europe")

US.Map = map_data("usa")
str(US.Map)
dim(US.Map)
ggplot(US.Map, aes(x = long, y = lat, group = group, fill=1:nrow(US.Map))) + geom_polygon(color = "black") +
  scale_fill_gradient(low = "blue", high = "white", guide = "legend") + ggtitle("US")

# http://bcb.dfci.harvard.edu/~aedin/courses/R/CDC/maps.html
map("world", "Norway")
map.cities(country="Norway", capitals=2)
map("county", col=1:50, fill=T, mar=c(1,1,1,1))

# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

