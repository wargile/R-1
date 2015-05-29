# dplyr tutorials
# http://datascience.la/hadley-wickhams-dplyr-tutorial-at-user-2014-part-1/
# https://www.dropbox.com/sh/i8qnluwmuieicxc/AAAgt9tIKoIm7WZKIyK25lh6a
# https://github.com/dgrapov/TeachingDemos/blob/master/Demos/dplyr/hands_on_with_dplyr.md

package.install("nycflights13")

library(nycflights13)

library(dplyr)

data(flights)
head(flights, n=1)
dim(flights)
summary(flights)
str(flights)

data(airports)
head(airports, n=1)
dim(airports)
summary(airports)
str(airports)

data(planes)
head(planes, n=1)
dim(planes)
summary(flights)
str(planes)

data(weather)
head(weather, n=1)
dim(weather)
summary(flights)
str(flights)

# flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE)) 

filter(weather, month == 2)