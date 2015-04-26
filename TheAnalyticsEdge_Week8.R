# The Analytics Edge week 8 - Linear Optimization
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/optimization/
# ------------------------------------------------------------------------------------

# http://en.wikipedia.org/wiki/Linear_programming
# Dantzig's simplex algorithm/method: http://en.wikipedia.org/wiki/Simplex_algorithm
# Simplex: http://en.wikipedia.org/wiki/Simplex
# http://en.wikipedia.org/wiki/Regular_polytope
# http://en.wikipedia.org/wiki/Convex_set (Euclidean space)
# http://en.wikipedia.org/wiki/Convex_hull

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

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 8/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) 
# ----------------------------------------------------------

# In a convex polygon, a line segment between two points on the boundary never goes outside the polygon.
# In a convex polygon, all interior angles are less than or equal to 180 degrees

# A simple (non-self-intersecting) polygon that is not convex is called concave, non-convex or reentrant.
# A simple concave polygon will always have an interior angle with a measure that is greater than 180 degrees.

# https://stat.ethz.ch/R-manual/R-devel/library/boot/html/simplex.html
library(boot)
# This example is taken from Exercise 7.5 of Gill, Murray and Wright (1991).
enj <- c(200, 6000, 3000, -200)
fat <- c(800, 6000, 1000, 400)
vitx <- c(50, 3, 150, 100)
vity <- c(10, 10, 75, 100)
vitz <- c(150, 35, 75, 5)
simplex(a = enj, A1 = fat, b1 = 13800, A2 = rbind(vitx, vity, vitz),
        b2 = c(600, 300, 550), maxi = TRUE)

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 ()
# -----------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK:

# 1) 
# ---------------------------------

