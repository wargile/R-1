# Testing the SparkR package (included with Spark 1.4, June 2015)
# https://amplab-extras.github.io/SparkR-pkg/
# http://people.apache.org/~pwendell/spark-releases/latest/sparkr.html
# http://blog.rstudio.org/2015/05/28/sparkr-preview-by-vincent-warmerdam/
# http://ampcamp.berkeley.edu/5/exercises/sparkr.html

# To build SparkR on a fresh download/build of Spark, launch c:/Spark/R/install-dev.bat

set.seed(1000)

source("tools.R")
library(caret)
library(randomForest)
library(gbm)
package.install("readr")
library(readr)
library(caTools)
library(wordcloud)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
# install.packages("Metrics") # ?MeanQuadraticWeightedKappa
library(Metrics)
library(SnowballC)
library(tm)
# etc.

# Need: library(rJava)
package.install("rJava")
# install.packages('rJava', .libPaths()[1], 'http://www.rforge.net/')
options(java.home="C:\\Azul\\zulu1.7.0_65-7.6.0.1-win64\\jre") # IMPORTANT! SET THIS FIRST!
library(rJava)

# library(devtools)
# install_github("amplab-extras/SparkR-pkg", subdir="pkg")
# TODO: mvn.bat compile problem, and also wrong hadoop and spark version set in command string

Sys.setenv('SPARKR_SUBMIT_ARGS'='"--packages" "com.databricks:spark-csv_2.10:1.0.3" "sparkr-shell"')

SetStandardOptions()

# Set this to where Spark is installed
# Sys.setenv(SPARK_HOME="C:/spark")
# This line loads SparkR from the installed directory
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), .libPaths()))
library(SparkR)
sc <- sparkR.init(master="local", sparkJars="com.databricks:spark-csv_2.10:1.0.3")
sc <- sparkR.init(master="local", appName="TestingSomeSparkR")
# TODO: Add spark jars param, etc.!
args(sparkR.init)
# ./bin/sparkR --packages com.databricks:spark-csv_2.10:1.0.3
# https://github.com/amplab-extras/SparkR-pkg/blob/master/pkg/R/sparkR.R

sqlContext <- sparkRSQL.init(sc)
sqlContext

people <- read.df(sqlContext, file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json"), "json")
head(people)

path <- file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json")
peopleDF <- jsonFile(sqlContext, path)
printSchema(peopleDF)

# http://www.r-bloggers.com/sparkr-distributed-data-frames-with-spark-and-r/
# # Download the nyc flights dataset as a CSV from https://s3-us-west-2.amazonaws.com/sparkr-data/nycflights13.csv
flights <- read.df(sqlContext, "c:/coding/R/TestData/nycflights13.csv", "com.databricks.spark.csv", header="true")

hiveContext <- sparkRHive.init(sc)
# TODO: Need to build Spark SQL with Hive support (how?)

sparkR.stop()
