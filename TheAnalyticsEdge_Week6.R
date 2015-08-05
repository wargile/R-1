# The Analytics Edge week 6 - Clustering
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/clustering/
# ------------------------------------------------------------------------------------

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)

# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 6/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) RECOMMENDATIONS WORTH A MILLION: AN INTRODUCTION TO CLUSTERING
# -----------------------------------------------------------------
# Collaborative filtering: Using other user's ratings, and the similarities between users,
# to make predictions/recommendations for a user. Requires lots of data, but can accurately suggest complec items-
# Content filtering: Based on properties of a movie person N liked, recommend other movies that are on the
# same theme, have the same actor in it, have the same director, etc. as the movie person N liked and rated high.
# Does not requre much data to get started, but can be limited in scope.

# Hybrid recommendation systems (like the Netflix system) uses both techniques.

# Why clustering: clustering is used to group similar data into clusters, and THEN make predictions on each group.
# Clustering is NOT a predictive method! Works best for large datasets. Clustering is "Unsupervised learning".

# Define the distance between two datapoints using Euclidean Distance:
Toy.Story <- c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
Batman.Forever <- c(0,1,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
sqrt(sum((Toy.Story - Batman.Forever)^2)) # Euclidean Distance between the two movies: 2.236
# Other popular measurements:
# - Manhattan distance: Sum of absolute values instead of squares
# - Maximum Coordinate Distance: Only consider measurement for which data points deviate the most

# Finding distance between clusters:
# 1. Get the Minimum Distance: The distance between the two points (one in each cluster) that are the closest to each other.
# 2. Get the Maximum Distance: The distance between the two points (one in each cluster) that are the farthest apart.
# 3. Centroid Distance: The distance between the centroids (the point that has the average distance of all the points in the
# cluster) of the two clusters. This is the most common method.

# Important to normalize data when using clustering. For instance, movie genre variables versus Box Office Revenue.
# Normalize: (x - mean(x)) / sd(x)
x1 <- runif(10)
x2 <- runif(10) * 1000
norm.x1 <- (x1 - mean(x1)) / sd(x1)
norm.x1
norm.x2 <- (x2 - mean(x2)) / sd(x2)
norm.x2

# Hierarchical Clustering:
# Dendrogram: The graph is "bottom up", we start at the bottom. The length of the vertical lines shows how far apart
# the points were when we clustered them. The farthest the horizontal line can move up and down in the dendrogram, the 
# better the choice of clusters are.
# In general:
# - Always consider how many clusters make sens for the particular application one is working with.
# - See if the clusters have a feature (like an outcome variable) in each cluster that was not used in the clustering process.
# - Look at statistics (mean, max, etc.) for each cluster and each variable.
movies <- read.table("http://files.grouplens.org/datasets/movielens/ml-100k/u.item", header=F, sep="|", quote="\"")
head(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
# Remove duplicates
movies = unique(movies)
# Take a look at our data again:
str(movies)
distances <- dist(movies[2:20], method="euclidean")
class(distances) # class 'dist'
attr(distances, "Labels")
attr(distances, "Size")
length(distances)
# Create clusters on the 'distances' class:
clusterMovies <- hclust(distances, method="ward.D")
# plot(clusterMovies) # Makes R crash...
clusterGroups <- cutree(clusterMovies, k=10) # Cut the tree into 10 groups of data
barplot(tapply(corpus$accessori, clusterGroups, mean), main="Mean 'accesori by clusterGroup")
barplot(tapply(corpus$cotton, clusterGroups, mean), main="Mean 'cotton' by clusterGroup")
# Above gives the percentage of movies belonging in the Action and Romance clusters respectively
df <- do.call(rbind, lapply(movies[2:20], function(x) tapply(x, clusterGroups, mean)))
df <- round(df, 2)
colnames(df) <- paste0("ClusterGroup", 1:10)
write.table(df, file=paste0(folder, "movie_genres.csv"), sep=";")
subset(movies, Title=="Men in Black (1997)") # in row 257
clusterGroups[257] # The movie is in clusterGroup 2
table(clusterGroups == 2)
cluster2 <- subset(movies, clusterGroups == 2) # NOTE syntax for getting the movies where clusterGroups == T
cluster2$Title[1:10]
# Conclusion: Clustering forms the backbone of recommendation systems in many big companies. Some use content
# filtering, some use collaborative filtering, some use both (like Netflix).

# 2) PREDICTIVE DIAGNOSIS: DISCOVERING PATTERNS FOR DISEASE DETECTION
# -------------------------------------------------------------------

# Predicting heart attack: We will use health insurance claims filed for about 7,000 members
# from January 2000 - November 2007.

# Used clustering to separate patients into buckets before creatng models/predictions on each bucket.

# 3) SEEING THE BIG PICTURE: SEGMENTING IMAGES TO CREATE DATA (Recitation)
# ------------------------------------------------------------------------
# Unit 6 - Recitation

# Clustering methods: Partition image to clusters based on difference in pixel colors, intensity or texture
# Edge detection: Based on the detection of discontinuity, such as an abrupt change in the gray level in gray-scale images
# Region-growing methods: Divides image into regions, then sequentially merges sufficiently similar regions

# Video 2
flower <-read.csv(paste0(folder, "flower.csv"), header=FALSE) # Gray-scale flower image
str(flower)
# Change the data type to matrix
flowerMatrix = as.matrix(flower)
image(flowerMatrix, asp=1)
str(flowerMatrix)
# Turn matrix into a vector
flowerVector <- as.vector(flowerMatrix) # Need to first confine to a matrix, and then to vector
str(flowerVector)
flowerVector2 = as.vector(flower) # Does not work, is still treated as 50 variables/cols and 50 rows
str(flowerVector2)
dim(flowerVector2)
# Compute distances
distance <- dist(flowerVector, method="euclidean")
# Video 3
# Hierarchical clustering
clusterIntensity <- hclust(distance, method="ward.D")
# Plot the dendrogram
plot(clusterIntensity)
# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")
flowerClusters <- cutree(clusterIntensity, k = 3) # Cuts the dendrogram into the k clusters we want
table(flowerClusters)
# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)
# Plot the image and the clusters
dim(flowerClusters) <- c(50,50)
image(flowerClusters, axes = FALSE, asp=1)
# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)), asp=1)
# Video 4
# Let's try this with an MRI image (higher resolution) of the brain
healthy <- read.csv(paste0(folder, "healthy.csv"), header=FALSE)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))
# Hierarchial clustering
healthyVector <- as.vector(healthyMatrix)
distance <- dist(healthyVector, method = "euclidean")
# We have an error - memory allocation limit reached
str(healthyVector)
# Video 5
# Specify number of clusters
k = 5
# Run k-means
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
# Extract clusters
healthyClusters <- KMC$cluster
KMC$centers[2]
# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col=rainbow(k), asp=1)
# Video 6
# Apply to a test image
tumor <- read.csv(paste0(folder, "tumor.csv"), header=FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)
# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)
KMC.kcca <- as.kcca(KMC, healthyVector)
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)
# Visualize the clusters
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col=rainbow(k), asp=1)

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (RECOMMENDATIONS WORTH A MILLION: AN INTRODUCTION TO CLUSTERING)
# ----------------------------------------------------------------------------------
# 1) About how many years did it take for a team to submit a 10% improvement over Cinematch? Answer: 2.5 years

# 2) If Amazon.com constructs a recommendation system for books, and would like to use the same exact algorithm
# for shoes, what type would it have to be? 
# - Answer: Collaborative Filtering
# If Amazon.com would like to suggest books to users based on the previous books they have purchased, what type
# of recommendation system would it be?
# - Answer: Content Filtering

# 3) Which of the following tasks do you think are appropriate for clustering?
# - Answer: Dividing search results on Google into categories based on the topic
# - Answer: Grouping players into different "types" of basketball players that make it to the NBA

# 4) Answer: Euclidean Distance between the two movies: 1.414
The.Godfather <- c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
Titanic <- c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
sqrt(sum((The.Godfather - Titanic)^2))

# 5) Suppose you are running the Hierarchical clustering algorithm with 212 observations.
# - How many clusters will there be at the start of the algorithm? Answer: 212
# - How many clusters will there be at the end of the algorithm? Answer: 1

# 6) Answer: Comedies: 502, Western: 27, Romance AND Drama: 97
table(movies$Comedy)
table(movies$Western)
table(movies$Romance == T & movies$Drama == T)

# 7) Answer: All movies in the "Drama" category are in Cluster2. Cluster1 contains all the other categories. 
clusterGroups2 <- cutree(clusterMovies, k=2) # Cut the tree into 2 groups of data
df2 <- do.call(rbind, lapply(movies[2:20], function(x) tapply(x, clusterGroups2, mean)))
df2 <- round(df2, 2)

# Quick questions 2 (PREDICTIVE DIAGNOSIS: DISCOVERING PATTERNS FOR DISEASE DETECTION)
# ------------------------------------------------------------------------------------

# 1) Answer: Logistic Regression, CART, Random Forest
# Which of the following methods is designed to be used to predict an outcome like whether or not someone will experience a heart attack?

# 2) 
# In the previous video, we discussed how we split the data into three groups, or buckets, according to cost.
# Which bucket has the most data, in terms of number of patients? Answer: Bucket 1 (lowest medical cost)
# Which bucket probably has the densest data, in terms of number of claims per person?. Answer: Bucket 3 (highest medical cost)

# 3) K-means clustering differs from Hierarchical clustering in a couple important ways. Which of the following statements is true?
# Answer: In k-means clustering, you have to pick the number of clusters you want before you run the algorithm.

# 4) If you wanted to find more unusual patterns shared by a small number of people, would you increase or decrease the number of clusters?
# Answer: Increase the number of clusters, as this would create smaller clusters, making it easier to find patterns shared by just a few patients


# ---------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK:

# 1) DOCUMENT CLUSTERING WITH DAILY KOS
# -------------------------------------

# Data from: https://www.dailykos.com/
# The two most common algorithms used for document clustering are Hierarchical and k-means. 

# PROBLEM 1.1 - HIERARCHICAL CLUSTERING. Answer: We have a lot of observations, so it takes a long time to compute the distance
# between each pair of observations. We have a lot of variables, so the distance computation is long.

# Contains data on 3,430 news articles or blogs that have been posted on Daily Kos. These articles were posted in 2004, leading up to
# the United States Presidential Election. The leading candidates were incumbent President George W. Bush (republican) and
# John Kerry (democratic). Foreign policy was a dominant topic of the election, specifically, the 2003 invasion of Iraq. 
dailykos <- read.csv(paste0(folder, "dailykos.csv"))
str(dailykos)
dim(dailykos)
dailykos.matrix <- as.matrix(dailykos)
dailykos.vector <- as.vector(dailykos.matrix)
length(dailykos.vector)
distances <- dist(dailykos, method="euclidean") # Takes a loong time, so save for later use
save(distances, file=paste0(folder, "distances.rda"))
load(file=paste0(folder, "distances.rda"))

# PROBLEM 1.2 - HIERARCHICAL CLUSTERING. Answer:
clusterDailykos <- hclust(distances, method="ward.D")
library(ggdendro)
# load package ape; remember to install it: install.packages('ape')
library(ape)
# plot basic tree
plot(as.phylo(clusterDailykos), cex=0.7, label.offset=1)
hcd <- as.dendrogram(clusterDailykos)
# plot(clusterDailykos, check=T) # Makes R crash, so use alternatives...

# PROBLEM 1.3 - HIERARCHICAL CLUSTERING.
# we are trying to cluster news articles or blog posts into groups. This can be used to show readers categories
# to choose from when trying to decide what to read. Just thinking about this application, what are good choices
# for the number of clusters?
# Answer: 7 and 8 are good choices of numbers of clusters

# PROBLEM 1.4 - HIERARCHICAL CLUSTERING.
# Answer: 1) 374 observations in cluster3  2) Cluster 1 has most rows  3) cluster 4 has least rows
clusterGroups3 <- cutree(clusterDailykos, k=7) # Cut the tree into 7 groups of data
clusterList <- lapply(1:7, function(x) subset(dailykos, clusterGroups3 == x))
dim(clusterList[[3]]) # 374 observations in cluster 3
sapply(clusterList, nrow) # Cluster 1 has most rows, cluster 4 has least rows
sum(sapply(clusterList, nrow))
# should be same as:
nrow(dailykos)
hclust.clusters <- clusterList

# PROBLEM 1.5 - HIERARCHICAL CLUSTERING. Answer: "bush" is the most frequenct word in terms of highest average value
tail(sort(colMeans(clusterList[[1]])))
# This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that
# occur the most frequently

# PROBLEM 1.6 - HIERARCHICAL CLUSTERING.
# 1) Which words best describe cluster 2? Answer: november, poll, vote, challenge
# 2) Which cluster could best be described as the cluster related to the Iraq war? Answer: Cluster 5
# 3) Answer: Cluster 7 (Dean, Kerry, Edwards)
lapply(1:7, function(x) tail(sort(colMeans(clusterList[[x]]))))

# PROBLEM 2.1 - K-MEANS CLUSTERING. Answer: See below.
# Answer 1: 277 observations in kmeans cluster 3
# Answer 2: cluster 4 has the most observations (2063)
# Answer 2: cluster 2 has the fewest observations (144) 
set.seed(1000)
#km.clusters <- kmeans(distances, centers=7)
km.clusters <- kmeans(dailykos, centers=7) # Important, use data frame here if only numeric cols!
str(km.clusters)
#clusterGroups.km <- cutree(km.clusters, k=7) # Cut the tree into 7 groups of data (just for hclust)
clusterList.km <- lapply(1:7, function(x) subset(dailykos, km.clusters$cluster == x))
# save(clusterList.km, paste0(folder, "clusterList.km.rda"))
# load(paste0(folder, "clusterList.km.rda"))
KmeansClusters <- split(dailykos, km.clusters$cluster)
length(clusterList.km[[3]]) # 277 observations
sapply(clusterList.km, nrow) # Cluster 4 (2063 obs.) longest, cluster 5 (144 obs.) shortest
# or just use:
table(km.clusters$cluster)
# NOTE:
sum(sapply(clusterList.km, nrow))
# should be equal to:
nrow(dailykos)

# PROBLEM 2.2 - K-MEANS CLUSTERING. Answer: See below.
# Which k-means cluster best corresponds to the Iraq War? Answer: Cluster 3
# Which k-means cluster best corresponds to the democratic party?
# (Remember that we are looking for the names of the key democratic party leaders.) Answer: Cluster 2
lapply(1:7, function(x) tail(sort(colMeans(clusterList.km[[x]]))))

# PROBLEM 2.3 - K-MEANS CLUSTERING. Answer: 
# From "table(hierGroups, KmeansCluster$cluster)", we read that 116 (80.6%) of the observations in K-Means Cluster 2
# also fall in Hierarchical Cluster 7.
table(clusterGroups3) # hclust result
table(km.clusters$cluster) # kmeans result
table(clusterGroups3, km.clusters$cluster, deparse.level=2) # NOTE: Get result from 2.1 solution (kmeans) and 1.4 solution (hclust)
ConfusionMatrix(table(clusterGroups3, km.clusters$cluster), labels=1:7, xlab="clusterGroups3", ylab="km.clusters$cluster")

# PROBLEM 2.4 - K-MEANS CLUSTERING. Answer:
# From "table(hierGroups, KmeansCluster$cluster)", we read that 171 (61.7%) of the observations in K-Means Cluster 3 also fall in
# Hierarchical Cluster 5.

# PROBLEM 2.5 - K-MEANS CLUSTERING. Answer:  No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7
result <- table(clusterGroups3, km.clusters$cluster, deparse.level=2) # NOTE: Get result from 2.1 solution (kmeans) and 1.4 solution (hclust)
123 / colSums(result)[7] # Just 0.3994, so: No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7

# PROBLEM 2.6 - K-MEANS CLUSTERING. Answer:
# From "table(hierGroups, KmeansCluster$cluster)", we read that 320 (97.3%) of observations in K-Means Cluster 6 fall in
# Hierarchical Cluster 2.


# 2) MARKET SEGMENTATION FOR AIRLINES
# -----------------------------------

# Data from the book "Data Mining for Business Intelligence,"
# by Galit Shmueli, Nitin R. Patel, and Peter C. Bruce: http://www.dataminingbook.com/

# 1) 
airlines <- read.csv(paste0(folder, "AirlinesCluster.csv"))
str(airlines)
# Balance = number of miles eligible for award travel
# QualMiles = number of miles qualifying for TopFlight status
# BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
# BonusTrans = number of non-flight bonus transactions in the past 12 months
# FlightMiles = number of flight miles in the past 12 months
# FlightTrans = number of flight transactions in the past 12 months
# DaysSinceEnroll = number of days since enrolled in the frequent flyer program

# PROBLEM 1.1 - NORMALIZING THE DATA. Answer: See below.
# 1) Answer: BonusTrans and FlightTrans vars have the smallest mean values
# 2) Answer: Balance and BonusMiles vars have the largest mean values
summary(airlines)

# PROBLEM 1.2 - NORMALIZING THE DATA. Answer: If we don't normalize the data, the clustering will be dominated by the variables
# that are on a larger scale.

# PROBLEM 1.3 - NORMALIZING THE DATA. Answer:
# In the normalized data, which variable has the largest maximum value?  Answer: FlightMiles
# In the normalized data, which variable has the smallest minimum value? Answer: DaysSinceEnroll
library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm) # NOTE: All vars now have mean = 0

# PROBLEM 2.1 - HIERARCHICAL CLUSTERING. Answer: See below.
# Answer: If you run a horizontal line down the dendrogram, you can see that there is a long time that the line crosses 2 clusters,
# 3 clusters, or 7 clusters. However, it it hard to see the horizontal line cross 6 clusters. This means that 6 clusters is probably
# not a good choice.
airline.distances <- dist(airlinesNorm, method="euclidean")
length(airline.distances)
# Create clusters on the 'distances' class:
clusterAirlines <- hclust(airline.distances, method="ward.D")
library(ape)
# plot basic tree
plot(as.phylo(clusterAirlines), cex=0.7, label.offset=1)

# PROBLEM 2.2 - HIERARCHICAL CLUSTERING. Answer: 776 datapoints in cluster 1
clusterGroupsAirlines <- cutree(clusterAirlines, k=5) # Cut the tree into 5 groups of data
table(clusterGroupsAirlines)

# PROBLEM 2.3 - HIERARCHICAL CLUSTERING. Answer: See below.
# 1) Compared to the other clusters, Cluster 1 has the largest average values in which variables (if any)?
#    Answer: DaysSinceEnroll
# 2) How would you describe the customers in Cluster 1?
#    Answer: Infrequent but loyal customers (max DaysSinceEnroll but small in flight points)
sapply(airlines, function(x) tapply(x, clusterGroupsAirlines, mean))

# PROBLEM 2.4 - HIERARCHICAL CLUSTERING. Answer:
# 1) Compared to the other clusters, Cluster 2 has the largest average values in which variables (if any)?
#    Answer: QualMiles, FlightMiles, FlightTrans
# 2) How would you describe the customers in Cluster 2?
#    Answer: Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions.

# PROBLEM 2.5 - HIERARCHICAL CLUSTERING
# 1) Compared to the other clusters, Cluster 3 has the largest average values in which variables (if any)?
#    Answer: Balance, BonusMiles, BonusTrans
# 2) How would you describe the customers in Cluster 3?
#    Answer: Customers who have accumulated a large amount of miles, mostly through non-flight transactions

# PROBLEM 2.6 - HIERARCHICAL CLUSTERING
# 1) Compared to the other clusters, Cluster 4 has the largest average values in which variables (if any)?
#    Answer: None
# 2) How would you describe the customers in Cluster 4?
#    Answer: Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions

# PROBLEM 2.7 - HIERARCHICAL CLUSTERING
# 1) Compared to the other clusters, Cluster 5 has the largest average values in which variables (if any)?
#    Answer: None
# 2) How would you describe the customers in Cluster 5?
#    Answer: Relatively new customers who don't use the airline very often.

# PROBLEM 3.1 - K-MEANS CLUSTERING. Answer: 2 clusters have more than 1000 observations
set.seed(88)
clusterAirlines.km <- kmeans(airlinesNorm, centers = 5, iter.max = 1000)
table(clusterAirlines.km$cluster)

# PROBLEM 3.2 - K-MEANS CLUSTERING. Answer: See below.
# Do you expect Cluster 1 of the K-Means clustering output to necessarily be similar to Cluster 1 of the Hierarchical
# clustering output?
# Answer: No, because cluster ordering is not meaningful in either k-means clustering or hierarchical clustering.
clusterAirlines.km$centers # NOTE: Normalized data
sapply(airlines, function(x) tapply(x, clusterAirlines.km$cluster, mean)) # Kmeans: Result on unnormalized data
sapply(airlines, function(x) tapply(x, clusterGroupsAirlines, mean)) # Hclust: Result on unnormalized data


# 3) PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT
# -----------------------------------------------------

# Data from: http://www.infochimps.com/datasets/nasdaq-exchange-daily-1970-2010-open-close-high-low-and-volume
# Each observation in the dataset is the monthly returns of a particular company in a particular year.

# PROBLEM 1.1 - EXPLORING THE DATASET. Answer: 11580 observations in the dataset
stocks <- read.csv(paste0(folder, "StocksCluster.csv"))
str(stocks)
head(stocks)
nrow(stocks)

# PROBLEM 1.2 - EXPLORING THE DATASET. Answer: 0.5461 of stocks had a positive return in December
result <- table(stocks$PositiveDec)
result
result[2] / sum(result)

# PROBLEM 1.3 - EXPLORING THE DATASET. Max correlation between any two vars: 0.192 (ReturnOct-ReturnNov)
cor(stocks)
CorrelationPlot(stocks)
result <- cor(stocks)
result[result == 1] <- 0
result[which(result == max(result))])
#rownames(result)[which(result == max(result))[1]]

# PROBLEM 1.4 - EXPLORING THE DATASET. Answer: See below.
# 1) Which month (from January through November) has the largest mean return across all observations in the dataset?
#    Answer: April
# 2) Which month (from January through November) has the smallest mean return across all observations in the dataset?
#    Answer: September
barplot(colMeans(stocks[,-ncol(stocks)]), las=2)
colMeans(stocks[,-ncol(stocks)])

# PROBLEM 2.1 - INITIAL LOGISTIC REGRESSION MODEL. Answer: GLM accuracy on train set: 0.5712
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)
StocksModel <- glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
summary(StocksModel)
predStocksTrain <- predict(StocksModel, type="response") # Important with "response" here!
result <- table(stocksTrain$PositiveDec, predStocksTrain > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 2.2 - INITIAL LOGISTIC REGRESSION MODEL. Answer: GLM accuracy on test set: 0.5671
predStocksTest <- predict(StocksModel, newdata=stocksTest, type="response") # Important with "response" here!
result <- table(stocksTest$PositiveDec, predStocksTest > 0.5)
result
accuracy <- sum(diag(result)) / sum(result)
accuracy

# PROBLEM 2.3 - INITIAL LOGISTIC REGRESSION MODEL. Answer: Baseline accuracy (PositiveDec = 1) is: 0.5461
table(stocksTest$PositiveDec)[2] / sum(table(stocksTest$PositiveDec))

# PROBLEM 3.1 - CLUSTERING STOCKS. Answer: Needing to know the dependent variable value to assign an observation to
# a cluster defeats the purpose of the methodology
# Why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL

# PROBLEM 3.2 - CLUSTERING STOCKS. Answer: See below.
# 1) What is the mean of the ReturnJan variable in normTrain? Answer: 0
# 2) What is the mean of the ReturnJan variable in normTest? Answer: 0
library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
summary(normTrain)
sapply(normTrain, mean)
summary(normTest)
sapply(normTest, mean)

# PROBLEM 3.3 - CLUSTERING STOCKS
# Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
# Answer: From mean(stocksTrain$ReturnJan) and mean(stocksTest$ReturnJan), we see that the average return in
# January is slightly higher in the training set than in the testing set. Since normTest was constructed by
# subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan
# is slightly negative in normTest.
mean(stocksTrain$ReturnJan)
mean(stocksTest$ReturnJan)
# NOTE: preProcess was done on train set, that has a slightly higher mean value than test

# PROBLEM 3.4 - CLUSTERING STOCKS. Answer: Cluster 2 has the largest number of observations
set.seed(144)
clusterStocks.km <- kmeans(normTrain, centers = 3, iter.max = 1000)
table(clusterStocks.km$cluster)

# PROBLEM 3.5 - CLUSTERING STOCKS. Answer: 2080 observations were assigned to cluster 2
# Recall from the recitation that we can use the flexclust package to obtain training set and testing set cluster
# assignments for our observations.
# How many test-set observations were assigned to Cluster 2?
library(flexclust)
clusterStocks.km.kcca <- as.kcca(clusterStocks.km, normTrain)
clusterTrain <- predict(clusterStocks.km.kcca)
clusterTest <- predict(clusterStocks.km.kcca, newdata=normTest)
table(clusterTest)

# PROBLEM 4.1 - CLUSTER-SPECIFIC PREDICTION. Answer: Cluster 1 has the highest average mean of the outcome variable
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

# PROBLEM 4.2 - CLUSTER-SPECIFIC PREDICTIONS. Answer: See below.
# Answer: Both positive and negative signs: Jan, Feb, Mar, June, Aug, Oct
stocksLog1 <- glm(PositiveDec ~., data=stocksTrain1, family=binomial)
stocksLog2 <- glm(PositiveDec ~., data=stocksTrain2, family=binomial)
stocksLog3 <- glm(PositiveDec ~., data=stocksTrain3, family=binomial)
summary(stocksLog1)
summary(stocksLog2)
summary(stocksLog3)
stocksLog1$coeff[2:length(stocksLog1$coeff)]
stocksLog2$coeff[2:length(stocksLog2$coeff)]
stocksLog3$coeff[2:length(stocksLog3$coeff)]
                                                   
# PROBLEM 4.3 - CLUSTER-SPECIFIC PREDICTIONS. Answer: Accuracy 1: 0.6194, Accuracy 2: 0.5505, Accuracy 3: 0.6458
PredictTest1 <- predict(stocksLog1, newdata=stocksTest1, type="response")
PredictTest2 <- predict(stocksLog2, newdata=stocksTest2, type="response")
PredictTest3 <- predict(stocksLog3, newdata=stocksTest3, type="response")
result1 <- table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
result1
result2 <- table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
result2
result3 <- table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
result3
accuracyTest1 <- sum(diag(result1)) / sum(result1)
accuracyTest1
accuracyTest2 <- sum(diag(result2)) / sum(result2)
accuracyTest2
accuracyTest3 <- sum(diag(result3)) / sum(result3)
accuracyTest3

# PROBLEM 4.4 - CLUSTER-SPECIFIC PREDICTIONS. Answer: Accuracy on total test set: 0.5789
# (A modest, but good, improvement over Logistic Regression with this Cluster-then-Predict approach)
AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
resultAll <- table(AllOutcomes, AllPredictions > 0.5)
resultAll
accuracyAll <- sum(diag(resultAll)) / sum(resultAll)
accuracyAll
# We see a modest improvement over the original logistic regression model. Since predicting stock returns is a notoriously
# hard problem, this is a good increase in accuracy. By investing in stocks for which we are more confident that they will
# have positive returns (by selecting the ones with higher predicted probabilities), this cluster-then-predict model can
# give us an edge over the original logistic regression model.
