# Caterpillar Tube Pricing
# https://www.kaggle.com/c/caterpillar-tube-pricing
# Deadline: 31 aug 2015

# The evaluation metric for this competition is RMSLE (https://www.kaggle.com/c/caterpillar-tube-pricing/details/evaluation)

# TIPS/CODE:

# TODO:

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
library(Metrics)
library(SnowballC)
library(tm)
library(png)
library(sqldf)
library(dplyr)
# etc.

SetStandardOptions()

# -------------------------------------------------------------------------------------------------------------------------------

trainfolder <- "C:/coding/Kaggle/CaterpillarTubePricing/Data/data/competition_data/"
submissionfolder <- "C:/coding/Kaggle/CaterpillarTubePricing/submissions/"

train <- read.csv(paste0(trainfolder, "train_set.csv"), header=T, sep=",", stringsAsFactors=F)
tube <- read.csv(paste0(trainfolder, "tube.csv"), header=T, sep=",", stringsAsFactors=F)
head(tube)

# Join with sqldf:
train_tube <- sqldf("SELECT * FROM train INNER JOIN tube USING(tube_assembly_id)")
head(train_tube, n=5)
# Or use dplyr:
train_tube2 <- train %>% left_join(tube, by="tube_assembly_id")
head(train_tube2, n=5)
# Dplyr with more operations:
train_tube3 <- train %>%
  left_join(tube, by="tube_assembly_id") %>%
  select(tube_assembly_id, quote_date, supplier, annual_usage, cost) %>%
  filter(cost > 53) %>%
  mutate(quote_date2=as.Date(quote_date, format='%Y-%m-%d')) %>%
  group_by(supplier) %>%
  summarise(avg_cost=mean(cost, na.rm=T)) %>% # Average cost by 'supplier' group
  arrange(desc(avg_cost))
head(train_tube3, n=15)
nrow(train_tube3)
# Something similar with sqldf (but can not do alias etc.:
train_tube4 <- sqldf(paste0("SELECT tube_assembly_id, quote_date, supplier, annual_usage, cost ", 
                            "FROM train INNER JOIN tube USING(tube_assembly_id) ORDER BY cost DESC"))
head(train_tube4, n=15)

plot(sort(with(train_tube, tapply(cost, tube_assembly_id, mean))))
plot(sort(with(train_tube, tapply(cost, supplier, sum))))
plot(sort(with(train_tube, tapply(quantity, supplier, sum))))
boxplot(train_tube$quantity ~ train_tube$supplier, las=2, col="orange", main="Quantity by Supplier")
boxplot(train_tube$min_order_quantity ~ train_tube$supplier, las=2, col="orange", main="MinOrderQuantity by Supplier")
boxplot(train_tube$annual_usage ~ train_tube$supplier, las=2, col="orange", main="AnnualUsage by Supplier")
with(train_tube, tapply(cost, supplier, sum))

# --------------------------------------------------------------------------------------------------------------------------------
# Create the submission file
# options("scipen"=100, "digits"=8)
MySubmission <- data.frame(id=test$id, prediction=test$similarity) # TODO...
head(MySubmission)
KaggleSubmission(MySubmission, submissionfolder, "TODO")
