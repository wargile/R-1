# Kaggle - Acquire Valued Shoppers Challenge
# Deadline: July 14th 2014

# The transactions file can be joined to the history file by (id,chain). The history file can be joined to the
# offers file by (offer). The transactions file can be joined to the offers file by (category, brand, company).
# A negative value in productquantity and purchaseamount indicates a return.

# http://www.kaggle.com/c/acquire-valued-shoppers-challenge/data

set.seed(16071962)
dataFolder <- "C:/coding/Kaggle/AcquireValuedShoppersChallenge/data/"
codeFolder <- "C:/coding/Kaggle/AcquireValuedShoppersChallenge/code/"
submissionsFolder <- "C:/coding/Kaggle/AcquireValuedShoppersChallenge/submissions/"

if (file.exists(paste0(dataFolder, "sample.rda")) == F) {
  sampling <- read.csv(paste0(dataFolder, "sample.csv"), header=T, sep=",", stringsAsFactors=F)
  offers <- read.csv(paste0(dataFolder, "offers.csv"), header=T, sep=",", stringsAsFactors=F)
  trainHistory <- read.csv(paste0(dataFolder, "trainHistory.csv"), header=T, sep=",", stringsAsFactors=F)
  testHistory <- read.csv(paste0(dataFolder, "testHistory.csv"), header=T, sep=",", stringsAsFactors=F)
  transactions <- read.csv(paste0(dataFolder, "trans_cat.csv"), header=F, sep=",", stringsAsFactors=F)
  names(transactions) <- c("id", "chain", "dept", "category", "company", "brand", "date", "productsize",
                           "productmeasure", "purchasequantity", "purchaseamount")
  nrow(transactions)
  table(transactions$category)
  
  transactions <- transactions[which(transactions$category %in% c(706,799,1703,1726,2119,2202,3203,3504,3509,
                                                                  4401,4517,5122,5558,5616,5619,5824,6202,7205,9115,9909)),]
  nrow(transactions)
  table(transactions$category)
  
  save(sampling, file=paste0(dataFolder, "sampling.rda"))
  save(offers, file=paste0(dataFolder, "offers.rda"))
  save(trainHistory, file=paste0(dataFolder, "trainHistory.rda"))
  save(testHistory, file=paste0(dataFolder, "testHistory.rda"))
  save(transactions, file=paste0(dataFolder, "transactions.rda"))
} else {
  load(paste0(dataFolder, "sampling.rda"))
  load(paste0(dataFolder, "offers.rda"))
  load(paste0(dataFolder, "trainHistory.rda"))
  load(paste0(dataFolder, "testHistory.rda"))
  system.time(load(paste0(dataFolder, "transactions.rda")))
}

head(sampling)
tail(sampling)

head(transactions)
dim(transactions)
tail(transactions)
names(transactions)

head(offers)
dim(offers)
head(trainHistory)
dim(trainHistory)
head(testHistory)
dim(testHistory)

unique(complete.cases(offers.history.train)) # all OK

offers.history.train <- merge(offers, trainHistory, all.Y=T, by.y="offer")
offers.history.train$repeater.int <- ifelse(offers.history.train$repeater == "f", 0, 1)

# NOTE: Running out of memory on both all.X=T and all.Y=T for transactions, so trying sampling
# TODO: Look at merge correctness! Seems chain etc. does not match, and creates two cols
offers.history.transactions.train <- merge(offers.history.train, sampling, all.X=T, by=c("category", "brand", "company"))
head(offers.history.transactions.train)

head(offers.history.train)
fit <- glm(repeater.int ~ brand + offer + offervalue,
           data=offers.history.train, family=binomial(link="logit"))
summary(fit)
# result <- predict(fit, newdata=testHistory, response="prob")
