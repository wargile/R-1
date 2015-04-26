# Internet Advertisements Data Set
# http://archive.ics.uci.edu/ml/datasets/Internet+Advertisements
# http://archive.ics.uci.edu/ml/machine-learning-databases/internet_ads/ad.DOCUMENTATION


set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
par(cex.lab=.7)
par(cex.main=.8)
par(cex.axis=.7)
par(mar=c(3,3,2,1))

datafolder <- "C:/coding/R/testdata/"
ads <- read.csv(paste0(datafolder, "ad.csv"), header=F)

dim(ads)
str(ads)
summary(ads)
# Fix ad./nonad. -> 1/0
names.ads <- names(ads)
names.ads[length(names.ads)] <- "Ad"
names(ads) <- names.ads

ads$Ad <- as.factor(ifelse(ads$Ad == "ad.", 1, 0))
barplot(table(ads$Ad), main="Ad (1) or not (0)")

CorrelationPlot(ads[,1:30])

result <- CreateTrainAndValidationSets(ads)
train <- result[[1]]
validation <- result[[2]]

# Try GLM (hangs on all predictors!)
model <- glm(Ad ~ ., data=train, family=binomial(link="logit"))
summary(model)

# TODO: Try a Neural Net (and figure out how they work, hidden layers,etc.!)
library(nnet)
ideal <- class.ind(validation$Ad)

# Train the model. Leave out the class attribute
# TODO: Impute NA's and normalize(?)
nnet.result = nnet(Ad ~ ., data=train, size=3, na.action=na.omit, softmax=T) # Softmax for logit?
# ERROR: Too many weights. Try h2o?
# Predict on validation set
result <- predict(nnet.result, validation)
result <- plogis(result)
result <- ifelse(result < 0.5, 0, 1)
# Calculate Classification accuracy
table(validation$Ad, predict(nnet.result, validation, type="class"))

# TODO: try h2o on this...
