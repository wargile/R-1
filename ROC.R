# Create ROC curves
# -----------------
# http://people.inf.elte.hu/kiss/13dwhdm/roc.pdf
# http://www.r-bloggers.com/roc-curves-and-classification/

# Four outcomes:
# 1) If the instance is positive and it is classiﬁed as positive, it is counted as a true positive (tp).
# 2) If the instance is positive and it is classiﬁed as negative, it is counted as a false negative (fn).
# 3) If the instance is negative and it is classiﬁed as negative, it is counted as a true negative (tn).
# 4) If the instance is negative and it is classiﬁed as positive, it is counted as a false positive (fp).

# Nice ROCR plot:
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/discussion/forum/c58474c7d8924d06aef7fa89c32d9697/threads/550ad6af2a472d30690024b6

set.seed(16071962)

# Set some standard graphical params for plot
oldpar <- par()
SetStandardOptions()

n <- 150
noise <- 2.1
y <- sample(c(0,1), n, replace=T)
x <- ((y + 1) * 10) + (rnorm(n) * noise)
new.data <- data.frame(x=((y + 1) * 10) + (rnorm(n) * noise)) 

fit <- glm(y ~ x, family=binomial(link="logit"))
summary(fit)

predict.result <- predict(fit, newdata=new.data, type="response")
# NOTE: ROC measures the performance of the classifier, therefore use y in training data for ROC
#predict.result <- predict(fit, type="response")
plot(predict.result, pch=21, bg="cyan", col="blue", main="Prediction Result")

hypothesized.class <- round(predict.result) # Assume threshold is 0.5
true.class <- y
table(true.class)

# http://en.wikipedia.org/wiki/Precision_and_recall
# Confusion matrix: Verification on x-axis, Prediction on y-axis
# confusion.matrix <- table(hypothesized.class, true.class)
confusion.matrix <- table(true.class, hypothesized.class)
confusion.matrix
plot(confusion.matrix, col=2:3, main="Confusion Matrix", cex.axis=.8)

tp <- confusion.matrix[2, 2]
tp
fp <- confusion.matrix[1, 2]
fp
tn <- confusion.matrix[1, 1]
tn
fn <- confusion.matrix[2, 1]
fn
positives <- sum(confusion.matrix[c(2,4)])
negatives <- sum(confusion.matrix[c(1,3)])

fp.rate <- fp / negatives
tp.rate <- tp / positives
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
sensitivity <- recall
specificity <- tn / (fp + tn) # = 1 - fp.rate
positive.predictive.value <- precision
accuracy <- (tp + tn) / (positives + negatives)
# F-measure is the Harmonic mean of Recall and Precision:
f.measure <- 2 / ((1 / precision) + (1 / recall)) # Same as f1.score
f1.score <- 2 * ((precision * recall) / (precision + recall)) # Same as f.measure
# G-measure is the Geometric Mean of Recall and Precision:
g.measure <- sqrt(precision * recall)
# Matthews correlation coefficient:
mcc <- (tp * tn - fp * fn) / sqrt(as.numeric(tp + fp) * as.numeric(tp + fn) * as.numeric(tn + fp) * as.numeric(tn + fn))
# NOTE: as.numeric used above, otherwise we get integer overflow when product exceeds max integer value

S <- predict.result
Y <- y # NOTE: ROC measures the performance of the classifier, therefore use y in training data for ROC

# FROM: http://www.r-bloggers.com/roc-curves-and-classification/
# (http://freakonometrics.hypotheses.org/9066)
roc.curve <- function(s, print=FALSE) {
  Ps <- (S > s) * 1 # NOTE: Neat way of converting TRUE/FALSE to 1/0
  FP <- sum((Ps == 1) * (Y == 0)) / sum(Y == 0)
  TP <- sum((Ps == 1) * (Y == 1)) / sum(Y == 1)

  if (print == TRUE) {
    print(table(Observed=Y, Predicted=Ps))
  }
  
  vect <- c(FP, TP)
  names(vect) <- c("FPR","TPR")
  return(vect)
}

threshold <- 0.5
roc.curve(threshold, print=TRUE)
ROC.curve <- Vectorize(roc.curve)

par(mfrow=c(1,2))
I <- (((S > threshold) & (Y == 0)) | ((S <= threshold) & (Y == 1)))
plot(S, Y, col=c("red", "blue")[I + 1], pch=19, cex=.8, cex.axis=.8, xlab="", ylab="")
abline(v=threshold, col="gray", lwd=2)

M.ROC <- ROC.curve(seq(0, 1, by=.01))
plot(M.ROC[1,], M.ROC[2,], col="blue", lwd=2, type="l", main="ROC Curve",
     xlab="False positive rate", ylab="True positive rate", cex=.8, cex.axis=.8)
par(mfrow=c(1,1))

library(tree)
ctr <- tree(y ~ x)
plot(ctr)
text(ctr)

S2 <- predict(ctr)

# TODO: Plot separate ROC curve for S2, or overlay ROC plot 1 with lines(...)

# Calculate area under ROC curve formula
# --------------------------------------
# http://r.789695.n4.nabble.com/How-to-calculate-the-area-under-the-curve-td902633.html
# https://www.kaggle.com/wiki/AreaUnderCurve
# https://www.kaggle.com/c/SemiSupervisedFeatureLearning/forums/t/919/auc-implementation/6136#post6136
# http://www.gpa.etsmtl.ca/cours/sys828/REFS/A1/Fawcett_PRL2006.pdf
# http://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve

# Using a dataset from edX:
folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 3/Assignment/"
quality <- read.csv(paste0(folder, "quality.csv"), header=T)
str(quality)
# True positive rate (TPR, sensitivity) on the y-axis: Proportion of PoorCare (1) caught
# False positive rate (FPR, 1-specificity) on the x-axis: Proportion of GoodCare labeled as PoorCare.
# ROC Plot: Lower left corner (0,0): Threshold value of 1. Catches NO PoorCare cases. False Positive Rate = 0.
# ROC Plot: Upper right corner (1,1): Threshold value of 0. Catches ALL PoorCare cases (sensitivity=1). BUT it labels all
# of the GoodCare cases as PoorCare cases too! Meaning you have a False Positive Rate of 1
# The threshold DECREASES as you move from (0,0) to (1,1)

# The ROC curve captures all thresholds simultaneously: High threshold (close to 1): High specificity, low sensitivity.
# Low threshold (close to 0): Low specificity, high sensitivity.

# Choose best threshold for best trade-off: 
# - Cost of failing to detect positives
# - Costs of raising false alarms
