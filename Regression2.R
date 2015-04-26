# Testing Logistic Regression
# http://www.r-bloggers.com/generalized-linear-models-for-predicting-rates/
# http://digitheadslabnotebook.blogspot.no/2012/07/linear-regression-by-gradient-descent.html

d <- data.frame(
  y= c(       1,    1, 1, 0,    0,    0),
  x1=c(-1000000,20000,-1, 2,10000,10000),
  x2=c( 1000000,30000, 0, 1, 1000, 1000))
d

lm <- lm(y ~ x1 + x2, data=d)
predict(lm)
# Notice how the predictions don't have a cut-point
# separating "y" (items 1,2,3) from "n" (items 4,5,6).

lr <- glm(y ~ x1 + x2, data=d, family=binomial(link='logit'))
predict(lr,type='response')
# Notice how the predictions now have a cut-point separating "y=0" from "y=1".

round(predict(lr,type='response'))


install.packages('betareg')
library(betareg)
data("GasolineYield", package="betareg")
set.seed(52352)
GasolineYield$rgroup <- sample(1:100, size=dim(GasolineYield)[[1]],replace=T)
GTrain <- subset(GasolineYield, GasolineYield$rgroup<=50)
GTest <- subset(GasolineYield, GasolineYield$rgroup>50)
gy <- betareg(yield ~ gravity + pressure + temp | gravity + pressure + temp, data=GTrain)
print(summary(gy))
GTest$model <- predict(gy,newdata=GTest)
library(ggplot2)
ggplot(data=GTest,aes(x=model,y=yield)) + 
  geom_point() + geom_abline(slope=1)

