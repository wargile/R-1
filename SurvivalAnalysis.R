# Survival Analysis Examples
# --------------------------
# http://www.ms.uky.edu/~mai/Rsurv.pdf
# http://www.ats.ucla.edu/stat/r/examples/asa/asa_ch2_r.htm

library(survival)
head(aml)

# Survived or not (censored data)
stime <- c(2.2, 3, 8.4, 7.5)
status <- c(1,0,1,0)
Surv(stime, status)

Surv(aml$time, aml$status)

# To simulate the (randomly right) censored observations, we need to first simulate a lifetime vector
# and, independently, a termination time (=censoring time) vector. Then we observe whichever comes
# first (ztimes) and related indicator (status).
lifetimes <- rexp(25, rate=0.2)
censtimes <- 5 + 5*runif(25)
ztimes <- pmin(lifetimes, censtimes) # NOTE: Get the min value in one or the other array
status <- as.numeric(censtimes > lifetimes)
status
# The estimation goal is to recover the exponential distribution of the lifetimes from the censored
# observations ztimes and status only.

# Kaplan-Meier estimator (doesen't work...)
summary(survfit(Surv(aml$time[1:11], aml$status[1:11]) ~ 1))
fit1 <- survfit(Surv(aml$time[1:11],aml$status[1:11]) ~ 1)
plot(fit1)

# Parametric regression models
fit1 <- survreg(Surv(aml$time, aml$status) ~ aml$x, dist="exponential")
# or:
fit1 <- survreg(Surv(time, status) ~ x, data=aml, dist="exponential")
predict(fit1, type="quantile", p=c(0.1, 0.5, 0.9))
