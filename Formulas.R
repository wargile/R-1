# Here is R code to show all details of calculations.  It takes a sample (size N = 100)
# from a normal distribution function.  Then it does all calculations described above.
# Finally, it shows the histogram of the sample and marks the true population mean (blue line),
# the sample mean (green line) and the upper and lower 95% CI limits (red lines).

# Take random samples from a standard normal distribution (mean = 0.0 and sd = 1) and calculate 
# xmean = sample mean, 
# xsd = sample standard deviation,
# xse = standard error of the sample mean
# ci_mean_upper = upper limit for 95% conf interval of sample mean
# ci_mean_lower = lower limit for 95% conf interval of sample mean

# N is the sample size which is 100

xmean <- as.numeric(NA)
xsd <- as.numeric(NA)
xse <- as.numeric(NA)
ci_mean_upper <- as.numeric(NA)
ci_mean_lower <- as.numeric(NA)

N = 100

x <- rnorm(N, mean = 0.0, sd = 1.0)
xmean <- mean(x)
xsd <- sd(x)
xse <- sd(x) / sqrt(N)
length <- abs(qt(p = 0.025, df = N-1)) * xse

# TODO START: Example: How 0.025 is found for 95% CI
I.want.ci.level <- 95
ci.1 <- (100 - I.want.ci.level) / 2.0
ci.2 <- 100 - ci.1
ci.1.percent <- ci.1 / 100.0
# TODO END: Example: How 0.025 is found for 95% CI

ci_mean_upper <- xmean + length
ci_mean_lower <- xmean - length

print(sprintf("Sample size N = %d", N))
print(sprintf("Sample mean = %f", xmean))
print(sprintf("Sample standard deviation = %f", xsd))
print(sprintf("Sample standard error = %f", xse))
print(sprintf("Upper limit for the 95perc CI for mean = %f", ci_mean_upper))
print(sprintf("Lower limit for the 95perc CI for mean = %f", ci_mean_lower))

hist(x, main = sprintf("A sample of X values (sample size = %d)", N), col="powderblue")
abline(v = 0.0, col = "blue", lwd = 3)  # true population mean
abline(v = xmean, col = "green", lwd = 3)   # sample mean
abline(v = ci_mean_upper, col = "red", lwd = 3) # 95perc CI upper bound
abline(v = ci_mean_lower, col = "red", lwd = 3) # 95perc CI lower bound
