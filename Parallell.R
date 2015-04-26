# Parallell processing in R
# https://beckmw.wordpress.com/2014/01/21/a-brief-foray-into-parallel-processing-with-r/
# http://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf

#import packages
library(foreach)
#package.install("doParallel")
library(doParallel)
library(help="doParallel") # Show doc page

x <- rnorm(100)
y <- x * rnorm(100)
new.x <- data.frame(x=(y * rnorm(100)))
n <- 10000
result <- list()

# -----------------------------------------------------------------------------------------
# Try the "normal" way:

# Start
strt <- Sys.time()

for (counter in 1:n) {
  model1 <- lm(y ~ x)
  result[[counter]] <- predict(model1, new.x)
}

# Stop
print(Sys.time() - strt)

# -----------------------------------------------------------------------------------------
# Then do parallell:

cl <- makeCluster(8)
registerDoParallel(cl)

# Start
strt <- Sys.time()

# Create parallell loop
result2 <- foreach(icount(n)) %dopar% {
  model1 <- lm(y ~ x)
  to.result2 <- predict(model1, new.x)
}

# Stop
print(Sys.time() - strt) # Nice...!

# Stop the cluster
# stopCluster(cl)

# -----------------------------------------------------------------------------------------
# Try looping through files:

driverFolder <- "C:/coding/Kaggle/DriverTelematicsAnalysis/data/drivers/drivers"
drivers = list.files(driverFolder) # Get a list of all folders (= drivers)
# Create parallell loop
library(iterators)
#it <- icount(length(drivers))
n <- length(drivers)

# Create parallell loop, note syntax
result2 <- foreach(counter=1:n) %do% {
  to.result2 <- paste0("The folder is: ", drivers[counter])
}

# Stop the cluster
stopCluster(cl)
