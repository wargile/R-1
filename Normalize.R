# Normalize function. Yay!
# Normalize Function. Yay!

minnorm <- -1
maxnorm <- 1
minval <- 0
maxval <- 120

curval <- 119

# Find the normalized (0-1 range) value of curval
normalized <- (curval - minval) / (maxval - minval) # This gives normalized as 'normalized2' var below
normalized
# Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
normval <- minnorm + ((maxnorm - minnorm) * normalized)
normval

# Normalize from any range to 0-1, where min(range)=0 and max(range)=1:
x = sample(-120:100, 20)
x
normalized2 = (x - min(x)) / (max(x) - min(x))
normalized2

Normalize <- function(minval, maxval, minnorm, maxnorm, curval) {
  # Find the normalized (0-1 range) value of curval:
  normalized <- (curval - minval) / (maxval - minval) # This gives the normalized value between 0 and 1
  # Now let's get the new normalized value adjusted for our minnorm and maxnorm params:
  normval <- minnorm + ((maxnorm - minnorm) * normalized)
  return (normval)
}

# Test it...
data <- rnorm(10) * 50
data
round(Normalize(-120, 120, 0, 1, data), 2)
