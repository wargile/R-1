# Unit tests for Probability

library(RUnit)

test_probability.DoRoulette <- function()
{
  source("probability.R")
  
  scenario <- "Call function DoRoulette. Return values should be 9/19 and 5/9."
  #track <- tracker()
  #track$init()
  
  result1 <- NA

  ret.value <- DoRoulette()
  
  result1 <- checkEquals(ret.value, c(9/19, 5/9), tolerance=1.0e-10)
  result1 <- paste(scenario, result1)
  
  df <- data.frame(result=c(result1))
  df
}
