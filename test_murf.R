# Unit tests for MuRF

library(RUnit)

test_MuRF.GetPlotData <- function()
{
  source("murf.R")
  
  #track <- tracker()
  #track$init()
  
  result1 <- NA
  result2 <- NA
  result3 <- NA
  
  result1 <- checkEquals(max(GetPlotData()), volt_ref, tolerance=1.0e-4)
  result1 <- paste("Result test 1:", result1)
  result2 <- checkTrue(length(GetPlotData()) > 0)
  result2 <- paste("Result test 2:", result2)
  result3 <- checkEqualsNumeric(max(GetPlotData()), volt_ref+1, tolerance=1.0e-4)
  
  tryCatch({ result3 <- checkEqualsNumeric(max(GetPlotData()), volt_ref+1, tolerance=1.0e-4) },
                      warning=function(w) { result3 <- w$message },
                      error=function(e) { str(e) },
                      finally={ result3 <- paste("Result test 3:", result3)
                                result3 }
                      )
  #result3 <- paste("Result test 3:", result3)
  
  df <- data.frame(result=c(result1, result2, result3))
  df
}
