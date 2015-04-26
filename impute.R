# My take on impute...
# --------------------

do.compare <- function(data, rownum, colnum) {
  new.diff <- Inf # set to a high enough max value
  new.row <- 0
  
  for (counter in 1:nrow(data)) {
    if (counter != rownum) {
      total.diff <- 0
      for (colcounter in 1:ncol(data)) {
        if (class(data[, colcounter]) %in% c("numeric","integer")) {
          diff <- (abs(data[counter, colcounter] - data[rownum, colcounter]))
          total.diff <- total.diff + diff
        }
      }
      if (total.diff < new.diff) {
        new.diff <- total.diff    
        new.row <- counter
        #print(new.diff)
      }
    }
  }
  
  return(data[new.row, colnum])
}

impute <- function(data, colnum, na.value=NA) {
  for (counter in 1:nrow(data)) {
    if (data[counter, colnum] %in% na.value) { # or do specific na.value as inparam
      result <- do.compare(data, counter, colnum)
      data[counter, colnum] <- result
    }
  }
  
  return(data)
}

data <- data.frame(x=1:6,
                   y=c(3.44, -13.33, 12.44, 34.12, -2, 9.44),
                   z=c("en","to",NA,"fire",NA,"seks"))
data

result <- impute(data, 3, NA)
result
