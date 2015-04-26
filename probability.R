# Probabilities

DoRoulette <- function() {
  #Roulette Board
  total <- 38
  black <- 18
  red <- 18
  green <- 2
  red_and_even <- 8
  black_and_even <- 10
  
  p_black_or_red <- 1 - (green / total)
  p_black_or_green <- 1 - (red / total)
  p_red_or_green <- 1 - (black / total)
  
  p_black <- black / total
  p_red <- red / total
  p_green <- green / total
  
  p_black_and_even <- black_and_even / total 
  p_red_and_even <- red_and_even / total 
  
  p_even <- p_black_and_even + p_red_and_even
  cat("P_even:", sprintf("%.2f", p_even), "\n")
  
  p_black_or_even <- (p_black + p_even) / p_black_and_even
  p_red_or_even <- (p_red + p_even) / p_red_and_even
  
  p_black_given_even <- p_black_and_even / p_even
  p_red_given_even <- p_red_and_even / p_even
  
  cat("P_black_given_even:", sprintf("%.2f", p_black_given_even), "\n")
  
  c(p_even, p_black_given_even)
}

DoGameOfDice <- function() {
  # A game at the carnival offers these odds: you get to roll a ten-sided die,
  # and if you roll a 9, you make 6 dollars. Unfortunately, if you roll anything else, you lose 7 dollars. 
  # How much money do you expect to make (or lose) playing this game? 
  
  gain <- 6
  loss <- -7
  dice.sides <- 10
  prob.roll.a.9 <- 1/dice.sides
  prob.roll.anything.else <- (dice.sides - 1) / dice.sides
  
  gain.or.loss <- ((gain * prob.roll.a.9) + (loss * prob.roll.anything.else))
  
  if (gain.or.loss > 0)
    paste0("The gain was $", gain.or.loss)
  else
    paste0("The loss was -$", abs(gain.or.loss))
}

# The coin toss heads/tails probability should converge towards P=0.5
DoCoinToss <- function(tosses) {
  sides <- 2
  heads <- 0
  tails <- 1
  result <- numeric()  

  for (i in 1:tosses) {
    head.or.tail <- sample(c(heads, tails), 1)
  
    if (identical(head.or.tail, tails)) 
      result <- append(result, tails)
    else
      result <- append(result, heads)
  }
  
  paste0("Heads: ", length(result[result == heads]) / tosses, " Tails: ",
         length(result[result == tails]) / tosses)
}

DoPermutation <- function(n) {
  # http://www.youtube.com/watch?v=6XWqDezwbaw
  # Permutation: Order matters ("choose k of n")
  # Example: How many unique 1-2-3 finishes are there in a horse race of 10 horses?
  result <- numeric(n + 1)
  
  result <- sapply(0:n, function(x) factorial(n) / factorial(n - x))
  barplot(result / sum(result), col="cornflowerblue", main="Permutation: Probability for all combinations")
  probability <- sum(result / sum(result)) # Always 1
  invisible(return(result))
}

DoCombination <- function(n) {
  # http://www.youtube.com/watch?v=6XWqDezwbaw
  # Combination: Order does not matter ("choose k of n")
  result <- numeric(n + 1)
  
  result <- sapply(0:n, function(x) factorial(n) / (factorial(x) * factorial(n - x)))
  barplot(result / sum(result), col="cornflowerblue", main="Combination: Probability for all combinations")
  #mtext(result, at=c(1:(n+1)), padj=0, cex=.5)
  probability <- sum(result / sum(result)) # Always 1
  invisible(return(result))
}
