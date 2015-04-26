# Markov Chain example - Stockmarket, Snakes and Ladders, etc.
# ------------------------------------------------------------
# http://datagenetics.com/blog/november12011/
# http://www.r-bloggers.com/basics-on-markov-chain-for-parents/
# http://www.r-bloggers.com/uncertainty-in-markov-chains-fun-with-snakes-and-ladders/
# http://www.natalinobusa.com/2013/01/markov-chains-for-ladders-and-snakes.html
# http://en.wikipedia.org/wiki/Snakes_and_Ladders
# http://en.wikipedia.org/wiki/Markov_chain
# http://thexbar.me/2014/10/24/raddecay/

board <- matrix(1:100, ncol=10, byrow=T)
board
dice.eyes <- sample(6, 1, replace=F)

# http://en.wikipedia.org/wiki/Markov_chain, stock market example
A <- matrix(c(0.9, 0.075, 0.025, 0.15, 0.8, 0.05, 0.25, 0.25, 0.5), ncol=3, byrow=T)
A
c(0,1,0) %*% A^3
curve1 <- numeric()
curve2 <- numeric()
curve3 <- numeric()

for (counter in 1:100) {
  matlist <- lapply(seq(1:counter), function(x) return(A))
  a <- Reduce("%*%", matlist)
  curve1 <- c(curve1, (c(0,1,0) %*% a)[1])
  matlist <- lapply(seq(1:counter), function(x) return(A))
  a <- Reduce("%*%", matlist)
  curve2 <- c(curve2, (c(0,1,0) %*% a)[2])
  matlist <- lapply(seq(1:counter), function(x) return(A))
  a <- Reduce("%*%", matlist)
  curve3 <- c(curve3, (c(0,1,0) %*% a)[3])
}


plot(curve1, type="l", col="blue", ylim=c(min(curve3), max(curve2)), main="Stock market")
lines(curve2, col="red")
lines(curve3, col="green4")

# -------------------------------------------------------------------------------------------------------------
# Snakes and ladders game
finish <- 100
# prob <- 1/6
scores <- integer(0)
ladder.from <- c(1,4,9,21,28,36,51,71,80)
ladder.to <- c(38,14,31,42,84,44,67,91,100)
snake.from <- c(98,95,93,87,64,62,56,49,48,16)
snake.to <- c(78,75,73,24,60,19,53,11,26,6)
n <- 100000
n <- 250
dice.sides <- 6
positions.list <- list()

for (counter in 1:n) {
  if (counter %% 1000 == 0)
    print(paste0(counter, " simulations done..."))
  
  pos <- 1
  roll.no <- 0
  positions <- integer(0)
  
  while (T) {
    roll.no <- roll.no + 1
    roll <- sample(dice.sides, 1) # 1/dice.sides prob.
    if ((pos + roll) %in% ladder.from) {
      pos <- ladder.to[which(ladder.from == (pos + roll))]
    } else if ((pos + roll) %in% snake.from) {
      pos <- snake.to[which(snake.from == (pos + roll))]
    } else {
      pos <- pos + roll
    }

    positions <- c(positions, pos)
    
    if (pos >= finish) {
      scores <- c(scores, roll.no)
      positions.list[[counter]] <- positions
      break
    }
  }
}

n.breaks <- 35
colors <- SetColors(gradient=T, range=n.breaks, palette.no=1, setAsPalette=F)

hist(scores, col=colors, main=paste("Snakes and Ladders - Dice throws (mean = ", round(MyMean(scores), 0),
                                      "mode = ", MyMode(scores), ")"),
     cex.lab=.8, cex.axis=.8, cex.main=.9, breaks=n.breaks, xlab="Dice throws")

op <- par()
par(mar=c(2.5,2.5,2,.8))
par(mfrow=c(2,2))
picks <- sample(n, 4, replace=F)
max.positions <- integer(0)
for (counter in picks)
  max.positions <- c(max.positions, length(positions.list[[counter]]))
for (counter in picks)
  plot(positions.list[[counter]], type="o", pch=21, bg="aquamarine", col="blueviolet",
       main=paste("Snakes and Ladders -", length(positions.list[[counter]]), "dice throws"),
       cex.lab=.7, cex.axis=.8, cex.main=1, xlab="", ylab="",
       xlim=c(0, max(max.positions)), ylim=c(0, 106))
par <- op

# TODO: The transition matrix
# http://www.r-bloggers.com/basics-on-markov-chain-for-parents-2/
n <- 100
M <- matrix(0, n + 1, n + 1 + 6)
# from n+1 starting positions (0-100 inclusive) to n+1+6 ending (includes overshooting the last position)
rownames(M) <- 0:n
colnames(M) <- 0:(n + 6)

for(i in 1:6) {
  diag(M[, (i + 1):(i + 1 + n)]) <- 1/6
}

M[, n + 1] <- apply(M[, (n + 1):(n + 1 + 6)], 1, sum)
M <- M[, 1:(n + 1)]

initial <- c(1, rep(0, n))
initial <- initial %*% M
