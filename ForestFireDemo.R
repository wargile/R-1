# Forest Fire Demo - recursion example

SetStandardOptions()
library(manipulate)

universe <- NULL
stack.level <- 0
options(expressions=(50000)) # Make sure nesting level for recursion is deep enough
forest.cell <- 1
fire.cell <- 2
row.n <- 0
col.n <- 0

# Recursive function that checks for adjacent "forest cells"
SpreadFire <- function(n.row, n.col) {
  stack.level <<- stack.level + 1
  universe[n.row, n.col] <<- fire.cell
  
  if (n.row > 1) {
    if (universe[n.row - 1, n.col] == forest.cell)
      SpreadFire(n.row - 1, n.col)
  }
  
  if (n.row < nrow(universe)) {
    if (universe[n.row + 1, n.col] == forest.cell)
      SpreadFire(n.row + 1, n.col)
  }
  
  if (n.col > 1) {
    if (universe[n.row, n.col - 1] == forest.cell)
      SpreadFire(n.row, n.col - 1)
  }
  
  if (n.col < ncol(universe)) {
    if (universe[n.row, n.col + 1] == forest.cell)
      SpreadFire(n.row, n.col + 1)
  }
}

ForestFireDemo <- function(n, m, p, burn.forest=F) {
  op <- par
  par(mar=c(1.2,1,2,1))
  
  if (burn.forest == F) {
    stack.level <<- 0
    data <- rbinom(n * m, 1, prob=p)
    fire <- sample(which(data == forest.cell), 1)
    universe <<- matrix(data, ncol=m, byrow=T)
    # Create parking lot (gray), forest (green) and fire (orange):
    row.n <<- floor(fire / m)
    if (row.n == 0) row.n <<- 1
    col.n <<- (fire %% m)
    if (col.n == 0) col.n <<- 1
    the.title <- paste0("Forest Fire Test. Parking lot: ", table(universe)[1],
                        " cells, Forest: ", table(universe)[2], " cells")
    universe[row.n, col.n] <<- fire.cell
    image(t(universe), col=c("gray","green4","orange"), main=the.title, xaxt="n", yaxt="n", cex.main=1)
    box(lty="solid")
    return()
  }
  
  if (burn.forest == T) {
    SpreadFire(row.n, col.n)
    image(t(universe), col=c("gray","green4","orange"),
          main=paste0("Forest Fire Test (stack.level=", stack.level, ")"),
          xaxt="n", yaxt="n", cex.main=1)
    box(lty="solid")
  }
  
  par <- op
}

manipulate(
  ForestFireDemo(n, m, p, burn.forest),
  n=slider(min=2, max=100, initial=50, step=1, label="Matrix rows"),
  m=slider(min=2, max=100, initial=50, step=1, label="Matrix columns"),
  p=slider(min=.1, max=.75, initial=.5, step=.025, label="Probability of trees"),
  burn.forest=checkbox(initial=F, label="Spread the fire!")
)
