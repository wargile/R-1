# fifteen.R

board <- 25
data <- sample(board, board)
data
fifteen <- matrix(data, nrow=sqrt(board), ncol=sqrt(board))
fifteen

empty.col <- ceil(which.max(data) / sqrt(board))
empty.col
empty.row <- which.max(fifteen[, empty.col])
empty.row

fifteen[empty.row, empty.col]
