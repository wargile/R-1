# Color gradient function
# -----------------------

# See also: http://colorbrewer2.org/

rgb.values <- c("8856a7", "9ebcda", "e0ecf4")
rgb.values <- c("e7e1ef", "c994c7", "dd1c77")
rgb.values <- c("e5f5e0", "a1d99b", "31a354")
rgb.values <- c("edf8b1", "7fcdbb", "2c7fb8")
rgb.values <- c("fee6ce", "fdae6b", "e6550d")
rgb.values <- c("7fc97f", "beaed4", "fdc086")
rgb.values <- c("e41a1c", "377eb8", "4daf4a")
rgb.values <- c("8dd3c7", "ffffb3", "bebada")
rgb.values <- c("ff0000", "c0c0c0", "0000ff")
rgb.values <- c("ffffc0", "c2fec2", "000000")
rgb.values <- c("ffffc0", "c0ffc0", "000000")
rgb.values <- c("ffbbc0", "20ff20", "ffbbc0")
rgb.values <- c("000088", "20ffb0", "000088")
rgb.values <- c("b3e2cd", "fdcdac", "cbd5e8")
rgb.values <- c("cbd5e8", "fdcdac", "b3e2cd") # NOTE: Above reversed
rgb.values <- c("e41a1c", "377eb8", "4daf4a")

elements1 <- list(paste0("0x", substr(rgb.values[1], 1, 2)),
                  paste0("0x", substr(rgb.values[1], 3, 4)),
                  paste0("0x", substr(rgb.values[1], 5, 6)))
elements2 <- list(paste0("0x", substr(rgb.values[2], 1, 2)),
                  paste0("0x", substr(rgb.values[2], 3, 4)),
                  paste0("0x", substr(rgb.values[2], 5, 6)))
elements3 <- list(paste0("0x", substr(rgb.values[3], 1, 2)),
                  paste0("0x", substr(rgb.values[3], 3, 4)),
                  paste0("0x", substr(rgb.values[3], 5, 6)))

elements1 <- strtoi(c(elements1[[1]], elements1[[2]], elements1[[3]]))
elements2 <- strtoi(c(elements2[[1]], elements2[[2]], elements2[[3]]))
elements3 <- strtoi(c(elements3[[1]], elements3[[2]], elements3[[3]]))

# TODO: Handle gradient from color a to b, and not just a to b to c
rgb.elements <- length(rgb.values) - 1
n <- 23
if (n %% 2 > 0) n <- n - 1
colors <- character(0)

for (counter in 1:rgb.elements) {
  # NOTE: Since we have three colors, the seq needs to go like this:
  # 1) start.col to (middle.col - interval), i = 0
  # 2) middle.col to end.col, i = 2
  if (counter == 1) {
    startelement <- elements3
    endelement <- elements2
    i <- 0
  } else {
    startelement <- elements2
    endelement <- elements1
    i <- 2
  }
  
  startval1 <- startelement[1]
  endval1 <- endelement[1]
  
  if (endval1 == startval1)
    vals1 <- rep(endval1, (n / 2))
  else
    vals1 <- round(seq(startval1, endval1, by=((endval1 - startval1) / ((n - i) / 2))))
  
  startval2 <- startelement[2]
  endval2 <- endelement[2]
  
  if (endval2 == startval2)
    vals2 <- rep(endval2, (n / 2))
  else
    vals2 <- round(seq(startval2, endval2, by=((endval2 - startval2) / ((n - i) / 2))))
  
  startval3 <- startelement[3]
  endval3 <- endelement[3]
  
  if (endval3 == startval3)
    vals3 <- rep(endval3, (n / 2))
  else
    vals3 <- round(seq(startval3, endval3, by=((endval3 - startval3) / ((n - i) / 2))))
  
  for (counter2 in 1:(n/2)) {
    f1 <- ifelse(vals1[counter2] < 16, "0%x", "%x")
    f2 <- ifelse(vals2[counter2] < 16, "0%x", "%x")
    f3 <- ifelse(vals3[counter2] < 16, "0%x", "%x")
    
    colors <- c(colors, sprintf(paste0("#", f1, f2, f3),
                                vals1[counter2], vals2[counter2], vals3[counter2]))
  }
}

colors

par(mar=c(4,3,2,.5))
x <- barplot(1:n, col=colors, main="Color test", cex.main=1, cex.axis=.7, cex.lab=.7, yaxt="n", xaxt="n")
# NOTE: x contains the positions of the bars!
axis(side=1, at=x, labels=colors, las=2, cex.axis=.7, tick=F)
axis(side=2, at=seq(0, length(x), 2), las=2, cex.axis=.7, tick=T)
