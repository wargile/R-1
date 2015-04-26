# Buchla 264/265 stage logic
# --------------------------

set.seed(19620716)
SetStandardOptions()

Strobe <- function(stage.no) {
  stages[(stage.no * 2)] <<- !stages[(stage.no * 2)]
  plot(seq(1, stages.no, by=2), rep(0, ceiling(stages.no / 2)), pch=21, type="o", bg="lightgray", col="gray",
       main="Buchla Sequential Voltage Source 245", yaxt="n", xaxt="n",
       cex.main=.8, cex.lab=.7, cex.axis=.7, xlab="Stages", ylab="Status", cex=2)
  labels <- unlist(lapply(1:(stage.no), function(x) c(x, "Sep")))
  labels <- c(labels, stage.no + 1)
  axis(side=1, at=1:((stage.no * 2)+1), labels=labels)
  abline(v=seq(2, stages.no, by=2), col="lightgray", lty=1, lwd=1)
  abline(v=which(stages == 1), col="green4", lty=1, lwd=2)
}

GetActiveStageRange <- function(stage.no) {
  cur.stage <- ((stage.no - 1) * 2) + 1
  first.stage <- cur.stage
  last.stage <- cur.stage
  
  while (1) {
    if ((stages[last.stage] == 1 & (last.stage < stages.no)) | (last.stage == stages.no + 1)) {
      last.stage = last.stage - 1
      break
    } else last.stage <- last.stage + 1
  }
  
  while (1) {
    if (first.stage == 0) {
      first.stage <- first.stage + 1
      break
    }
    if (stages[first.stage] == 1) {
      first.stage <- first.stage + 1
      break
    }
    first.stage <- first.stage - 1
  }

  active.stages <- (first.stage:last.stage %% 2)
  active.stages <- unique(ceiling(first.stage:last.stage / 2)) # TODO: Simplify
  points((active.stages * 2) - 1, rep(0, length(active.stages)), col="blue", pch=21, bg="cyan", cex=2)
  points((stage.no * 2) - 1, 0, col="red", pch=21, bg="yellow", cex=2)
  return(c(stage.no, "->", first.stage, "-", last.stage, ":", active.stages))
}

stages.no <- 9 # (Stages * 2) - 1

stages <- 0:(stages.no-1) %% 2
stages
first.stage <- 0
last.stage <- 0

Strobe(sample(((stages.no / 2) - 1), 1))
GetActiveStageRange(sample((stages.no / 2), 1))

# Create an animated image
# http://www.r-bloggers.com/going-viral-with-rs-igraph-package/
package.install("animation")
library(animation)

ani.options(interval=1)
saveGIF({
  count <- 0
  for(i in 1:3) {
    # TODO: Need to have plot directly here
    plot(((-10 - count):(10 + count))^2, col="blue", pch=19)
    #Strobe(sample(4, 1))
    #GetActiveStageRange(sample(5, 1))
    count <- count + 1
  }
}, interval = 1, movie.name = "demo.gif", ani.width = 1000, ani.height = 1000)
