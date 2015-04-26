# ------------------------------------------------------------------------------------------------------
# Cycle handling

n <- 32
stage.status <- as.integer(rep(0, n))
stage.status
stage.begin <- 1
stage.end <- -1
old.stage.n.end <- 0
old.stage.n.start <- 0

FindStageStartAndEnd <- function(stage) {
  stages.start.end <- c(1,2,3,4,5,6,7,8,9,10) # Start/end pairs
  stages.start.end[(stages.start.end %% 2) == 1]
  stages.start.end[(stages.start.end %% 2) == 0]
}

UpdateCycles <- function(stage.n.start, stage.n.end) {
  stage.status.display <- as.integer(rep(0, n))

  if ((stage.n.start > 0) & (stage.n.start != old.stage.n.start)) {
    stage.status[stage.n.start] <<- stage.begin
    if (stage.n.start > 1) {
      for (counter in seq((stage.n.start - 1), 1, -1)) {
        if (stage.status[counter] == stage.end)
          break
        if (stage.status[counter] == stage.begin)
          stage.status[counter] <- 0
      }
    }
    if (stage.n.start < n) {
      for (counter in seq((stage.n.start + 1), n, 1)) {
        if (stage.status[counter] == stage.end)
          break
        if (stage.status[counter] == stage.begin)
          stage.status[counter] <- 0
      }
    }
  }

  if ((stage.n.end > 0) & (stage.n.end != old.stage.n.end)) {
    stage.status[stage.n.end] <<- stage.end
    if (stage.n.end < n) {
      for (counter in seq((stage.n.end + 1), n, 1)) {
        if (stage.status[counter] == stage.begin)
          break
        if (stage.status[counter] == stage.end)
          stage.status[counter] <- 0
      }
    }
    if (stage.n.end > 1) {
      for (counter in seq((stage.n.end - 1), 1, -1)) {
        if (stage.status[counter] == stage.begin)
          break
        if (stage.status[counter] == stage.end)
          stage.status[counter] <- 0
      }
    }
  }
  
  flag <- 0
  for (counter in 1:n) {
    if (stage.status[counter] == stage.begin)
      flag <- 1
    if (stage.status[counter] == stage.end)
      flag <- 0
    stage.status.display[counter] <- flag
  }
  
  old.stage.n.start <<- stage.n.start
  old.stage.n.end <<- stage.n.end
  
  stage.status.str <- "Stage status:"
  sapply(stage.status, function(x) stage.status.str <<- paste(stage.status.str, x))
  op <- par()
  par(mfrow=c(2,1))
  par(mar=c(3,2,2,1))
  plot(stage.status.display, type="l", col="blue", main=stage.status.str, xlab="", ylab="",
       cex.main=1, cex.lab=.8, cex.axis=.8, ylim=c(-.2,1.2), xaxt="n", yaxt="n")
  axis(side=1, at=which(stage.status != 0), labels=which(stage.status != 0))
  image(as.matrix(stage.status.display), xaxt="n", yaxt="n")
  par(mfrow=c(1,1), cex.main=1, cex.lab=.8, cex.axis=.8)
  par <- op
  return(stage.status.display)
}

stage.n.start.d <- 0
stage.n.end.d <- 0
stages <- as.list(seq(1,n))

# 1)
manipulate(
  UpdateCycles(stage.n.start.d, stage.n.end.d),
  stage.n.start.d=picker(stages, initial=1, label="Select start stage number"),
  stage.n.end.d=picker(stages, initial=5, label="Select start stage end")
)
