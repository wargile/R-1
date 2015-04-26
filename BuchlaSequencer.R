# BuchlaSequencer functions
# -------------------------

# Idea: Euclidean sequencer??
# http://createdigitalmusic.com/2011/03/circles-and-euclidian-rhythms-off-the-grid-a-few-music-makers-that-go-round-and-round/
# http://www.hisschemoller.com/2011/euclidean-rhythms/
# http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf
# See also: Bjorklund's algorithm (see Python code (not mine) bjorklund.py)
# http://blog.fader.co.uk/post/11519018856/bjorklund-algorithm-and-euclidean-rhythms
# http://brianhouse.net/files/bjorklund.txt

# CV in for sequncer start step
result <- numeric(0)

Euclid <- function(m, k) {
  temp <- c(rep(1, m), rep(0, k))
  result <<- c(result, temp)
  if (k == 0) return
  else Euclid(k, (m %% k))
}

op <- par()
par(mfrow=c(2,1))
par(mar=c(4.5,4,1.8,.8))
result <- numeric(0)
m <- 20
k <- 8
#Euclid(8,3)
Euclid(sample(m, 1), sample(k, 1))
plot(result, col="blue", main="Euclidean Sequencer 1", pch=21, bg="gray", ylim=c(-.2, 1.2), cex.lab=.7,
     cex.main=1, cex.axis=.8)
result <- numeric(0)
#Euclid(8,5)
Euclid(sample(m, 1), sample(k, 1))
plot(result, col="red", main="Euclidean Sequencer 2", pch=21, bg="gray", ylim=c(-.2, 1.2), cex.lab=.7,
     cex.main=1, cex.axis=.8)
par <- op

GetTimeRange <- function() {
  # (marf_man_12:) Each stage has an initial period of 2 to 30 seconds as established by the slide pots <3.1.2>.
  # Each interval time can be divided by: 
  #   10:   A .2 to 3 second command
  #   10^2: A .02 to .3 second command
  #   10^3: A .002 to .03 second command
  timerange <- list()
  org.timerange <- c(2, 30)
  timerange[[1]] <- org.timerange
  timerange[[2]] <- org.timerange / 10   # 0.2 3
  timerange[[3]] <- org.timerange / 10^2 # 0.02 0.3
  timerange[[4]] <- org.timerange / 10^3 # 0.002 0.03
  
  return (timerange)
}

GetStartStep <- function(ref_voltage, max_step) {
  resolution <- .0025
  
  # Do x-axis labels horizontal if max_step < N, otherwise vertical
  direction <- ifelse(max_step <= 10, 1, 2)

  plot(ceiling(seq(0, ref_voltage, resolution) * (max_step / ref_voltage)), type = "l", col="blue", ylab="Steps",
       xlab="CV in", cex.lab=.8, cex.main=1, cex.axis=.7, lwd=2, ljoin=1, lmitre=2, main="CV start step handling", xaxt="n")

  lines((seq(0, ref_voltage, resolution) * (max_step / ref_voltage)), type = "l", lwd=1, lty=2, col="cyan")
  
  axis(side=1, at=seq(0, round(ref_voltage / resolution), length.out=(max_step + 1)),
       cex.axis=.7, las=direction, labels=round(seq(0, ref_voltage, length.out=max_step + 1), 1))
}

GetQuantizedVoltValue <- function(cont.volt.value, min.voltage=0, max.voltage=10, notes=12) {
  volt.per.step <- (max.voltage - min.voltage) / notes
  quantized <- (cont.volt.value - (cont.volt.value %% volt.per.step)) + volt.per.step
  if (quantized > max.voltage)
    quantized <- max.voltage
  if (quantized < min.voltage)
    quantized <- min.voltage
  
  return (quantized)
}

DoADC <- function(ADC_result, max_step) {
  REF_VOLTAGE <- 5.0
  DAC_10_BIT <- 1023
  
  f_result = REF_VOLTAGE * (ADC_result / DAC_10_BIT)
  #old_result = result
  
  portb_pin = ceiling(f_result * (max_step / REF_VOLTAGE))
  
  # Plot the result:
  # plot(ceil(seq(0, REF_VOLTAGE, .005) * (MAX_STEP / REF_VOLTAGE)), type = "l", col="blue")
  
  #if (portb_pin < 1) {
  #  portb_pin <- 1
  #}
  
  #return (bitwShiftL(1, portb_pin)) # Set the correct pin LED  
  return (portb_pin) # Set the correct pin LED  
}

CreateSlope <- function(stages=16, steady.time=F) {
  if (stages > 50 | stages < 1)
    return()
  
  op <- par()
  par(mar=c(5,4.5,2,1))
  
  n <- stages
  max.cv <- 10
  min.stage.time <- 20 # NOTE: min.stage.time set higher for minimum plot tick distance
  max.stage.time <- 120
  cv <- c(sample(seq(0.1, max.cv, by=.2), n, replace=F))
  stages.toogle.slope.status <- rep(0, n)
  
  if (steady.time == F) {
    probs <- (seq(-((max.stage.time - min.stage.time) / 2),
                  (max.stage.time - min.stage.time) / 2))^6
    probs <- Normalize(min(probs), max(probs), 0.1, 0.9, probs)
    times <- sample(min.stage.time:max.stage.time, n, replace=T,
                    prob=probs) # NOTE: Probs array for forced skew
  } else {
    times <- rep(max.stage.time / 2, n)
  }
  
  tick.pos <- integer(0)
  cv.array <- integer(0)
  cv.slope <- numeric(0)
  total.counter <- 0
  
  for (counter in 1:(length(cv))) {
    #if (counter < length(cv)) {
    #  cv.interval <- ((cv[counter] - cv[counter + 1]) * -1) / times[counter]
    #} else {
    #  cv.interval <- ((cv[counter] - cv[1]) * -1) / times[counter]
    #}
    if (counter > 1) {
      cv.interval <- ((cv[counter - 1] - cv[counter]) * -1) / times[counter]
    } else {
      cv.interval <- ((cv[length(cv)] - cv[counter]) * -1) / times[counter]
    }
    
    tick.pos[counter] <- total.counter + 1
    
    for (slope.counter in (1:times[counter])) {
      #cv.slope[total.counter] <- cv[counter] + (cv.interval * slope.counter)
      cv.slope[total.counter] <- ifelse(counter > 1, cv[counter - 1], cv[length(cv)]) +
        (cv.interval * slope.counter)
      cv.array[total.counter] <- cv[counter]
      total.counter <- total.counter + 1
    }
  }
  
  #plot(cv.array, col="lightgray", type="l", lwd=2, main="CV Slope Test", ylab="CV", xlab="Stages",
  #     cex.lab=.8, cex.axis=.7, cex.main=.8, xaxt="n")
  #axis(side=1, at=tick.pos, labels=1:length(cv), cex.axis=.5, las=2)
  
  #lines(cv.slope, col="red")
  
  df <- data.frame(x=1:length(cv.array), cv=cv.array, cv.slope=cv.slope)
  
  g <- ggplot(data=df) + geom_line(aes(x=x, y=cv), size=1.8, colour="LightGray") +
    geom_line(aes(x=x, y=cv.slope), size=.8, colour="Red") + theme_bw() + 
    scale_x_discrete(breaks=tick.pos, labels=1:length(cv)) +
    ggtitle("CV Slope Test") + xlab("Stages") + ylab("CV") +
    theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5, hjust=1)) +
    theme(plot.background = element_rect(fill=rgb(.95,.97,.96), colour='black'))
  
  g
  #par <- op
  #return (df)
}

plot(DoADC(1:1023, 8), type="l", col="blue", cex.main=1, cex.lab=.7, cex.axis=.7, main="DoADC test")

par(mfrow=c(1,2))
par(mar=c(4.5,4,2,.6))
resolution <- 50
notes <- 12
min.voltage <- 2
max.voltage <- 4
volt <- numeric(0)
ticks <- seq(min.voltage, max.voltage, by=(max.voltage - min.voltage) / resolution)
xtick <- length(ticks) / notes
for (counter in ticks)
  volt <- c(volt, GetQuantizedVoltValue(counter, min.voltage, max.voltage))
plot(volt, type="o", bg="wheat", col="red", cex.main=.8, cex.lab=.7, cex.axis=.7, pch=21,
     main="DAC test, quantized volt values", xlab="Notes", ylab="Quantized CV",
     ylim=c(min.voltage, max.voltage), xaxt="n")
axis(side=1, at=seq(1, length(ticks), by=xtick), labels=1:notes, cex.axis=.7)

min.voltage <- 0
max.voltage <- 10
volt <- numeric(0)
ticks <- seq(min.voltage, max.voltage, by=(max.voltage - min.voltage) / resolution)
xtick <- length(ticks) / notes
for (counter in ticks)
  volt <- c(volt, GetQuantizedVoltValue(counter, min.voltage, max.voltage))
plot(volt, type="o", bg="wheat", col="blue", cex.main=.8, cex.lab=.7, cex.axis=.7, pch=21,
     main="DAC test, quantized volt values", xlab="Notes", ylab="Quantized CV",
     ylim=c(min.voltage, max.voltage), xaxt="n")
axis(side=1, at=seq(1, length(ticks), by=xtick), labels=1:notes, cex.axis=.7)
par(mfrow=c(1,1))

GetStartStep2 <- function(min.voltage, max.voltage, notes) {
  if (min.voltage >= max.voltage)
    return ("Min voltage must be smaller than max voltage.")
  
  resolution <- 100
  volt <- numeric(0)
  ticks <- seq(min.voltage, max.voltage, by=(max.voltage - min.voltage) / resolution)
  xtick <- length(ticks) / notes
  
  for (counter in ticks)
    volt <- c(volt, GetQuantizedVoltValue(counter, min.voltage, max.voltage, notes))
  
  plot(volt, type="o", bg="wheat", col="blue", cex.main=.8, cex.lab=.7, cex.axis=.7, pch=21,
       main="DAC test, quantized volt values", xlab="Notes", ylab="Quantized CV",
       ylim=c(min.voltage, max.voltage), xaxt="n")
  axis(side=1, at=seq(1, length(ticks), by=xtick), labels=1:notes, cex.axis=.7)
}


notes <- 12
min.volt <- 2.2
max.volt <- 4.2
volt.per.step <- (max.volt - min.volt) / notes
interval <- (max.volt - min.volt) / 100
ticks <- seq(min.volt, max.volt, by=interval)
plot(ticks - (ticks %% volt.per.step), ylim=c(min.volt, max.volt))
grid()

# ------------------------------------------------------------------------------------------------------
# Get limited voltage range
DAC_10BIT <- 1023
MAX_VOLT <- 10
RANGE_0_2 <- 0
RANGE_2_4 <- 1
RANGE_4_6 <- 2
RANGE_6_8 <- 3
RANGE_8_10 <- 4

cur_volt <- 0:10
range <- RANGE_0_2
result <- Normalize(0, MAX_VOLT, range * 2, (range * 2) + 2, cur_volt) 
result
dac_out <- ceiling(DAC_10BIT * (result / (MAX_VOLT)))
plot(dac_out, col="blue", type="o")

# ------------------------------------------------------------------------------------------------------
# Start step and steps
manipulate(
  GetStartStep2(min.voltage, max.voltage, notes),
  min.voltage=slider(min=0, max=14, initial=0, step=1, label="Min voltage"),
  max.voltage=slider(min=1, max=15, initial=10, step=1, label="Max voltage"),
  notes=slider(min=1, max=32, initial=12, label="Max notes")
)
manipulate(
  GetStartStep(ref_voltage, max_step),
  ref_voltage=slider(min=1, max=15, initial=5, step=.5, label="VREF+ voltage"),
  max_step=slider(min=1, max=32, initial=8, label="Max steps in sequence")
  )
# MARF 248 stages
manipulate(
  CreateSlope(stages, steady.time),
  stages=slider(min=2, max=40, initial=16, step=1, label="Stages"),
  steady.time=checkbox(initial=F, label="Regular time intervals")
)
