# MuRF functions in R -- Copyright (C) T. Bakkelokken 2011-2013
#
# setwd("c:/coding/R")
# sink("murflog.lis") ## Log console output from program to file
# rm(list=setdiff(ls(), "murf.data"))
# rm(list=setdiff(ls(), c("constellations_data", "hygxyz_data", "ngc2000_data")))
# source("murf.R")
# par(mfrow = c(2, 1)) ## Two subplots in one column


# Require libraries ----
require(graphics)
require(ggplot2)
library(manipulate)
library(reshape2) # for melt(...)


# Public properties ----
max_pot_pos <- 10.0 # Static
timer_ticks <- 100.0; # Control Adjustable
volt_ref <- 5.0 # Control Adjustable
peak_pos_rise_fall_curve <- 3.9 # Control Adjustable
curve_dip <- 5.5 # Control Adjustable
curve_peak_rise <- 80.0 # TODO: Control Adjustable
curve_peak_fall <- 151.0 # TODO: Control Adjustable

# Public functions ----

# Return plot data to main window. NOTE: Function contains PIC timer tick block ----
GetPlotData <- function(timer_ticks=100, pot_pos=3.2, volt_ref=5.0, peak_pos_rise_fall_curve=3.9, curve_dip=5.5) {
  # Call get_volt_value() pr. plot data pos
  the_curve <- numeric(timer_ticks + 1)
    
  # Assign to member vars
  pot_pos_org <- pot_pos
  
  if (pot_pos_org > max_pot_pos / 2.0)
    pot_pos <- max_pot_pos - pot_pos
  
  # cat("\nGPD-Pot pos: ", pot_pos, "\n")
  
  # Get rise and fall steps for pot_pos
  #steps.rise.fall <- numeric(2)
  steps.rise.fall <- GetRiseFallStepsAtPotPos(timer_ticks, pot_pos, peak_pos_rise_fall_curve, curve_dip)
  steps_rise <- steps.rise.fall[1]
  steps_fall <- steps.rise.fall[2]
  
  # cat("GPD-Rise/fall: ", steps_rise, "--", steps_fall, "\n")
  
  # Convert the percents to number of steps in timer_ticks
  steps_rise <- (steps_rise * timer_ticks) / 100.0
  steps_fall <- (steps_fall * timer_ticks) / 100.0

  # Get peak position for current timer_ticks
  peak <- GetPeakPosInTimerTicks(timer_ticks, pot_pos)
  
  # cat("GPD-Rise/fall timer_ticks: ", steps_rise, "--", steps_fall, "\n")
  # cat("GPD-Peak: ", peak, "\n")
  
  # Get the cutoff volt for the fall curve and volt_per_step_fall
  #cutoff.and.voltptstepfall = numeric(2)
  cutoff.and.voltprstepfall <- GetCutoffVoltForFallCurve(timer_ticks, steps_fall, peak, volt_ref)
  cutoff <- cutoff.and.voltprstepfall[1]
  volt_pr_step_fall <- cutoff.and.voltprstepfall[2]
  
  # cat("\nGPD-Cutoff: ", cutoff, " volt_per_step_fall: ", volt_pr_step_fall, "\n")
  
  if (cutoff > 0)
    start_pos <- 0
  else {
    start_pos <- peak - steps_rise
    
    if (start_pos < 0)
      start_pos <- 0
  }
  
  if (steps_rise == 0)
    volt_pr_step_rise <- volt_ref
  else {
    if ((peak - start_pos) != 0)
      volt_pr_step_rise <- (volt_ref - cutoff) / (peak - start_pos)
    else
      volt_pr_step_rise <- volt_ref
  }
  
  if (volt_pr_step_rise > volt_ref)
    volt_pr_step_rise <- volt_ref
  
  # cat("GPD-VPSR: ", volt_pr_step_rise, " VPSF: ", volt_pr_step_fall, "\n")
  
  # START: This block is for each timer tick in PIC from 0 to timer_ticks
  for (counter in 0:timer_ticks) {
    if (pot_pos_org > max_pot_pos / 2.0)
      volt <- GetVoltValue(timer_ticks - counter, start_pos, volt_pr_step_rise, volt_pr_step_fall, cutoff, steps_fall, peak, volt_ref)
    else
      volt <- GetVoltValue(counter, start_pos, volt_pr_step_rise, volt_pr_step_fall, cutoff, steps_fall, peak, volt_ref)
    
    # cat("V:", volt, " ")
    
    the_curve[counter + 1] <- volt
  }
  # END: This block is for each timer tick in PIC from 0 to timer_ticks
  
  cat("GPD-Curve data: ", head(the_curve), "(...)", tail(the_curve), "\n\n")
  
  # Plot the curve data
  par(bg="cornsilk")
  
  # Return curve data to caller
  return(the_curve)
}

# Test/debug function to get the rise/fall curves data ----
GetRiseFallPercentCurves <- function(timer_ticks=100, peak_pos_rise_fall_curve=3.9, curve_dip=5.5) {
  array_counter <- 1
  counter <- 0.0
  max.elements <- ceiling((max_pot_pos / 2.0) * max_pot_pos) + 1
  steps.rise <- numeric(max.elements)
  steps.fall <- numeric(max.elements)

  #steps.rise.fall <- numeric(2)
  
  for(counter in seq(from=0, to=max_pot_pos / 2.0, by=0.1)) {
    steps.rise.fall <- GetRiseFallStepsAtPotPos(timer_ticks, counter, peak_pos_rise_fall_curve, curve_dip)
    
    steps.rise[array_counter] <- ifelse(steps.rise.fall[1] < 0, 0, steps.rise.fall[1])
    steps.fall[array_counter] <- ifelse(steps.rise.fall[2] < 0, 0, steps.rise.fall[2])
    array_counter <- array_counter + 1
  }
  
  cat("(...)", tail(steps.rise), "\n")
  cat("(...)", tail(steps.fall), "\n")
  
  steps.rise.fall <- data.frame(x=seq(1:max.elements), steps.rise=steps.rise, steps.fall=steps.fall)
  #colnames(steps.rise.fall)[1] <- "X"
  #colnames(steps.rise.fall)[2] <- "Steps rise"
  #colnames(steps.rise.fall)[3] <- "Steps fall"
  
  #steps.rise.fall <- melt(steps.rise.fall, id='X', variable_name='series')
  
  write.table(steps.rise.fall, "murfdata.txt", append=FALSE, sep=";", col.names=TRUE)

  return(steps.rise.fall)
}

# Function to get the rise and fall steps at pot_pos ----
GetRiseFallStepsAtPotPos <- function(timer_ticks, pot_pos, peak_pos_rise_fall_curve, curve_dip) {
  elements <- 0
  interval <- 0.0
  steps.rise.fall <- numeric(2)
  
  # Get rise percent
  if (pot_pos <= peak_pos_rise_fall_curve) {
    elements <- (peak_pos_rise_fall_curve * (timer_ticks / max_pot_pos))
    interval <- sqrt(curve_peak_rise) / elements
    steps_rise <- ((pot_pos * (timer_ticks / max_pot_pos)) * interval)^2
  } else {
    elements <- (((max_pot_pos / 2.0) - peak_pos_rise_fall_curve) * (timer_ticks / max_pot_pos))
    interval <- (sqrt(curve_peak_rise) - sqrt(curve_dip)) / elements
    steps_rise <- (sqrt(curve_peak_rise) -
                            (interval * ((pot_pos - peak_pos_rise_fall_curve) * (timer_ticks / max_pot_pos))))^2
  }
  
  # Get fall percent
  if (pot_pos <= peak_pos_rise_fall_curve) {
    elements <- (peak_pos_rise_fall_curve * (timer_ticks / max_pot_pos))
    interval <- sqrt(curve_peak_fall) / elements
    steps_fall <- (((pot_pos * (timer_ticks / max_pot_pos)) * interval)^2) # note: removed ceiling
  } else {
    elements <- (((max_pot_pos / 2.0) - peak_pos_rise_fall_curve) * (timer_ticks / max_pot_pos))
    interval <- (sqrt(curve_peak_fall) - sqrt(curve_dip)) / elements
    steps_fall <- ((sqrt(curve_peak_fall) -
                            (interval * ((pot_pos - peak_pos_rise_fall_curve) * (timer_ticks / max_pot_pos))))^2) # note: removed ceiling
  }
  
  # cat("GRFAAPP-steps_rise: ", steps_rise, " steps_fall: ", steps_fall, "\n")
  
  steps.rise.fall[1] <- steps_rise
  steps.rise.fall[2] <- steps_fall
  return(steps.rise.fall)
}

# Function to get the curve peak pos in timer_ticks for pot_pos ----
GetPeakPosInTimerTicks <- function(timer_ticks, pot_pos) {
  peak <- (pot_pos^2 / (max_pot_pos / 2.0)) * (timer_ticks / max_pot_pos)
  
  # cat("GPPIR-peak: ", peak, "\n")
  
  return(peak)
}

# Function to get the cutoff volt on fall curve, and volt_pr_step_fall ----
GetCutoffVoltForFallCurve <- function(timer_ticks, steps_fall, peak, volt_ref) {
  cutoff_steps <- 0
  volt.and.cutoff <- numeric(2)
  
  if (steps_fall != 0) # Avoid DIV/0!
    volt_pr_step_fall <- (volt_ref / steps_fall)
  else
    volt_pr_step_fall <- volt_ref
  
  if (volt_pr_step_fall > volt_ref)
    volt_pr_step_fall <- volt_ref
  
  if (timer_ticks - peak - steps_fall < 0) {
    cutoff_steps <- abs(timer_ticks + 0 - peak - steps_fall) # NOTE: + 0: changed from + 1
    cutoff <- (cutoff_steps + 0) * volt_pr_step_fall # NOTE: + 0: changed from + 1
  } else cutoff <- 0
  
  # cat("GCVFFC-cutoff: ", cutoff, " volt_per_step_fall: ", volt_pr_step_fall, " steps fall: ", steps_fall, "peak: ", peak, "volt ref: ", volt_ref)
  
  volt.and.cutoff[1] <- cutoff
  volt.and.cutoff[2] <- volt_pr_step_fall
  return(volt.and.cutoff)
}

# Function to get the volt_value for a PIC timer tick in the current timer_ticks ----
GetVoltValue <- function(timer_ticks_pos, start_pos, volt_pr_step_rise, volt_pr_step_fall, cutoff, steps_fall, peak, volt_ref) {
  #cat("\nGVV-timer_ticks_pos: ", timer_ticks_pos, " start_pos: ", start_pos, " cutoff: ", cutoff, "volt_ref: ", volt_ref)
  #cat(" volt pr. step rise: ", volt_pr_step_rise, "volt pr. step fall: ", volt_pr_step_fall, " peak: ", peak)
  #cat(" steps fall: ", steps_fall, "\n")
  
  if (timer_ticks_pos < start_pos)
    volt <- 0
  else {
    if (timer_ticks_pos < peak)
      volt <- (cutoff + (volt_pr_step_rise * (timer_ticks_pos - start_pos)))^2 / volt_ref
    else {
      if (timer_ticks_pos <= peak + steps_fall)
        volt <- (volt_ref - (volt_pr_step_fall * (timer_ticks_pos - peak)))^2 / volt_ref
      else
        volt <- 0
    }
    
    # Ensure DAC volt = volt_ref at peak pos
    if (timer_ticks_pos == round(peak) && volt < volt_ref) # changed from ceiling
      volt <- volt_ref
  }

  # cat("GGV-volt: ", volt)
  
  return(volt)
}


# Public debug/graph functions for manipulating curve data ----

# Function for manipulate
ShowMurfCurveData <- function() {
  manipulate( {
      murf.volt.data <- data.frame(x=seq(0:timer_ticks),
        voltdata=GetPlotData(timer_ticks, pot_pos, volt_ref, peak_pos_rise_fall_curve, curve_dip))
      
      g <- ggplot(murf.volt.data, aes(x=x))
      g <- g + scale_size_area() + xlab("Timer ticks") + ylab("Volt") + ggtitle("MuRF curve data") +
        theme(plot.title = element_text(size = 18, colour="steelblue4"))
      #g <- g + scale_x_continuous(breaks=seq(0, timer_ticks, 10))
      g <- g + geom_area(aes(y=voltdata), alpha=0.2, fill="blue") +
        geom_point(aes(y=voltdata), colour="blue", size=3.4, shape=16, alpha=.6)
      g
    },

    timer_ticks=slider(16, 160, 100, "Timer ticks"),
    pot_pos=slider(0, 10, 5, "Pot position", step=0.1),
    volt_ref=slider(1, 15, 5, "Volt reference", step=0.1),
    peak_pos_rise_fall_curve=slider(0.1, 4.9, 3.9, "Peak pos in rise/fall curve", step=0.1),
    curve_dip=slider(0.5, curve_peak_rise, 4.5, "Curve dip", step=0.1)
  )
}

# Function for manipulate
# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
ShowRiseFallCurveData <- function() {
  manipulate( {
      rise.fall.data <- GetRiseFallPercentCurves(timer_ticks, peak_pos_rise_fall_curve, curve_dip)

      g <- ggplot(rise.fall.data, aes(x=x))
      g <- g + scale_size_area() + xlab("Timer ticks") + ylab("Rise and fall curves") + ggtitle("Rise and fall steps") +
        theme(plot.title = element_text(size = 18, colour="steelblue4"))
      #g <- g + scale_x_continuous(breaks=seq(0, timer_ticks, 10))
      g <- g + geom_area(aes(y=steps.rise), alpha=0.2, fill="blue") +
        geom_point(aes(y=steps.rise, colour="steps.rise"), colour="blue", size=3.4, shape=16, alpha=.6)
      g <- g + geom_area(aes(y=steps.fall), alpha=0.2, fill="green") +
        geom_point(aes(y=steps.fall, colour="steps.fall"), colour="green", size=3.4, shape=16, alpha=.6)
      
      # TODO: Need melt(...) first for legends to appear?
      g <- g + scale_colour_manual("Steps", breaks = c("steps.rise", "steps.fall"), values = c("blue", "green"))
      
      #g <- g + theme(legend.position=c(.6,0.8)) +
      #  theme(legend.background = element_rect(colour="black", fill="grey90", size=1, linetype="solid")) +
      #  theme()
      g
    },

    #timer_ticks=slider(16, 160, 100, "Timer ticks"),
    peak_pos_rise_fall_curve=slider(0.1, 4.9, 3.9, "Peak pos in rise/fall curve", step=0.1),
    curve_dip=slider(0.5, curve_peak_rise, 15, "Curve dip", step=0.1)
  )
}

# Show rise and fall steps curves, with legends
ShowRiseFallCurveData2 <- function() {
  df <- GetRiseFallPercentCurves(timer_ticks, peak_pos_rise_fall_curve, curve_dip)
  df <- subset(df, select=c(steps.rise, steps.fall))
  dfm <- melt(df, variable="curve.type", na.rm=TRUE)
  dfm["x"] <- seq(1:(length(df$steps.rise)))
  
  # http://www.cookbook-r.com/Graphs/Legends_%28ggplot2%29/
  # names(dfm)[names(dfm) == "group"]  <- "Experimental Condition"
  
  g <- ggplot(dfm, aes(x=x, value, group=curve.type, colour=curve.type)) + geom_line() + geom_point(size=3)
  g
}

ShowMurfCurveAndRiseFallData <- function() {
  manipulate( {
      murf.volt.data <- data.frame(x=seq(0:timer_ticks),
                                   voltdata=GetPlotData(timer_ticks, pot_pos, volt_ref, peak_pos_rise_fall_curve, curve_dip))
      
      g1 <- ggplot(murf.volt.data, aes(x=x))
      g1 <- g1 + scale_size_area() + xlab("Timer ticks") + ylab("Volt") + ggtitle("MuRF curve data") +
        theme(plot.title = element_text(size = 18, colour="steelblue4"))
      #g1 <- g1 + scale_x_continuous(breaks=seq(0, timer_ticks, 10))
      g1 <- g1 + geom_area(aes(y=voltdata), alpha=0.2, fill="blue") +
        geom_point(aes(y=voltdata), colour="blue", size=3.4, shape=16, alpha=.6)
      g1

      rise.fall.data <- GetRiseFallPercentCurves(timer_ticks, peak_pos_rise_fall_curve, curve_dip)
      
      g2 <- ggplot(rise.fall.data, aes(x=x))
      g2 <- g2 + scale_size_area() + xlab("Timer ticks") + ylab("Rise and fall curves") + ggtitle("Rise and fall steps") +
        theme(plot.title = element_text(size = 18, colour="steelblue4"))
      #g <- g + scale_x_continuous(breaks=seq(0, timer_ticks, 10))
      g2 <- g2 + geom_area(aes(y=steps.rise), alpha=0.2, fill="blue") +
        geom_point(aes(y=steps.rise, colour="steps.rise"), colour="blue", size=3.4, shape=16, alpha=.6)
      g2 <- g2 + geom_area(aes(y=steps.fall), alpha=0.2, fill="green") +
        geom_point(aes(y=steps.fall, colour="steps.fall"), colour="green", size=3.4, shape=16, alpha=.6)
      
      g2 <- g2 + scale_colour_manual("Steps", breaks = c("steps.rise", "steps.fall"), values = c("blue", "green"))
      
      grid.arrange(g1, g2, nrow=2, ncol=1)
    },
                
    timer_ticks=slider(16, 160, 100, "Timer ticks"),
    pot_pos=slider(0, 10, 5, "Pot position", step=0.1),
    volt_ref=slider(1, 15, 5, "Volt reference", step=0.1),
    peak_pos_rise_fall_curve=slider(0.1, 4.9, 3.9, "Peak pos in rise/fall curve", step=0.1),
    curve_dip=slider(0.5, curve_peak_rise, 4.5, "Curve dip", step=0.1)
  )
}
