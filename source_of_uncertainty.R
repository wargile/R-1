# Source of Uncertainty -- R style!

#   Parameters:
#     1) Rounds: Minimum 1
#     2) Random phase: true/false
#     3) Increase frequency/interval: true/false
#     4) ADC value: Increase value - greater random variations
#     5) Follow baseline: true/false
#     6) Complex shape: true/false
#     7) Start point: Where the curve should start. Default is 0 (start = -pi)

require(graphics)
require(ggplot2)
library(manipulate)

# Define globals:
rounds <- 2
rnd_time <- F
inc_freq <- F
follow_baseline <- T
complex_shape <- F
start_point <- 0
volt <- 5

source_of_uncertainty <- function(rounds, rnd_time=0, inc_freq=F,
                                  follow_baseline=T, complex_shape=F,
                                  start_point=0, volt=5)  {
  FIG_NAME <- 'Source of Uncertainty'
  VOLT_REF <- 5
  #DAC_12BIT_MAX <- hex2dec('fff') % 12-bit: 4095
  #ADC_10BIT_MAX <- hex2dec('fff') % 10-bit: 1023
  interval <- 0.03
  curve_data <- numeric()
  rand_value <- 1

  adc_value <- (VOLT_REF / volt / 2) # Default: No ADC-input

  MAX_ELEMENTS <- ceiling((((pi - (pi * -1)) / interval) * rounds) + (rounds - 1))

  mod_interval <- 1 # Always force pitch change on init
  curve_pos <- -pi + start_point
  random_curve_data <- numeric(MAX_ELEMENTS) 
  curve_data <- numeric(MAX_ELEMENTS)
  
  cat("Calling function with rounds", rounds, "...\n")

  for (counter in 1:MAX_ELEMENTS) {
    curve_data[counter] <- sin(curve_pos)

    if (complex_shape == T) {
      curve_shape <- sin(curve_pos) + cos(curve_pos * 4)
    } else {
      curve_shape <- sin(curve_pos)
    }
    
    if ((counter %% mod_interval) == F) { # Change frequency
      rand_value <- ((runif(1, 0, 1) * curve_shape) / adc_value)
    
      if (follow_baseline == F) {
        random_curve_data[counter] <- curve_shape - rand_value
      } else {
        random_curve_data[counter] <- rand_value
      }
      
      if (rnd_time == T) {
        mod_interval <- ceiling(runif(1, 0, 1) * 4)
      }
    } else { # Do not change pitch
      if (counter > 1) {
        random_curve_data[counter] <- random_curve_data[counter - 1]
      }
    }
  
    if (curve_pos == pi) {
      curve_pos <- -pi
    } else {
      curve_pos <- curve_pos + interval
    }
  
    #  Increase interval size to increase curve frequency
    if (inc_freq == T) {
      interval <- interval + (rounds / 25000.0)
    }
  }
  
  return(list(MAX_ELEMENTS, curve_data, random_curve_data))    
}

source_of_uncertainty_graph <- function() {
  manipulate( {
      ret <- source_of_uncertainty(rounds, rnd_time, inc_freq,
                                   follow_baseline, complex_shape, start_pos, volt)
      
      MAX_ELEMENTS <- ret[[1]]
      curve_data <- ret[[2]]
      random_curve_data <- ret[[3]]
      df <- data.frame(x=1:MAX_ELEMENTS, cd=curve_data, rcd=random_curve_data)
      
      #cat(random_curve_data[1:10], "\n")
      
      g <- ggplot(data=df) + geom_line(aes(x=x, y=cd), color="powderblue", size=2) +
        geom_line(aes(x=x, y=rcd), color="blue") + xlab("Time") + ylab("Pitch") +
        ggtitle("Source of Uncertainty") + 
        geom_hline(yintercept=0, colour="gray", size=.5)
      g
    },
  
    rounds=slider(.1, 4, 1.2, "Rounds", step=.1),
    rnd_time=checkbox(rnd_time, label="Random phase/time"),
    inc_freq=checkbox(inc_freq, label="Increase frequency"),
    follow_baseline=checkbox(follow_baseline, label="Follow baseline"),
    complex_shape=checkbox(complex_shape, label="Complex shape"),
    start_pos=slider(-pi, pi, -pi, "Start pos", step=.1),
    volt=slider(.1, 5, 5, "Volt", step=.1)
  )
}
