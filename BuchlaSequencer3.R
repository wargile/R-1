# Buchla MARF 248 - simulation 3

stages_no <- 16

# Level 1 Programming
level1_output_voltage <- 0.0 # 0-10V
level1_interval_time <- 0.0 # 2 to 30 sec.

# Level 2 Programming
level2_output_pulses_1_2 <- 1

level2_output_voltage_quantize_continuous <- 1
level2_output_voltage_sloped_stepped <- 1
level2_output_voltage_range <- 0
level2_output_voltage_source <- 0

level2_opmode_cycle_start <- 0
level2_opmode_cycle_stop <- 0
level2_opmode_cycle_sustain <- 0
level2_opmode_cycle_enable <- 0

level2_time_range_divisions <- 0
level2_time_range_source <- 0

# Level 3 Programming
level3_po_stage_address <- 0
level3_po_display <- 0
level3_po_reset <- 0
level3_po_int_ext <- 0
level3_po_cont_strobe <- 0
level3_po_time_multiplier <- 0
level3_po_output_voltage_ref <- 0
level3_po_output_voltage_time <- 0
level3_po_output_voltage_all_pulses <- 0
level3_po_output_voltage_output_1_2 <- 0

level3_mode_advance <- 0
level3_mode_status_lights <- 0
level3_mode_stop_start <- 0

stage_status <- list()
stages <- list()
for (counter in 1:stages_no) {
  stage_status$level1_output_voltage <- 10
  stage_status$level1_interval_time <- 2
  stage_status$level2_output_voltage_sloped_stepped <- 1
  stages[[counter]] <- stage_status
}
stages

EditStages <- function(stage, level2_sloped_stepped, level1_output_voltage, level1_interval_time) {
  stages[[stage]]$level1_output_voltage <<- level1_output_voltage
  stages[[stage]]$level1_interval_time <<- level1_interval_time
  stages[[stage]]$level2_output_voltage_sloped_stepped <<- level2_sloped_stepped
  
  level1_output_voltage_disp <- integer(0)
  level1_interval_time_disp <- integer(0)
  level2_sloped_stepped_disp <- integer(0)
  breaks <- integer(0)
  
  tick.pos <- integer(0)
  cv.array <- integer(0)
  cv.slope <- numeric(0)
  total.counter <- 0
  
  for (counter in 1:stages_no) {
    if (counter == 1)
      breaks[counter] <- stages[[counter]]$level1_interval_time
    else
      breaks[counter] <- breaks[counter - 1] + stages[[counter]]$level1_interval_time
    
    level2_sloped_stepped_disp <- stages[[counter]]$level2_output_voltage_sloped_stepped
    print(c("Sloped/stepped: ", level2_sloped_stepped_disp))
    
    if (level2_sloped_stepped_disp == "0") # 0 = 'Stepped' stage status TODO: Constants
      cv.interval <- 0
    else {
      if (counter > 1) {
        cv.interval <- ((stages[[counter-1]]$level1_output_voltage -
                           stages[[counter]]$level1_output_voltage) * -1) / stages[[counter]]$level1_interval_time
      } else {
        cv.interval <- ((stages[[stages_no]]$level1_output_voltage -
                           stages[[counter]]$level1_output_voltage) * -1) / stages[[counter]]$level1_interval_time
      }
    }
    
    for (slope.counter in (1:stages[[counter]]$level1_interval_time)) {
      cv.slope[total.counter] <- ifelse(counter > 1, stages[[counter-1]]$level1_output_voltage,
                                        stages[[stages_no]]$level1_output_voltage) +
        (cv.interval * slope.counter)
      cv.array[total.counter] <- stages[[counter]]$level1_output_voltage
      total.counter <- total.counter + 1
    }
    tick.pos[counter] <- total.counter + 1
  }

  ret$cv.array <<- cv.array
  ret$cv.slope <<- cv.slope
  
  df <- data.frame(x=1:length(cv.array), cv=cv.array, cv.slope=cv.slope)
  
  g <- ggplot(data=df) + geom_line(aes(x=x, y=cv), size=1.8, colour="LightGray") +
    geom_line(aes(x=x, y=cv.slope), size=.8, colour="Red") + theme_bw() + 
    scale_x_discrete(breaks=tick.pos, labels=1:length(stages)) +
    ggtitle("CV Slope Test") + xlab("Stages") + ylab("CV") +
    theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5, hjust=1)) +
    theme(plot.background = element_rect(fill=rgb(.95,.97,.96), colour='black'))
  
  g
}

level2_sloped_stepped <- 0
ret <- list()

# Simulation for level1 controls
manipulate(
  level2_sloped_stepped=picker("Sloped"=1,"Stepped"=0, label="Sloped/stepped"),
  stage=slider(min=1, max=stages_no, initial=1, step=1, label="Select Stage"),
  level1_output_voltage=slider(min=0, max=10, initial=2, step=1, label="Output Voltage"),
  level1_interval_time=slider(min=2, max=30, initial=2, step=1, label="Interval time"),
  EditStages(stage, level2_sloped_stepped, level1_output_voltage, level1_interval_time)
)

# Simulation for level2 controls

# Simulation for level3 controls

par(mfrow=c(1,1))
