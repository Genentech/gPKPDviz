get_start_time <- function(this_period_name, prev_period_name, input){

  this_period <- input[[paste0(this_period_name, "__period")]] 
  
  if(this_period == "0") return(NULL)
  
  prev_period <- input[[paste0(prev_period_name, "__period")]] 
  
  prev_period_use <- case_when(
    prev_period == "1" ~ "first",
    prev_period == "2" ~ "second",
    prev_period == "3" ~ "third"
  )
  
  prev_dose_start_time <- input[[paste0(prev_period_name, "__dose_start_time")]]
  prev_dose_start_time_unit <- input[[paste0(prev_period_name, "__dose_start_time_unit")]]
  
  prev_n_doses <- input[[paste0(prev_period_use, "__n_doses")]]
  prev_dosing_interval <- input[[paste0(prev_period_use, "__dosing_interval")]]
  prev_dosing_interval_unit <- input[[paste0(prev_period_use, "__dosing_interval_unit")]]
  
  if(is.null(prev_dosing_interval_unit)) return(NULL)
  if(nchar(prev_dosing_interval_unit) == 0) return(NULL)
  
  ans <- list()
  
  doseStartTime <- 
    unclass(as_time(as_time(prev_dose_start_time,
                            prev_dose_start_time_unit),
                    prev_dosing_interval_unit)
    )
  
  ans$doseStartTime <- doseStartTime +
    (prev_n_doses * prev_dosing_interval)
  
  ans$doseStartTimeUnit <- prev_dosing_interval_unit
  
  ans$noteText <-
    paste0(
      "Updated <i>Start time</i> for <span style='font-size:16px'><b>", 
      tools::toTitleCase(this_period_name), 
      " intervention</b></span> to be end time of <i>",
      tools::toTitleCase(prev_period_name),
      " intervention</i>: <span style='font-size:16px'><b>",
      ans$doseStartTime,
      " ", 
      ans$doseStartTimeUnit, "(s)</b></span>"
    )
  
  ans$interventionInfo <-
    paste0(
      "*<b>Start time unit</b> is <b>Dosing Interval Unit</b> of ", 
      tools::toTitleCase(prev_period_name),
      " intervention <b>(", 
      LETTERS[as.integer(prev_period)],
      ")</b>"
    )
  
  ans
}
