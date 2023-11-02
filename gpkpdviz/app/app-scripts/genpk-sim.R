genpk_sim <- function(rV, input){
  
  if(input$doseDataType == "buildDose"){
    
    simulation_data <- 
      dosing_sequence(
        input,
        .d1,.d2,.d3,.p1,.p2,.p3,
        idata=rV$populationData,
        nid=rV$nPopIDs
      )
    
    
  } else if(input$doseDataType == "uploadDose"){
    
    simulation_data <- 
      dose_upload_sequence(
        rV$doseUploadData,
        rV$populationData
      )
    
  }
  
  genpk_recsort <- 1  # input$relrec
  genpk_obsonly <- FALSE
  genpk_carry.out <- c("period", "EVID", "AMT", "REC_TYPE")
  dose_time_adj <- 0.0000000001
  
  if(all(input$modelCovariates != "no_covariates")){
    genpk_carry.out <- c(genpk_carry.out, input$modelCovariates)
  }
  
  simulation_data_expanded <- simulation_data
  
  if (!is.null(rV$samplingTimes)) {
    simulation_data_expanded <-
      bind_rows(
        simulation_data_expanded,
        rV$samplingTimes %>% 
          select(USUBJID, time, contains("cmt")) %>% 
          inner_join(simulation_data_expanded %>% distinct(USUBJID, ID)) %>% 
          mutate(evid = 0, amt = 0, REC_TYPE = -1, covar_propo = "")
      ) %>% 
      sort_and_fill_covars(x = .)
  }
  
  if(nchar(input$user_sample_times) > 0){
    
    numeric_sample_times <- as.numeric(
      unlist(strsplit(input$user_sample_times, " ", fixed = TRUE))
    )
    
    numeric_sample_times <- numeric_sample_times[!is.na(numeric_sample_times)]
    
    if(length(numeric_sample_times) == 0){
      stop("Sampling times not in correct format")
    }
    
    samples_times_to_model_times_conversion <- case_when(
      input$model_time == input$user_sample_times_unit ~ 1,
      input$model_time == "hour" & input$user_sample_times_unit == "day" ~ 24,
      # input$model_time == "hour" & input$user_sample_times_unit == "week" ~ ,
      input$model_time == "day" & input$user_sample_times_unit == "hour" ~ (1/24),
      # input$model_time == "day" & input$user_sample_times_unit == "week" ~ (1/24),
      
      # input$model_time == "week" & input$user_sample_times_unit == "hour" ~ (1/24),
      # input$model_time == "week" & input$user_sample_times_unit == "day" ~ (1/24),
      TRUE ~ 0
    )
    
    times_to_add <- 
      numeric_sample_times * samples_times_to_model_times_conversion
    
    if(input$user_sample_times_relative_to == "last_dose"){
      
      dose_times <- simulation_data_expanded %>% filter(evid != 2) %>% pull(time) %>% unique()
      
      times_to_add <- sort(rep(times_to_add, each = length(dose_times)) + dose_times)
      
      times_to_add <- times_to_add + (dose_time_adj / 10)
      
    }
    
  } else {
    times_to_add <- numeric(0)
  }
  
  
  end <- to_model_time(input$max_obs_time, unit = input$max_obs_time_unit, input)
  
  delta <- to_model_time(input$obs_interval, unit = input$obs_time_unit, input)
  
  
  if(rV$seed != "Random"){
    set.seed(as.integer(rV$seed))
  } else {
    set.seed(as.integer(runif(1, 5, 500)))
  }
  
  sim_times <- stime(tgrid(0, end, delta , times_to_add))
  
  sim_times <- setdiff(
    sim_times,
    simulation_data_expanded %>% filter(REC_TYPE <= 0) %>% pull(time) %>% unique
  )
  
  modelBuildRun <- rV$modelBuild
  
  if(input$doZeroRe){
    modelBuildRun <- modelBuildRun %>% zero_re()
  }
  
  
  simulation_data_expanded <-
    bind_rows(
    simulation_data_expanded,
    simulation_data_expanded %>% 
      distinct(ID, USUBJID) %>%
      group_by(ID) %>% 
      slice(rep(1,length(sim_times))) %>% 
      ungroup() %>%
      mutate(
        time = rep(sim_times, length(unique(simulation_data_expanded$ID))), 
        REC_TYPE = 0, 
        evid = 0,
        amt = 0,
        covar_propo = ""
        )
    ) %>% 
    sort_and_fill_covars(x = .)
  
  mrgSimulationsBuild <-
    modelBuildRun %>%
    mrgsim(data=simulation_data_expanded, 
           end = -1, 
           # add = sim_times,
           obsonly = genpk_obsonly,
           recsort = genpk_recsort,
           carry.out=genpk_carry.out,
           # rtol = 
           # hmax = 
           # maxsteps = 
           atol = as.numeric(paste0("1E-", input$aTolValue))) %>% 
    tibble::as_tibble() %>% 
    mutate(
      SIM_TYPE = case_when(
        REC_TYPE == -1 ~ "User Specified",
        (time %in% times_to_add) ~ "User Specified",
        TRUE ~ "Simulation"
      ),
      REC_TYPE = case_when(
        REC_TYPE == 0 & EVID == 2 ~ "Population",
        REC_TYPE == 1 & EVID == 1 ~ "Dose",
        REC_TYPE == 1 & EVID == 2 ~ "Other type event",
        REC_TYPE == 1 & EVID == 3 ~ "System reset",
        REC_TYPE == 1 & EVID == 4 ~ "System reset and dose",
        REC_TYPE == 1 & EVID == 8 ~ "Replace the amount in a specific compartment",
        TRUE ~ "Observation"
      )
    ) %>% 
    select(SIM_TYPE, REC_TYPE, everything())
  
  sim_data_join <-
    simulation_data_expanded %>% 
    filter(covar_propo != "") %>% 
    select(USUBJID, ID, period, covar_propo, contains("(Bins)")) %>% 
    distinct()
  
  genpkSim <- 
    mrgSimulationsBuild %>% 
    mutate(SIM_ID = input$sim_id) %>% 
    mutate_at(vars(time),
              funs(
                case_when(
                  !(REC_TYPE %in% c("Population", "Observation")) ~ . + dose_time_adj,
                  TRUE ~ .
                )
              )
    ) %>% 
    left_join(sim_data_join) %>% 
    mutate(
      covar_propo = if_else(EVID == 1, covar_propo, "")
    ) %>% 
    arrange(ID, time, -EVID) %>% 
    group_by(ID) %>% 
    mutate(`Cumulative Dose Count` = cumsum(!(REC_TYPE %in% c("Population", "Observation")))) %>%
    mutate(`Cumulative Dose Count` = if_else(time == 0,
                                             1L,
                                             `Cumulative Dose Count`)) %>%
    ungroup() %>% 
    select(USUBJID, everything())
  
  
  # Gather inputs -----------------------------------------------------------
  ans <- list()
  
  ans$mrgInputs <- rV$genpkSimList$mrgInputs
  
  ans$mrgInputs[[input$sim_id]] <- list()
  
  ans$mrgInputs[[input$sim_id]]$modelCode <- rV$modelCode
  
  populationData <- rV$populationData
  
  if(all(colnames(populationData) == "ID")){
    populationData <- data_frame(Message = "No Population Data")
  }
  
  ans$mrgInputs[[input$sim_id]]$populationData <- populationData
  ans$mrgInputs[[input$sim_id]]$doseTable <- rV$doseTable
  
  
  # Append other sims -------------------------------------------------------
  prevSims <- rV$genpkSimList$mrgSimulations
  
  if(input$sim_id %in% prevSims$SIM_ID) {
    prevSims <- filter(prevSims, SIM_ID != input$sim_id)
  }
  
  genpkSim <- bind_rows(genpkSim, prevSims)
  
  
  # Create dose interval time columns ---------------------------------------
  # Change to input later
  # input$timeframeGroupings <- c("Day", "Week", "First Dosing Interval (12 Hours)" = "12 Hours", "16 Days")
  
  genpkSim <- genpkSim %>% select(-contains("Dosing Interval"))
  
  to_day <- case_when(
    input$model_time == "hour"~ 24,
    input$model_time == "day"~ 1,
    # input$model_time == "week"~ ,
    TRUE ~ 0
  )
  
  to_hour <- case_when(
    input$model_time == "hour"~ 1,
    input$model_time == "day"~ 1 / 24,
    # input$model_time == "week"~ ,
    TRUE ~ 0
  )
  
  genpkSim <- 
    genpkSim %>% 
    mutate(
      day_continuous = time / to_day,
      day_discrete = ifelse(day_continuous == round(day_continuous, 0),
                            day_continuous,
                            ceiling(day_continuous)),
      hour_continuous = time / to_hour,
      hour_discrete = ifelse(hour_continuous == round(hour_continuous, 0),
                             hour_continuous,
                             ceiling(hour_continuous)),
      week_continuous = day_continuous / 7,
      week_discrete = ifelse(week_continuous == round(week_continuous, 0),
                             week_continuous,
                             ceiling(week_continuous))
    )
  
  timeGroups <- rV$timeframeGroupings
  
  # for(time_frame.i in timeGroups){
  #   
  #   if(time_frame.i == "Day"){
  #     val.i <- 1
  #     unit.i <- "Days"
  #   } else if(time_frame.i == "Week"){
  #     val.i <- 7
  #     unit.i <- "Days"
  #   } else {
  #     split_time_frame.i <- unlist(strsplit(time_frame.i, split = " ", fixed = TRUE))
  #     val.i <- as.numeric(split_time_frame.i[1])
  #     unit.i <- split_time_frame.i[2]
  #     rm(split_time_frame.i)
  #   }
  #   
  #   if(unit.i == "Days"){
  #     discrete_col.i <- genpkSim$day_discrete
  #   } else if(unit.i == "Hours") {
  #     discrete_col.i <- genpkSim$hour_discrete
  #   } else if(unit.i == "Weeks") {
  #     discrete_col.i <- genpkSim$week_discrete
  #   }
  #   
  #   rm(unit.i)
  #   
  #   
  #   col.i <- ceiling(discrete_col.i / val.i)
  #   rm(discrete_col.i)
  #   col.i <- ifelse(col.i == 0, 1, col.i)
  #   col.i <- factor(col.i)
  #   
  #   genpkSim[[time_frame.i]] <- col.i
  #   
  #   rm(col.i)
  # }
  
  genpkSim <- 
    genpkSim %>% 
    select(-day_continuous,
           -day_discrete,
           -hour_continuous,
           -hour_discrete,
           -week_discrete,
           -week_continuous) %>% 
    select(SIM_TYPE, REC_TYPE, SIM_ID, everything())
  
  
  ans$mrgSimulations <- genpkSim
  
  # Return ------------------------------------------------------------------
  ans
}
