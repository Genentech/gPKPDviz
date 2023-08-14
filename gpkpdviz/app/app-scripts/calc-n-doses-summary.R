calc_n_doses_summary <- function(n_doses_data, model_time, cmt_choices, IDs_Simulated){
  
  n_doses_data_1 <- n_doses_data %>% filter(ID == min(ID))
  
  ans <- list()
  
  ans$n_doses <- n_doses_data_1 %>% 
    mutate(n_doses = n()) %>% 
    group_by(period) %>% 
    mutate(n_period = n()) %>% 
    ungroup() %>% 
    group_by(period_label) %>% 
    mutate(n_period_label = n()) %>% 
    ungroup() %>% 
    mutate(ntimesapplied = n_period_label / n_period) %>% 
    mutate(
      ndosenumeric = n_period_label,
      ndose = case_when(
        ntimesapplied == 1 ~ as.character(ndosenumeric),
        ntimesapplied == 2 ~ 
          paste0(ndosenumeric, "   &nbsp;&nbsp;<i><span style='font-weight:normal'>(<b>", n_period, "</b> doses ran twice)</span></i>"),
        ntimesapplied == 3 ~ 
          paste0(ndosenumeric, "   &nbsp;&nbsp;<i><span style='font-weight:normal'>(<b>", n_period, "</b> doses ran three times)</span></i>")
      )
    ) %>% 
    select(period_label, ndosenumeric, ndose) %>% 
    distinct() %>% 
    arrange(period_label)
  
  max_time_dose_row_1 <- n_doses_data_1 %>% filter(time == max(time)) %>% slice(1)
  
  max_dose_time_1 <- max_time_dose_row_1$time
  
  if(model_time == "hour"){
    max_dose_time_1 <- ceiling(max_dose_time_1 / 24)
  }
  
  # Add one since time starts at zero
  max_dose_time_1 <- max_dose_time_1 + 1
  
  # Round up to nearest 5 (in days)
  ans$max_dose_day <- 5 * ceiling(max_dose_time_1 / 5)
  
  cmt_choices_df <- data_frame(cmt_name = names(cmt_choices), cmt = cmt_choices)
  
  # dosing_event_names_df <- as.data.frame(dosing_event_names)
  # dosing_event_names_df$event_descr <- rownames(dosing_event_names_df)
  # rownames(dosing_event_names_df) <- NULL
  # dosing_event_names_df <- rename(dosing_event_names_df, evid = dosing_event_names)    
  # dosing_event_names_df$evid <- as.integer(dosing_event_names_df$evid)
  
  period_label_df <- data_frame(Intervention = c("A", "B", "C"),
                                period_label = c(1, 2, 3))
  
  covar_propo_df <- 
    n_doses_data %>%
    select(!!c("period", 
               "covar_propo",
               unique(n_doses_data$covar_propo)[unique(n_doses_data$covar_propo) != "none"])) %>% 
    distinct()
  
  covar_propo_df$covar_propo_col <- NA_real_
  
  for(i in unique(covar_propo_df$period)){
    
    prop.i <- covar_propo_df$covar_propo[covar_propo_df$period==i]
    
    if(prop.i != "none"){
      covar_propo_df$covar_propo_col[covar_propo_df$period == i] <- 
        covar_propo_df[[prop.i]][covar_propo_df$period==i]
    }
  }
  
  covar_propo_df <- covar_propo_df %>% select(period, covar_propo, covar_propo_col)
  
  n_doses_table <- n_doses_data %>% 
    left_join(covar_propo_df) %>% 
    mutate(
      amt = case_when(
        covar_propo != "none" ~ as.numeric(amt / covar_propo_col),
        TRUE ~ as.numeric(amt)
      ),
      rate = case_when(
        covar_propo != "none" ~ as.numeric(rate / covar_propo_col),
        TRUE ~ as.numeric(rate)
      )
    ) %>% 
    mutate(regimen = floor((ID - 1) / IDs_Simulated) + 1) %>% 
    group_by(period_label, amt, regimen) %>%
    filter(ID == min(ID)) %>% 
    ungroup() %>%
    group_by(regimen) %>% 
    # mutate(Dosing = paste(amt, collapse = "/", sep = "")) %>% 
    ungroup() %>% 
    left_join(cmt_choices_df) %>% 
    arrange(regimen, time) %>% 
    group_by(regimen, time) %>% 
    mutate(`# Doses` = n()) %>% 
    ungroup() %>% 
    # left_join(dosing_event_names_df) %>% 
    left_join(period_label_df) %>% 
    mutate(
      Admin. = case_when(
        rate == 0 ~ "Bolus",
        TRUE ~ paste0("Infusion <br/> (Duration: ",
                      round(amt/rate, 3),
                      " ",
                      model_time,
                      if_else(round(amt/rate, 3) == 1, "", "s"),
                      ")")
      )
    )
  
  n_doses_table <- n_doses_table %>% 
    mutate(time_diff_next_period = lead(time) - time) %>% 
    group_by(Intervention, period) %>% 
    mutate(time_diff = lead(time) - time) %>% 
    summarise(
      Amount = paste0(unique(range(amt, na.rm = TRUE)), collapse = " - "),
      `Amount Proportion` = ifelse(
        first(covar_propo == "none"), "None (flat dosing)", first(covar_propo)
      ),
      Admin. = paste0(unique(Admin.), collapse = ";<br/> "),
      time = min(time),
      ii = ifelse(is.na(first(time_diff)), last(time_diff_next_period), first(time_diff)),
      `# Doses` = n(),
      `Event ID` = first(evid),
      `Comp.` = unique(cmt_name)
    ) %>% 
    ungroup() %>% 
    arrange(period) %>% 
    select(-period) %>% 
    mutate_all(as.character)
  
  # interventions <- c("First", "Second", "Third")[1:(nrow(n_doses_table) / length(unique(n_doses_table$Regimen)))]
  
  # n_doses_table$Intervention <- rep(interventions, times = nrow(n_doses_table) / length(interventions))
  
  colnames(n_doses_table)[colnames(n_doses_table) == "time"] <-
    paste0("Start Time (", tools::toTitleCase(model_time), ")")
  
  colnames(n_doses_table)[colnames(n_doses_table) == "ii"] <-
    paste0("Dose Interval (", tools::toTitleCase(model_time), ")")
  
  ans$n_doses_table <- n_doses_table
  
  ans
}