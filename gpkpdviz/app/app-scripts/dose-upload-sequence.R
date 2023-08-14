dose_upload_sequence <- function(dose_upload, pop_data){
  
  fill_default <- function(.df, .col, .fill){
    if(!(.col %in% colnames(.df))){
      .df[[.col]] <- .fill
    }
    .df
  }
  
  
  dose_upload <- 
    dose_upload %>% 
    fill_default(., "period", 1) %>% 
    fill_default(., "period_label", 1) %>% 
    fill_default(., "covar_propo", "none") %>% 
    fill_default(., "rate", 0) %>% 
    mutate(dose = amt)
  
  if(!("USUBJID" %in% colnames(dose_upload))){
    
    dose_upload_full <- data_frame()
    
    for(i in unique(pop_data$USUBJID)){
      
      dose_upload_full <- 
        bind_rows(
          dose_upload_full,
          dose_upload %>% mutate(USUBJID = i) %>% select(USUBJID, everything())
        )
      
    }
    
    dose_upload <- dose_upload_full
  }
  
  
  
  dose_upload <- dose_upload %>% bind_dose_and_pop(., pop_data)
  
  if(!all(dose_upload$covar_propo %in% c("", "none"))){
    
    covar_propos <- unique(dose_upload$covar_propo)
    covar_propos <- covar_propos[covar_propos != ""]
    
    dose_upload$covar_propo_col <- dose_upload[[covar_propos]]
    
    dose_upload <-
      dose_upload %>% 
      mutate(
        amt = case_when(
          covar_propo != "none" & amt != 0 ~ amt * covar_propo_col,
          TRUE ~ amt
        )
      ) %>% 
      select(-covar_propo_col)
  }
  

  
  dose_upload %>% 
    select(
      ID,
      amt, 
      cmt,
      evid,
      time, 
      dose,
      period,
      rate,
      period_label,
      covar_propo,
      USUBJID, 
      REC_TYPE, 
      everything()
    )
}
