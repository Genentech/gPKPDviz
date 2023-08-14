validate_prep_and_bind_overlay_data <- function(rV){
  
  overlayData <- rV$overlayDataRaw
  mrgSimulations <- rV$genpkSimList$mrgSimulations
  
  colsFound <- colnames(overlayData)
  
  
  # Validate ----------------------------------------------------------------
  reqCols <- c("USUBJID", "time")
  
  for(i in reqCols){
    if(!(i %in% colsFound)){ 
      stop(paste0(i, " column not found"))
    }
    
    if(any(is.na(overlayData[[i]]))){
      stop(paste0(i, " column has missing or NA values"))
    }
  }
  
  overlayData$time <- as.numeric(overlayData$time)
  
  if(any(is.na(overlayData$time))){
    stop("'time' column has non-numeric values")
  }
  
  # Remove extra columns
  
  overlayData <-
    overlayData %>% 
    select(
      intersect(colnames(overlayData), colnames(mrgSimulations))
    )
  
  # Use ID if found in simulation data
  # Else make negative to ensure its not already used
  overlayData$ID <- NULL
  
  overlayData <-
    overlayData %>% 
    left_join(
      mrgSimulations %>% distinct(USUBJID, ID)
    ) %>% 
    mutate(
      IDfill = -as.numeric(group_indices(., USUBJID)),
      ID = if_else(is.na(ID), IDfill, ID)
    ) %>% 
    select(-IDfill)
  
  if(!any(rV$modelList$capture %in% colnames(overlayData))){
    stop(
      paste0("At least one of ",
             paste(rV$modelList$capture, collapse = ", "),
             " must be in overlay data")
    )
  }
  
  overlayData[overlayData == "."] <- NA
  
  overlayData$SIM_ID <- "user-overlay"
  overlayData$SIM_TYPE <- "User Overlay"
  overlayData$REC_TYPE <- "User Overlay"
  
  mrgSimulations %>% filter(SIM_ID != "user-overlay") %>% bind_rows(overlayData)
}