validate_dose_upload <- function(.dose_upload, .pop_data){
  
  if(is.null(.pop_data)) return(.dose_upload)
  
  for(i in c("amt", "evid", "cmt", "time")){
    if(!(i %in% colnames(.dose_upload))) {
      stop(paste0("Column ", i, " not found in data"))
    }
  }
  
  .sampling_upload <- .dose_upload %>% filter(evid %in% c(0, 2))
  .dose_upload <- .dose_upload %>% filter(!(evid %in% c(0, 2)))
  
  if(nrow(.sampling_upload) == 0){
    .sampling_upload <- NULL
  }
  
  if(length(unique(.dose_upload$cmt)) > 1){
    stop("Uploaded dosing data must have only one unique value for 'cmt'")
  }
  
  if(("covar_propo" %in% colnames(.dose_upload))) {
    if(length(unique(.dose_upload$covar_propo)) > 1){
      stop("Uploaded dosing data must have only one unique value for 'covar_propo'")
    }
    
    if(!unique(.dose_upload$covar_propo) %in% colnames(.pop_data)){
      stop(
        paste0(
          "covar_propo '", unique(.dose_upload$covar_propo), "' not found in population data"
        )
      )
    }
    
    
  }
  
  if(("USUBJID" %in% colnames(.dose_upload))) {
    
    test_subj <- .pop_data$USUBJID %in% .dose_upload$USUBJID
    
    if(!all(test_subj)){
      stop("Uploaded dosing data missing USUBJIDs found in population data")
    }
    
  }
  
  ans <- list()
  ans$doses <- .dose_upload
  ans$samplingTimes <- .sampling_upload
  
  ans
  
}