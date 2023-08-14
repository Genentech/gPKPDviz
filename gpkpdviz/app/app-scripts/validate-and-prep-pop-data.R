validate_and_prep_pop_data <- function(.pop_data, .model_covariates, .build_id = TRUE){
  # .pop_data <- read.csv("PK-Population-Data-2019-04-10.csv", stringsAsFactors = F)
  # .model_covariates <- c("CAT", "CONT", "WT")
  req_cols <- c("USUBJID", "time", .model_covariates)
  cols_found <- req_cols %in% colnames(.pop_data)
  
  if(!all(cols_found)){
    stop(
      paste0(
        "Population data input error - Columns '",
        paste(req_cols[!cols_found], collapse = ", ", sep = ", "),
        "' not found in data"
      )
    )
  }
  
  finite_check <-
    lapply(.pop_data[,c("time", .model_covariates)], function(x){
      any((is.na(x)) | (nchar(x, keepNA = FALSE) == 0))
      })
  
  if(any(unlist(finite_check))){
    stop(
      paste0(
        "Missing Values Found In ",
        names(finite_check[unlist(finite_check)][1])
      )
    )
  }
  
  if(.build_id){
    .pop_data <- .pop_data %>% mutate(ID = group_indices(.data = ., USUBJID))
  }
  .pop_data <- .pop_data %>% select(c("ID", req_cols))
  .pop_data %>% select(USUBJID, ID, time, everything()) %>% arrange(ID, time)
  
}