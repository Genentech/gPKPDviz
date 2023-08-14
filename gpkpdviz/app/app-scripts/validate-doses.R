validate_doses <- function(input){
  
  check_numeric <- function(x){
    if(is.null(x)) return(FALSE)
    ans <- as.numeric(x)
    length(ans[!is.na(ans)]) > 0
  }
  
  check_has_chars <- function(x){
    if(is.null(x)) return(FALSE)
    nchar(x) > 0
  }
  
  toCheckNumeric <- c(
    "dosing_interval", 
    "dosing_duration"
  )
  
  toCheckCharacter <- c(
    "dosing_interval_unit",
    "dosing_duration_unit", 
    "evid",
    "cmt",
    "admin"
  )
  
  infusionEVID238 <- c()
  validateDosesNumeric <- c()
  validateDosesChars <- c()
  
  for(i in c("first", "second", "third")){
    
    infusionCheck.i <-
      !(
        (input[[paste0(i, "__evid")]] %in% c(2, 3, 8)) &
        (input[[paste0(i, "__admin")]] == 'Infusion')
        )
    
    infusionEVID238 <- c(infusionEVID238, infusionCheck.i)
    
    for(numeric.i in toCheckNumeric){
      
      validateDosesNumeric <- 
        c(validateDosesNumeric, check_numeric(input[[paste0(i, "__", numeric.i)]])) 
    }
    
    for(char.i in toCheckCharacter){
      
      validateDosesChars <- 
        c(validateDosesChars, check_has_chars(input[[paste0(i, "__", char.i)]])) 
    }
    
  }
  
  all(infusionEVID238) & all(validateDosesNumeric) & all(validateDosesChars)
}