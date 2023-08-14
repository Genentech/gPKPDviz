sort_and_fill_covars <- function(x){
  
  x <- x %>% arrange(ID, time, -evid)
  
  notVars <- c("USUBJID", "ID", "amt", "evid", "time", "covar_propo")
  
  covars <- colnames(x)[!(colnames(x) %in% notVars)]
  
  for(covar.i in covars){
    
    x$covarCol <- x[[covar.i]]
    
    x <- 
      x %>% 
      group_by(ID) %>% 
      tidyr::fill(covarCol, .direction = "down") %>% 
      tidyr::fill(covarCol, .direction = "up") %>%
      ungroup()
    
    x[[covar.i]] <- x$covarCol
    
    x$covarCol <- NULL
  }
  
  x
}