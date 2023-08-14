build_model <- function(rV){
  
  ans <- mcode(model = "app-code", code = rV$modelCode, recover = TRUE)
  
  if (inherits(ans, "mrgmod")) {
    
    ans <- ans %>% update(., request = "(all)")
    
  } else {
    
    stop(paste(ans$out$stderr, collapse = "<br>"))
    
  }
  
  ans
}
