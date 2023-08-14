model_info_ui <- function(rV, input, variable, from){
  ans <- tags$ul()
  
  if(from == "modelList"){
    baseList <- rV$modelList
  } else  if(from == "input"){
    baseList <- input
  }
  
  if(variable == "modelCovariates"){
    if(length(input$modelCovariates) == 0) return(NULL)
    if(any(input$modelCovariates == "no_covariates")) return("No covariates")
  }
  
  for(i in baseList[[variable]]){
    ans <- tagAppendChild(ans, tags$li(i))
  }
  
  ans
}