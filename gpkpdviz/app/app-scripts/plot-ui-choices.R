plot_ui_choices <- function(rV){
  
  ans <- list()
  
  # Y-Axis Variable ---------------------------------------------------------
  plotDataVarChoices <- rV$modelList$capture
  names(plotDataVarChoices) <- paste(plotDataVarChoices, "(Model Capture)")
  plotDataVarSelected <- plotDataVarChoices
  
  ans$plotDataVarChoices <- plotDataVarChoices
  ans$plotDataVarSelected <- plotDataVarSelected[1]
  
  
  # Summarize By Variable ---------------------------------------------------
  summarizeVariables <- rV$summarizeVariables
  names(summarizeVariables) <- summarizeVariables
  plotDataSummarizeChoices <- summarizeVariables
  
  if(!is.null(rV$catCovs)){
    if(length(rV$catCovs) > 0){
      cat_vars <- rV$catCovs
      names(cat_vars) <- paste(cat_vars, "(Categorical Covariate)")
      plotDataSummarizeChoices <- c(plotDataSummarizeChoices, cat_vars)
    }
  }
  
  ans$plotDataSummarizeChoices <- plotDataSummarizeChoices
  ans$plotDataSummarizeSelected <- "SIM_ID"
  
  
  # Color By Variable -------------------------------------------------------
  plotDataColorChoices <- ans$plotDataSummarizeChoices
  
  if(!is.null(rV$contCovs)){
    if(length(rV$contCovs) > 0){
      cont_vars <- rV$contCovs
      names(cont_vars) <- paste(cont_vars, "(Continuous Covariate)")
      plotDataColorChoices <- c(plotDataColorChoices, cont_vars)
    }
  }
  
  ans$plotDataColorChoices <- c("None" = "no_color", plotDataColorChoices)
  ans$plotDataColorSelected <- "SIM_TYPE"
  
  ans
}


