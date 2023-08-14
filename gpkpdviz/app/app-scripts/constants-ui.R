constants_ui <- function(input, rV){
  if(!input$viewModelSummary) return(NULL)
  maxLen <- 11
  modelNamePrint <- ifelse(nchar(rV$modelName) > maxLen,
                           paste0(substr(rV$modelName, 1, maxLen), "..."), rV$modelName)
  
  covarSidebar <- tags$ul()
  
  if(length(rV$catCovs) != 0){
    catCovarNames <- paste(rV$catCovs, " (Categorical)")
  } else{
    catCovarNames <- NULL
  }
  
  if(length(rV$contCovs) != 0){
    contCovarNames <- paste(rV$contCovs, " (Continuous)")
  } else{
    contCovarNames <- NULL
  }
  
  covarNames <- c(catCovarNames, contCovarNames)
  
  if(length(covarNames) > 0){
    
    for(i in covarNames){
      covarSidebar <- tagAppendChild(covarSidebar, tags$li(i))
    }
    
  } else {
    covarSidebar <- HTML("None<br>")
  }
  
  showPop <- FALSE
  if(!is.null(rV$nPopIDs)){
    if(rV$nPopIDs > 0) showPop <- TRUE
  }
  
  showDose <- FALSE
  if(!is.null(rV$nDoseDisplaySidebar)) {
    showDose <- TRUE
  }
  
  tagList(
    tags$h3(tags$u("Input Summary")),
    HTML(paste0("<b>Model:</b> ", modelNamePrint, "")),
    tags$br(),
    tags$br(),
    HTML(paste0("<b>Model Time Unit: </b>", input$model_time, "")),
    tags$br(),
    tags$br(),
    HTML(paste0("<b>Model Covariates: </b>", covarSidebar, "")),
    tags$br(),
    HTML(ifelse(!showPop,
                "",
                paste0("<b>Number of Subjects: </b>", rV$nPopIDs))),
    tags$br(),
    tags$br(),
    HTML(ifelse(!showDose,
                "",
                paste0("<b>Number of Doses: </b>", rV$nDoseDisplaySidebar,"</b>")))
  )
}