upload_pop_ui <- function(popDataType, modelTime, modelCovariates){
  
  if(popDataType != 'uploadPop') return(div())
  
  colList <- tags$ul()
  
  colList <- tagAppendChild(
    colList,
    tags$li(tags$span(style = "color:blue", "Comma separated (.csv) file"))
  )
  colList <- tagAppendChild(
    colList, 
    tags$li(tags$span(style = "color:blue", "Necessary columns: ",
                      tags$b("USUBJID,"),
                      tags$b("time,"),
                      tags$b(paste(modelCovariates, collapse = ", "))))
  )
  colList <- tagAppendChild(
    colList,
    tags$li("All columns not in the list of ", tags$b('Necessary columns'), " are dropped")
  )
  
  colList <- tagAppendChild(
    colList,
    tags$li(tags$b("ID"), "column will be auto-generated based on ",  tags$b("USUBJID"))
  )
  
  colList <- tagAppendChild(
    colList,
    tags$li("If your covariate data is not time-varying, create a ", tags$b('time'), " column with all values of 0 (zero)")
  )
  
  tagList(
    fileInput(
      inputId = "uploadedPopData",
      label = "Choose CSV File",
      accept = ".csv",
      width = "100%"
    ),
    tags$h3(tags$u("Data Upload Criteria:")),
    colList
  )
  
}