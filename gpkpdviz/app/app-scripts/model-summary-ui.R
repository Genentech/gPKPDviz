model_summary_ui <- function(input, rV){
  
  if(is.null(rV$modelBuild)) return(NULL)
  
  if(all(class(rV$modelBuild) == "character")){
    
    return(
      tags$span(
        style = "color:red",
        HTML(rV$modelBuild)
      )
    )
  }
  
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          title = "Model Capture",
          uiOutput("modelCapture")
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          title = "Model Compartments",
          uiOutput("modelCompartments")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          title = "Model Covariates",
          uiOutput("modelCovariatesUI")
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          title = "Model Time Unit",
          uiOutput("modelTimeUnit")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          title = "Parameters",
          DTOutput("modelSummary")
        )
      )
    )
  )
}