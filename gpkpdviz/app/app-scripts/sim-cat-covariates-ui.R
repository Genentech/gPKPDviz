sim_cat_covariates_ui <- function(input, rV){
  
  if (any(input$modelCovariates == "no_covariates")) {
    return(
      tagList(
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br(),
        tags$span(
          class = "redselect alert", "Remove 'No Covariates' from list of covariates"
        )
      )
    )
  }
  
  covariateTypes <- tagList()

  
  for (cov.i in rV$catCovs) {
      
      .CovarUI <-
        fluidRow(
          column(
            width = 6,
            textInput(
              inputId = paste0(cov.i, "cats"),
              label = HTML("Categories <br> <i>(comma separated)</i>"),
              value = c("0, 1"),
              placeholder = "A, B, C"
            )
          ),
          column(
            width = 6,
            textInput(
              inputId = paste0(cov.i, "probs"),
              label = HTML("Ratios <br> <i>(comma separated)</i>"),
              value = c(".5, .5"),
              placeholder = ".2, .1, .7"
            )
          )
        )
    
    covariateTypes <- tagAppendChild(
      covariateTypes,
      tagList(
        box(
          width = NULL,
          status = "warning",
          title = paste(cov.i, " (Categorical)"),
          fluidRow(
            column(
              width = 12,
              .CovarUI
            )
          )
        )
      )
    )
  }
  

  
  if(length(rV$catCovs) == 0){
    covariateTypes <- "No categorical covariates"
  }
  
  box(
    title = "Categorical Covariate Specifications",
    solidHeader = TRUE,
    width = NULL,
    status = "warning",
    fluidRow(
      column(
        width = 12,
        covariateTypes
      )
    )
  )
}