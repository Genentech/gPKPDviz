sim_cont_covariates_ui <- function(input, rV){
  
  cont_box_out <- function(.cov, .cat_val){
    
    if (.cat_val == "ALLCATS") {
      .clean_name <- "across the dataset"
    } else {
      .clean_name <- gsub("__", " & ", .cat_val, fixed = TRUE)
      .clean_name <- gsub(paste0(.cov, "_"), "", .clean_name, fixed = TRUE)
      .clean_name <- gsub("_", "=", .clean_name, fixed = TRUE)
      .clean_name <- glue::glue("where {.clean_name}")
    }
    
    tagList(
      box(
        width = NULL,
        status = "info",
        title = glue::glue("{.cov} {.clean_name}"),
        fluidRow(
          column(
            width = 3,
            textInput(
              inputId = glue::glue("{.cov}_{.cat_val}_mean"), 
              label = "Mean",
              value = "50"
            )
          ),
          column(
            width = 3,
            textInput(
              inputId = glue::glue("{.cov}_{.cat_val}_sd"), 
              label = "SD",
              value = "10"
            )
          ),
          column(
            width = 3,
            textInput(
              inputId = glue::glue("{.cov}_{.cat_val}_lower"),
              label = "Lower Bound",
              value = "-Inf"
            )
          ),
          column(
            width = 3,
            textInput(
              inputId = glue::glue("{.cov}_{.cat_val}_upper"),
              label = "Upper Bound",
              value = "Inf"
            )
          )
        )
      )
    )
  }
  
  if(any(input$modelCovariates == "no_covariates")) {
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
  
  for (cov.i in rV$contCovs) {
    
    split.i <-
      rV$splitCovariates[grepl(glue::glue(cov.i, "by"), rV$splitCovariates, fixed = TRUE)]
    
    if (length(split.i) > 0) {
      
      inputs.i <- gsub(pattern = glue::glue(cov.i, "by"), "", split.i, fixed = TRUE)
      
      combos_list.i <- parse_covariate_values(inputs.i, input)
      
      
      covarUI.i <- tagList(
        purrr::map(combos_list.i, ~ cont_box_out(cov.i, .x))
      )
      
    } else {
      
      covarUI.i <- tagList(cont_box_out(cov.i, "ALLCATS"))
    }
    
    
    covariateTypes <- tagAppendChild(
      covariateTypes,
      covarUI.i
    )
    
  }
  
  
  covariateTypes <- 
    fluidRow(
    column(
      width = 12,
      covariateTypes
    )
  )
  
  if(length(rV$contCovs) == 0){
    covariateTypes <- "No continuous covariates"
  }
  
  box(
    title = "Continuous Covariate Specifications",
    solidHeader = TRUE,
    width = NULL,
    status = "info",
    footer = 
      HTML(
        "<span class = 'text-right' style='font-size:16px'><i>*Continuous variables use a multivariate normal distribution. Click <a target='_blank' href='https://assessingpsyche.wordpress.com/2014/06/09/using-the-multivariate-truncated-normal-distribution/'>here</a> for more info.</i>"
      ),
    fluidRow(
      column(
        width = 12,
        covariateTypes
      )
    )
  )
}
