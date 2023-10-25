model_complete_modal <- function(rV, startSelected, timeStartSelected){
  modalDialog(
    # size = "m",
    title = 
      tagList(
        tags$span(class = "badge label-success", icon("check")),
        " Model Build Success"
      ),
    #tags$h4(tags$b("Some additional information required")),
    #tags$br(),
    tags$br(),
    tags$br(),
    fluidRow(
      column(
        width = 12,
        tabBox(width = NULL,
               tabPanel(
                 "Finalize Setup",
                 # tags$div(
                 #  class = "container",
                 fluidRow(
                   column(
                     width = 12,
                     selectInput(
                       inputId = "modelCovariates",
                       label = "Model Covariates",
                       choices = c("No covariates" = "no_covariates", sort(names(rV$modelParam))),
                       multiple = TRUE,
                       selected = startSelected,
                       width = "100%"
                     ),
                     tags$i("*Covariates will be represented as columns in the population data")
                   )
                 ),
                 tags$br(),
                 fluidRow(
                   column(
                     width = 12,
                     selectizeInput(
                       inputId = "model_time", 
                       label = "Model Time Unit", 
                       choices = c("hour", "day", "week"),
                       selected = timeStartSelected,
                       width = "100%"
                     )
                   )
                 ),
                 conditionalPanel(
                   "input.modelCovariates != 'no_covariates'",
                   uiOutput("covariateTypeSelectionUI")
                 )
               ),
               tabPanel("Model Code (for reference)",
                        aceEditor(outputId = "modelCodeModelCompleteModal",
                                  value = rV$modelCode,
                                  mode = "c_cpp",
                                  height = "400px",
                                  readOnly = TRUE)
               )
        )
      )
    ), 
    footer = 
      conditionalPanel(
        "input.modelCovariates",
        actionButton(class = 'btn-lg',
                     `data-dismiss` = "modal",
                     inputId = "modCovarClose",
                     label = "Done",
                     icon = icon("ok", lib = "glyphicon"))
      ),
    conditionalPanel(
      "!input.modelCovariates",
      tags$i(class = 'pull-right', style='color:red', "Select Covariates Before Proceeding")
    )
  )
}