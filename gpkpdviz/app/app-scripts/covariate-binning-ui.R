covariate_binning_ui <- function(contCovs){
  tagList(
    fluidRow(
      column(
        width = 6,
        radioButtons(
          inputId = "continuousCovariatesToBin",
          label = "Select Covariate",
          choices = contCovs,
          inline = TRUE
        )
      ),
      column(
        width = 6,
        radioButtons(
          inputId = "continuousCovariatesBinType",
          label = "Method",
          choices = c("Define Number of Bins" = "n_bins",
                      "Define Break Points" = "breaks"),
          inline = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        conditionalPanel(
          "input.continuousCovariatesBinType == 'n_bins'",
          numericInput(
            inputId = "continuousCovariatesNBins",
            label = "Number of Bins",
            value = 4,
            min = 2,
            max = Inf,
            step = 1
          )
        ),
        conditionalPanel(
          "input.continuousCovariatesBinType == 'breaks'",
          textInput(
            inputId = "continuousCovariatesBreaks",
            label = HTML("Breaks (Comma Separated)<br>*min and max auto-included"),
            value = "", 
            placeholder = "5, 10, 50"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(
          class = "btn-lg btn-block",
          inputId = "continuousCovariatesBinButton",
          label = "Create",
          icon = icon("gavel")
        )
      )
    ),
    tags$br(),
    tags$p("*These columns will be used subsequently for grouping/visualizations"),
    tags$br(),
    tags$p("*If covariate is time-varying, subjects will get binned based on their first observation")
  )
}