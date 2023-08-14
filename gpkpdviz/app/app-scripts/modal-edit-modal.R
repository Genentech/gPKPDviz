model_edit_modal <- function(rV){
  modalDialog(
    size = "l",
    title = "Model Code",
    aceEditor(outputId = "modelCodeEditor",
              value = rV$modelCode,
              mode = "c_cpp",
              readOnly = FALSE),
    footer = fluidRow(
      column(
        width = 3,
        # downloadButton(class = 'pull-left',
        #                outputId = 'downloadEditedModel',
        #                label = 'Download Edited Model'),
        icon = icon("save")
      ),
      column(
        width = 3,
        offset = 3,
        actionButton(
          inputId = "useEditModelCode",
          label = "Use Edits and Close",
          `data-dismiss`="modal",
          icon = icon("edit")
        )
      ),
      column(
        width = 3,
        offset = 0,
        tags$button(
          type="button", 
          class="btn btn-default",
          `data-dismiss`="modal",
          HTML("<i class='fa fa-close'></i>&nbsp;Close")
        )
      )
    )
  )
}