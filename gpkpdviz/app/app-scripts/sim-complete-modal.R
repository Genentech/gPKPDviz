sim_complete_modal <- function(rV, input){
  modalDialog(
    title = 
      tagList(
        tags$span(class = "badge label-success", icon("check")),
        " Simulation Successful"
      ),
    tagList(
      fluidRow(
        column(
          width = 8,
          class = 'text-right', 
          tags$span(style="font-size:18px", tags$i("Simulation ID:"))
        ),      
        column(
          width = 4,
          tags$span(style="font-size:15px", tags$b(input$sim_id))
        )
      ),
      tags$br(),
      fluidRow(
        column(
          width = 8,
          class = 'text-right', 
          tags$span(style="font-size:18px", tags$i("Number of subjects (IDs): "))
        ),
        column(
          width = 4,
          tags$span(style="font-size:15px", tags$b(length(unique(rV$genpkSim$ID))))
        )
      ),
      tags$br(),
      fluidRow(
        column(
          width = 8,
          class = 'text-right', 
          tags$span(style="font-size:18px", tags$i(paste0("Simulation time frame in model time unit (", input$model_time, "s): ")))
        ), 
        column(
          width = 4,
          tags$span(style="font-size:15px", 
                    tags$b(min(rV$genpkSim$time, na.rm = TRUE), 
                           " - ", max(rV$genpkSim$time, na.rm = TRUE)))
        )
      )
    ),
    footer = actionButton(`data-dismiss` = "modal",
                          inputId = "modSuccessClose",
                          label = "Dismiss")
  )
}