
uId <- function(id,x) paste0(id,"__", x)

period_ui <- function(id,label,selected) {
  pick_period <- selectizeInput(
    inputId = uId(id,"period"),
    label = "Intervention", 
    # choices = c(`Not used` = 0, `Period A` = 1, `Period B` =2, `Period C`=3), 
    choices = c(`Not used` = 0, `A` = 1, `B` =2, `C`=3), 
    selected = selected
  )
  pick_dose_start_time <- numericInput(
    inputId = uId(id,"dose_start_time"),
    label = HTML("Start time"), #</br><i>*Relative to end of 'First Intervention'</i>"), 
    min = 0, value = 0
  )
  pick_dose_start_time_unit <- selectizeInput(
    inputId = uId(id,"dose_start_time_unit"),
    label = "Start time unit", 
    selected = "day",
    choices = c("hour", "day", "week")
  )
  
  interventionSelects <- tagList(fluidRow(column(12,pick_period)))
   
  if(label != "First Intervention"){
    
    if(label == "Second Intervention"){
      prevText <- "First"
    } else {
      prevText <- "Second"
    }
    interventionSelects <- tagAppendChildren(
      interventionSelects,
      conditionalPanel(
        condition = paste0("input.", uId(id,"period"), ' > 0'),
        fluidRow(column(12,pick_dose_start_time)), fluidRow(column(12,pick_dose_start_time_unit)),
        tagList(tags$br(),
                tags$p(id = paste0(prevText, "PrevText"), "")
        )
      )
    )
    
  
   } else {
     interventionSelects <- tagAppendChildren(interventionSelects,
                                              fluidRow(column(12,pick_dose_start_time)),
                                              conditionalPanel(
                                                condition = "2==1",fluidRow(column(12,pick_dose_start_time_unit))))
     
     
     interventionSelects <- tagAppendChild(interventionSelects,
                                           tagList(tags$br(),
                                                   tags$p(HTML("*<b>First intervention</b> starts at time = 0"))))
   }
  
  
  
  box(width = NULL, title = label, interventionSelects)
}

period <- R6Class("period",
                  public = list(
                    name = NULL,
                    period = NULL,
                    dose_start_time = 0,
                    dose_start_time_unit = "hour",
                    initialize = function(name,period) {
                      self$name <- name
                      self$period <- period
                    }, 
                    make_ui = function(label,selected) {
                      period_ui(self$name,label,selected)
                    },
                    inputs = function() {
                      uId(self$name, c("period", "dose_start_time", "dose_start_time_unit"))
                    }, 
                    get_input = function(input) {
                      input <- reactiveValuesToList(input)
                      x <- input[self$inputs()]
                      self$period <- as.integer(x[[1]])
                      self$dose_start_time   <- as.numeric(x[[2]])
                      self$dose_start_time_unit <- as.character(x[[3]])
                    }
                  )
)






