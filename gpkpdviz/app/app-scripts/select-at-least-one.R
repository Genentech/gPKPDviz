select_at_least_one <- function(session, input, input_id, choices, label){
  
  if (is.null(input[[input_id]])) {
    
    updateCheckboxGroupInput(
      session = session, 
      inputId = input_id, 
      selected = choices
    )
    
    showNotification(
      ui = paste0("At least one '", label, "' must be selected"),
      duration = 7,
      type = "warning",
      session = session
    )
    
  }
}
