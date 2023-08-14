genpk_try <- function(session, .orig_obj, .action, .success_message = NULL, .error_notification_id = ""){
  
  action_try <- try(.action)
  
  if(all(class(action_try) == "try-error")){
    
    showNotification(
      session = session,
      duration = NULL,
      ui = HTML(attributes(action_try)$condition$message),
      type = "error",
      id = .error_notification_id
    )
    
    return(.orig_obj)
    
  } else {
    
    removeNotification(session, id = .error_notification_id)
    
    if(!is.null(.success_message)){
      showNotification(
        session = session,
        ui = .success_message,
        closeButton = FALSE,
        duration = 4,
        type = "message"
      )
    }
    
    return(action_try)
    
  }
  
}
