# create_dose_summary_text <- function(dose_summary, which_dose, prior_dose, cmt_choices){
#   
#   which_period <- 
#     dose_summary$Value[dose_summary$Var == "period" & 
#                          dose_summary$Dose == which_dose]
#   
#   if(which_period == "0"){
#     return(
#       HTML(
#         "<span style = 'font-size:18px'>",
#         "Not Used",
#         "</span>"
#       )
#     )
#   }
#   
#   which_dose_use <- case_when(
#     which_period == "1" ~ "first",
#     which_period == "2" ~ "second",
#     which_period == "3" ~ "third"
#   )
#   
#   doseEvents <- filter(dose_summary, Dose == which_dose_use) %>% select(-Dose)
#   doseEventsList <- as.list(doseEvents$Value)
#   names(doseEventsList) <- doseEvents$Var
#   
#   
#   
#   waiting_period <- ""
#   
#   if(prior_dose != "none"){
#     
#     priorDoseEvents <- filter(dose_summary, Dose == prior_dose) %>% select(-Dose)
#     priorDoseEventsList <- as.list(priorDoseEvents$Value)
#     names(priorDoseEventsList) <- priorDoseEvents$Var
#     
#     # if(priorDoseEventsList$wash != "0"){
#     #   waiting_period <- 
#     #     paste0(
#     #       "Wait ",
#     #       "<span style='font-weight:bold;color:#3c8dbc'>", priorDoseEventsList$wash,
#     #       " ", priorDoseEventsList$wash_unit, "(s)", "</span> ",
#     #       "then:",
#     #       "<br><br>"
#     #     )
#     # }
#   }
#   
#   main_text <- 
#     paste0(
#       "Administer dose(s) of ", 
#       "<span style='font-weight:bold;color:#3c8dbc'>", gsub(" ", ", ", doseEventsList$doses), "</span> ",
#       "via EVID ", 
#       "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$evid, "</span> ",
#       ifelse(doseEventsList$percovar != "none", "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$percovar, "</span>", ""),
#       "every ", 
#       "<span style='font-weight:bold;color:#3c8dbc'>",doseEventsList$dosing_interval, "</span> ",
#       "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$dosing_interval_unit, "(s)", "</span> ",
#       "for ", 
#       "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$dosing_duration, "</span> ",
#       "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$dosing_duration_unit, "(s)", "</span> ", 
#       "in compartment ",
#       # ifelse(doseEventsList$cmt == "1", "<span style='font-weight:bold;color:#3c8dbc'>Extravascular</span> ", "<span style='font-weight:bold;color:#3c8dbc'>Intravascular</span> "),
#       "<span style='font-weight:bold;color:#3c8dbc'>", names(cmt_choices)[cmt_choices == doseEventsList$cmt], "</span> ",
#       "administered via ",
#       "<span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$admin, "</span>",
#       ifelse(doseEventsList$admin == "Infusion", 
#              paste0(" with an infusion duration of <span style='font-weight:bold;color:#3c8dbc'>", doseEventsList$infusion_duration, " hour(s)</span>"),
#              ""
#       ),
#       "."
#     )
#   
#   HTML(
#     "<span style = 'font-size:18px'>",
#     waiting_period,
#     main_text,
#     "</span>"
#   )
# }



render_n_dose_table <- function(nDosesTable){
  
  nDosesTable %>% 
    DT::datatable(., 
                  escape = FALSE,
                  rownames = FALSE,
                  options=list(pageLength = nrow(nDosesTable),
                               dom = "t",
                               ordering = FALSE,
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))))
}
