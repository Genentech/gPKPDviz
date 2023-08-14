dosing_schedule_upload_ui <- function(input, rV){
  
  
  if(length(rV$contCovs)>0){
    covarPropoUL <-
      tags$ul(
        tags$h5(tags$b("covar_propo")),
        tags$li("Covariate Dosing is in Proportion To (covariate name)"),
        tags$li("character"),
        tags$li(paste(rV$contCovs, sep = "", collapse = ", ")),
        tags$li("Use 'none' for flat dosing"),
        tags$li("If missing: Assume 'none' (flat dosing)")
      )
  } else {
    covarPropoUL <- div()
  }
  
  simpleDF <-
    data_frame(
      amt = as.character(100),
      evid = as.character(1),
      cmt = as.character(rV$cmtChoices[1]),
      time = as.character(to_model_time(c(0, 1, 2), "day", input))
    )
  
  simpleExample <- 
    column(
      width = 6,
      box(
        width = NULL,
        status = "warning",
        collapsible = TRUE,
        collapsed = TRUE,
        title = "Apply to All Subjects Example",
        renderTable({simpleDF})
      )
    )
  
  if(!is.null(rV$populationData)){
    
    bySubjDF <- data_frame()
    
    for(i in rV$populationData$USUBJID[1:3]){
      
      if(is.na(i)) next
      
      bySubjDF <- 
        bind_rows(
          bySubjDF,
          simpleDF %>% mutate(USUBJID = i) %>% select(USUBJID, everything())
        )
      
    }
    
    bySubjExample <- 
      column(
        width = 6,
        box(
          width = NULL,
          status = "warning",
          collapsible = TRUE,
          collapsed = TRUE,
          title = "By Subject Example",
          renderTable({bySubjDF})
        )
      )
    
  } else {
    bySubjExample <- div()
  }
  
  box(
    width = NULL,
    title = "Upload Dosing Schedule",
    status = "primary",
    solidHeader = TRUE,
    fluidRow(
      column(
        width = 12,
        fileInput(
          inputId = "doseUploadData", 
          label = "Upload file (csv)",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        DTOutput("uploadedSchedule")
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          title = "Required Columns",
          status = "primary",
          tags$ul(
            tags$h5(tags$b("amt")),
            tags$li("Dose Amount"),
            tags$li("numeric")
          ),
          tags$ul(
            tags$h5(tags$b("evid")),
            tags$li("Event ID"),
            tags$li("numeric"),
            tags$li("1, 3, 4, or 8 are interpreted as dose type events"),
            tags$li("0 or 2 are interpreted as sampling time points to be used in the simulation"),
          ),
          tags$ul(
            tags$h5(tags$b("cmt")),
            tags$li("Dose Compartment"),
            tags$li("numeric"),
            tags$li(paste(names(rV$cmtChoices), rV$cmtChoices, sep = "=", collapse = ", "))
          ),
          tags$ul(
            tags$h5(tags$b("time")),
            tags$li("Dose Time"),
            tags$li("numeric"),
            tags$li("Unit matches 'Model Time Unit'")
          )
        )
      ),
      column(
        width = 6,
        box(
          width = NULL,
          title = "Optional Columns",
          status = "primary",
          tags$ul(
            tags$h5(tags$b("USUBJID")),
            tags$li("Unique subject identifier"),
            tags$li("numeric or character"),
            tags$li("If missing: Assume schedule is an example for one subject"),
            tags$li("If found: All subjects in population data must be included in dosing schedule")
          ),
          tags$ul(
            tags$h5(tags$b("rate")),
            tags$li("Rate of dose"),
            tags$li("numeric"),
            tags$li("User zero (0) for bolus"),
            tags$li("If missing: Assume zero (bolus)"),
            tags$li("Unit is the dosing amount unit (in model time)",
                    tags$ul("Example:",
                      tags$li("Model Time Unit = Hour"),
                      tags$li("Dose Amount = 75"),
                      tags$li("If 1.5 Hours is desired Infusion Duration, rate = 50 (75 / 1.5)")
                    ))
          ),
          covarPropoUL
        )
      )
    ),
    fluidRow(
      simpleExample,
      bySubjExample
    )
  )
  
  
}