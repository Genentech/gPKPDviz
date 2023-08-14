library(mrgsolve)
library(dplyr)

uId <- function(id,x) paste0(id,"__", x)
iv_dur_condition <- function(id) {
  paste0("input.",uId(id,"admin"),"=='Infusion'")
}

no_iv_condition <- function(id) {
  paste0("input.",uId(id,"admin"),"=='Infusion' & (input.",uId(id,"evid"),"=='2' | input.",uId(id,"evid"),"=='3' | input.",uId(id,"evid"),"=='8')")
}

dosing_ui <- function(id,title,collapsed) {
  pick_evid <- selectizeInput(
    inputId = uId(id,"evid"),
    label = "Event ID (EVID)",
    choices = dosing_event_names
  )
  pick_cmt <- selectizeInput(
    inputId = uId(id,"cmt"),
    label = "Compartment",
    # selected  = 1,
    choices = NULL # c(Extravascular = 1, Intravascular = 2)
  )
  pick_admin <- selectizeInput(
    inputId = uId(id,"admin"),
    label = "Admin", 
    choices = c("Bolus","Infusion")
  )
  
  pick_evid_row <- fluidRow(column(12, pick_evid))
  
  pick_route <- fluidRow(column(6,pick_cmt),
                         column(6,pick_admin))
  
  pick_duration <- numericInput(
    inputId = uId(id,"infusion_duration"), 
    label = "Infusion Duration", 
    value = 1, min = 0.5, max = 6
  )
  pick_infusion_duration_unit <- selectizeInput(
    inputId = uId(id,"infusion_duration_unit"), 
    label = "Unit",
    choices = c("hour", "day", "week")
  )
  pick_doses <- numericInput(
    inputId = uId(id,"doses"),
    label = HTML("Dosing Amount <br><span style='font-weight:normal;font-size:12px;'></span>"),
    value = 100,
    step = 25
  )
  pick_n_doses <- numericInput(
    inputId = uId(id,"n_doses"),
    label = "Number of Doses",
    value = 3,
    step = 1,
    min = 1
  )
  pick_interval_value <-  numericInput(
    inputId = uId(id,"dosing_interval"),
    label = "Dosing Interval",
    value = 24,
    min = 1,
    step = 1
  )
  pick_interval_unit <-  selectizeInput(
    inputId = uId(id,"dosing_interval_unit"),
    label = "Interval Unit",
    choices = c("hour", "day", "week")
  )
  
  pick_infusion_dir <- fluidRow(column(width = 6, pick_duration),
                                column(width = 6, pick_infusion_duration_unit))
  
  pick_interval <- fluidRow(column(6,pick_interval_value), 
                            column(6,pick_interval_unit))
  
  pick_dosing_dur_value <-  numericInput(
    inputId = uId(id,"dosing_duration"),
    label = "Dosing Duration",
    value = 3,
    min = 0
  )
  pick_dosing_dur_unit <-  selectizeInput(
    inputId = uId(id,"dosing_duration_unit"),
    label = "Unit",
    choices = c("day", "week")
  )
  pick_dosing_dur <- fluidRow(column(6,pick_dosing_dur_value), 
                              column(6,pick_dosing_dur_unit))
  
  pick_percovar <- selectInput(
    inputId=uId(id,"percovar"), 
    label="Amount in Proportion to Covariate",
    choices = c("None (flat dosing)" = "none")
  )
  
  # n_dose_summary <- uiOutput(paste0(id,"NDoses"))
  
  #box(
    #title=title,
    #width=NULL, collapsible=F,# collapsed=collapsed,
    #solidHeader = FALSE,#status="info",
  fluidRow(
    column(
      width = 12,
    fluidRow(column(width = 6,pick_n_doses)),  
    fluidRow(column(width = 6,pick_doses), column(width = 6,pick_percovar)),
    pick_interval,
    # pick_dosing_dur,
    pick_evid_row,
    pick_route,
    conditionalPanel(condition=iv_dur_condition(id),pick_infusion_dir),
    conditionalPanel(condition = no_iv_condition(id), tags$div(class="redselect alert", "Infusion not implemented for EVID: 2, 3, 8"))#,
    # n_dose_summary
  )
  )
}

dosing <- R6Class("dosing",
                  public = list(
                    name = NULL,
                    cmt = 1,
                    admin = 2,
                    doses = c(100,200,300),
                    infusion_duration  = 1,
                    infusion_duration_unit = "hour",
                    duration = 7,
                    duration_unit = "day",
                    interval = 24,
                    interval_unit = "hour",
                    percovar = "none",
                    evid = 1,
                    n_doses = 0,
                    initialize = function(name) {
                      self$name <- name
                    }, 
                    make_ui = function(title,collapsed=FALSE) {
                      dosing_ui(self$name,title,collapsed)
                    },
                    inputs = function() {
                      uId(self$name, c("cmt", "admin", "doses", "dosing_interval", "percovar",
                                       "dosing_duration", "dosing_duration_unit", "dosing_interval_unit",
                                       "infusion_duration", "infusion_duration_unit", "evid", "n_doses"))
                    }, 
                    get_input = function(input) {
                      input <- reactiveValuesToList(input)
                      x <- input[self$inputs()]
                      self$cmt               <- as.integer(x[[1]])
                      self$admin              <- as.character(x[[2]])
                      self$doses             <- (x[[3]])
                      self$interval          <- as.numeric(x[[4]])
                      self$percovar          <- as.character(x[[5]])
                      self$duration          <- as.numeric(x[[6]])
                      self$duration_unit     <- x[[7]]
                      self$interval_unit     <- x[[8]]
                      self$infusion_duration <- as.numeric(x[[9]])
                      self$infusion_duration_unit <- x[[10]]
                      self$evid              <- as.integer(x[[11]])
                      self$n_doses           <- as.integer(x[[12]])
                    }
                  )
)

dosing_period <- function(d,input,ids,period=1,idata,e, dose_start_time) {
  
  ii <- to_model_time(d$interval,d$interval_unit,input)
  
  # dur <- to_model_time(d$duration,d$duration_unit,input)
  
  # addl <- floor(dur/ii)-1
  addl <- d$n_doses - 1
  x <- expand.ev(ID=ids,amt=d$doses,ii=ii,cmt=d$cmt,addl=addl,evid = d$evid) 
  # if(d$cmt=="Intravascular") {
  #   x <- mutate(x,cmt=2) 
  # }
  x <- dplyr::mutate(x,dose=amt,period=period,time=dose_start_time)
  if(all(x$addl == 0)){
    x <- x %>% select(-addl, -ii)
  } else {
    x <- x %>% realize_addl() %>% select(-addl, -ii)
  }
  
  
  if(d$percovar != "none") {
    
    covarData <- 
    idata %>%
      group_by(ID) %>% 
      filter(time==min(time)) %>% 
      ungroup() %>% 
      select(!!c("ID", as.character(d$percovar))) %>% 
      distinct() %>% 
      as.data.frame()
    
    covarData$covarMult <- covarData[[as.character(d$percovar)]]
    covarData[[as.character(d$percovar)]] <- NULL
    
    x <- 
      x %>% 
      left_join(covarData) %>%
      mutate(amt = amt*covarMult) %>%
      select(-covarMult) %>%
      as.data.frame()
  }
  # if(d$admin=="Infusion") {
  #   x <- dplyr::mutate(x,rate = amt/d$infusion_duration)
  # }
  if((d$admin=="Infusion") & (d$evid %in% c(1, 4))) {
    infusion_use <- to_model_time(d$infusion_duration,d$infusion_duration_unit,input)
      x <- dplyr::mutate(x,rate = amt/infusion_use)
  } else {
    x <- dplyr::mutate(x,rate = 0)
  }
  # e$start <- e$start + dur
  x
}

dosing_sequence <- function(input,d1,d2,d3,p1,p2,p3,nid=30,idata) {
  # message("***################**** NEW dosing_sequence ***################****")
  e <- new.env()
 #  e$start <- 0
  
  d1$get_input(input)
  d2$get_input(input)
  d3$get_input(input)
  p1$get_input(input)
  p2$get_input(input)
  p3$get_input(input)
  
  periods <- c(p1$period,p2$period,p3$period)
  active <- periods > 0
  periods <- periods[active]
  dose_start_time <- c(p1$dose_start_time,p2$dose_start_time,p3$dose_start_time)
  dose_start_time <- dose_start_time[active]
  unit <- c(p1$dose_start_time_unit,p2$dose_start_time_unit,p3$dose_start_time_unit)
  unit <- unit[active]
  # message(paste(dose_start_time, collapse = " - "))
  start <- 0
  dosing_data <- vector("list",length(periods))
  
  for(i in seq_along(periods)) {
    # message("******* NEW I *******")
    # message(paste0("i = ", i))
    # message(paste0("periods[i] = ", periods[i]))

    if(periods[i]==1) {
      d <- d1
     # p.i <- 1
    }
    if(periods[i]==2) {
      d <- d2
    #  p.i <- 2
    }
    if(periods[i]==3) {
      d <- d3
     # p.i <- 3
    }
    # message(paste0("d = ", d))
    
    
    dose_start_time.i <- to_model_time(dose_start_time[i],unit[i],input)
    # message(paste0("dose_start_time.i ", dose_start_time.i))
    a <- dosing_period(d,input,ids=1:nid,period=i,idata=idata,e, dose_start_time.i)
    a$period_label <- periods[i]
    a$covar_propo <- d$percovar
    dosing_data[[i]] <- a
    #e$start <- e$start + dose_start_time[i]
    # if (!checked default){
   #  e$start <- dose_start_time.i
    # }
    rm(dose_start_time.i)
    #rm(p.i)
  }
  
  d <- bind_rows(dosing_data) %>% dplyr::arrange(ID,period,time)
  
  d <- bind_dose_and_pop(d, idata)
  
  return(d)
  
}
