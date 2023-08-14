function(input, output, session){
  
  observeEvent(input$sessionInfoButton, {
    
    session_info <- sessionInfo()
    
    loaded_pkg <-
      purrr::map_df(
        names(session_info$loadedOnly), 
        ~ tibble::tribble(~Package, ~Version, .x, session_info$loadedOnly[[.x]]$Version)
      ) %>% 
      arrange(Package)
    
    showModal(
      modalDialog(size = "l",
        fluidRow(
          column(
            width = 4,
            renderTable(
              tibble::tribble(
                ~`System Info`,
                session_info$R.version$version.string,
                session_info$running
              )
            )
          ),
          column(
            width = 8,
            DT::renderDT(loaded_pkg, rownames=F)
          )
        )
        )
      )
    
  })
  
  # Global Variables --------------------------------------------------------
  rV <- reactiveValues()
  
  
  output$constantsUI <- renderUI({
    constants_ui(input, rV)
  })
  
  observeEvent(input$setSeed, {
    rV$seed <- input$appSeed
  })
  
  observeEvent(rV$seed, {
    if(input$setSeed > 0){
      
      showNotification(
        session = session,
        ui = paste0("Seed updated to: ", rV$seed),
        type = "message"
      )
    }
  })
  
  observeEvent(input$appSeedType, {
    
    if(input$appSeedType == "Random"){
      rV$seed <- "Random"
    }
    
  })
  
  
  # Model Tab ---------------------------------------------------------------
  
  # Action to take when user select a model from library or uploads a model
  observe({
    
    if(input$modelLoadType == "useLibrary"){
      if(input$modelSelection == ""){
        return(NULL)
      }
      
      rV$inputCode <- 
        paste(
          readLines(paste0("models-library/", input$modelSelection, ".cpp")),
          collapse = "\n"
        )
      
      rV$modelName <- input$modelSelection
      
    } else if(input$modelLoadType == "uploadModel"){
      
      if(is.null(input$modelUploadFile)){
        return(NULL)
      }
      
      modelInFile <- input$modelUploadFile
      
      rV$inputCode <-
        paste(
          readLines(modelInFile$datapath), 
          collapse = "\n"
        )
      
      rV$modelName <- gsub(pattern = ".cpp", replacement = "", x = modelInFile$name)
    }
    
  })
  
  output$modelName <- renderUI({
    tags$i(tags$h5(rV$modelName))
  })
  
  observe({
    updateAceEditor(session,
                    editorId = "modelCodeEditor",
                    value = rV$inputCode)
    
  })
  
  observe({
    if(nchar(input$modelCodeEditor) == 0){
      rV$modelCode <- NULL  
    } else {
      rV$modelCode <- input$modelCodeEditor
    }
  })
  
  # When model code exists
  observe({
    updateCheckboxInput(session, 
                        inputId = "viewModelBuildButton",
                        value = !is.null(rV$modelCode))
  })
  
  
  # Download edited model
  output$downloadModel <- downloadHandler(
    filename = function() { paste0('GenPK-model-', Sys.Date(), '.cpp') },
    content = function(file) {
      writeLines(rV$modelCode, file)
    }
  )
  
  observeEvent(input$modelCovariates, {
    
    if(length(input$modelCovariates) == 0){
      
      session$sendCustomMessage(
        type = "genpkJS",
        '$("#modelCovariates").next().find(".selectize-input").addClass("redselect");'
      )
      
    } else {
      
      session$sendCustomMessage(
        type = "genpkJS",
        '$("#modelCovariates").next().find(".selectize-input").removeClass("redselect");'
      )
      
    }
    
  })
  
  observeEvent(input$buildModel, {
    
    withProgress(session = session, message = "Compiling model", value = .5, {
      
      rV$modelBuild <- genpk_try(
        session,
        rV$modelBuild,
        build_model(rV),
        "Model build success!",
        .error_notification_id = "build-model-genpk-error"
      )
      
      incProgress(.5)
    })
    
  })
  
  observeEvent(rV$modelCode, {
    
    if(is.null(rV$modelCode)){
      return(NULL)
    }
    
    if(input$compileOnEdit) {
      
      withProgress(session = session, message = "Compiling model", value = .5, {
        
        rV$modelBuild <- genpk_try(
          session,
          rV$modelBuild,
          build_model(rV),
          "Model build success!",
          .error_notification_id = "build-model-genpk-error"
        )
        
        incProgress(.5)
      })
    }
    
  })
  
  
  output$covariateTypeSelectionUI <- renderUI({
    
    if(is.null(input$modelCovariates)) return(div())  
    if(any(input$modelCovariates == "no_covariates")) return(div())
    
    covariateTypeSelectionUI <- tagList()
    
    for(i in input$modelCovariates){
      
      covariateTypeSelectionUI <- 
        tagAppendChild(
          covariateTypeSelectionUI,
          radioButtons(
            inputId = paste0(i, "__CovariateType"),
            label = paste0(i, " Type"),
            choices = c("Continuous", "Categorical")
          )
        )
      
    }
    
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL,
          status = "primary",
          title = "Coviariate Types",
          solidHeader = TRUE,
          covariateTypeSelectionUI
        )
      )
    )
  })
  
  observeEvent(rV$modelBuild, {
    
    if(all(class(rV$modelBuild) == "character")){
      
      updateCheckboxInput(session, inputId = "viewPop", value = FALSE)
      
    } else {
      updateCheckboxInput(session, inputId = "viewPop", value = TRUE)
      
      emptyParams <- is.null(rV$modelParam)
      
      rV$modelList <- as.list(rV$modelBuild)
      rV$modelParam <- param(rV$modelBuild)
      
      if(emptyParams){
        showModal(
          model_complete_modal(
            rV, 
            startSelected = NULL,
            timeStartSelected = "hour"
          )
        )
        session$sendCustomMessage(
          type = "genpkJS",
          '$("#modelCovariates").next().find(".selectize-input").addClass("redselect");'
        )
      }
      
    }
  })
  
  observeEvent(input$updateCovariates, {
    showModal(
      model_complete_modal(
        rV,
        startSelected = input$modelCovariates,
        timeStartSelected = input$model_time
      )
    )
  })
  
  observeEvent(input$modCovarClose, {
    updateCheckboxInput(session, inputId = "viewModelSummary", value = TRUE)
    updateCheckboxInput(session, inputId = "viewPopandDosing", value = TRUE)
  })
  
  observe({
    if(input$viewModelSummary){
      cmtVals <- seq_along(rV$modelList$cmt)
      names(cmtVals) <- rV$modelList$cmt
      rV$cmtChoices <- cmtVals
    }
  })
  
  observeEvent(rV$cmtChoices, {
    
    if(is.null(rV$cmtChoices)) {
      return(NULL)
    }
    
    cmtChoices <- rV$cmtChoices
    
    for(dose.i in c("first", "second", "third")){
      updateSelectInput(
        session,
        inputId = paste0(dose.i, "__cmt"),
        choices = cmtChoices
      )
    }
    
  })
  
  observeEvent(rV$contCovs, {
    
    for(dose.i in c("first", "second", "third")){
      
      updateSelectInput(
        session,
        inputId = paste0(dose.i, "__percovar"),
        choices = c("None (flat dosing)" = "none", rV$contCovs)
      )
    }
    
  })
  
  
  output$modelSummary <- renderDT({
    plain_table(as.data.frame(rV$modelParam) %>% tidyr::gather(Name, Value))
  })
  
  
  output$modelCovariatesUI <- renderUI({
    model_info_ui(rV, input, "modelCovariates", from = "input")
  })
  
  output$modelCapture <- renderUI({
    model_info_ui(rV, input, "capture", from = "modelList")
  })
  
  output$modelCompartments <- renderUI({
    model_info_ui(rV, input, "cmt", from = "modelList")
  })
  
  output$modelTimeUnit <- renderUI({
    tags$ul(tags$li(input$model_time))
  })
  
  output$modelSummaryUI <- renderUI({
    model_summary_ui(input, rV)
  })
  
  
  
  # # Population Tab ----------------------------------------------------------
  
  # UI for user to know what format the pop data needs to be in
  output$uploadedPopDataInput <- renderUI({
    upload_pop_ui(
      popDataType = input$popDataType,
      modelTime = input$model_time,
      modelCovariates = input$modelCovariates
    )
  })
  
  output$covariateCatTypesUI <- renderUI({
    sim_cat_covariates_ui(input, rV)
  })

  
  
  observeEvent(list(rV$catCovs, rV$contCovs), {
    
    if (length(rV$catCovs) == 0 | length(rV$contCovs) == 0) {
    
      observe({
        session$sendCustomMessage(type = "genpkJS", "$('#splitCovariates').css({ opacity: 0.0 });")
      })

      updateCheckboxInput(
        session = session,
        inputId = "splitCovariates",
        value = FALSE,
        label = "No spliting available"
      )
    } else {
      
      
      observe({
        session$sendCustomMessage(type = "genpkJS", "$('#splitCovariates').css({ opacity: 1.0 });")
      })
      
      updateCheckboxInput(
        session = session,
        inputId = "splitCovariates",
        value = FALSE,
        label = "Split Continuous Covariates by Categorical Covariates"
      )
      
    }
    
  })
  
  observeEvent(input$splitCovariates, {
    
    rVsplitCovariates <- expand.grid(rV$contCovs, paste0("by", rV$catCovs))
    
    rV$splitCovariates <-
      if(input$splitCovariates & nrow(rVsplitCovariates) > 0){
      rVsplitCovariates %>% 
      mutate(Var3 = paste0(Var1, Var2)) %>% 
      pull(Var3)
    } else {
      c()
    }
    
  })
  
  
  output$covariateSplitUI <- renderUI({
    split_covariates_ui(input, rV)
  })
  
  output$covariateContTypesUI <- renderUI({
    sim_cont_covariates_ui(input, rV)
  })
  
  
  output$correlationMatrix <- rhandsontable::renderRHandsontable({
    correlation_matrix_ui(input, rV$contCovs)
  })
  
  observeEvent(input$lockInNoCovarPop, {
    
    populationData <- data_frame(
      USUBJID = as.integer(seq(input$nPatientsNoCovars)),
      ID = as.integer(seq(input$nPatientsNoCovars)),
      time = 0
    )
    
    rV$populationData <- populationData
    rV$populationDataFull <- populationData
    
    showNotification(
      session = session,
      ui = "Population data success!",
      duration = 4,
      type = "message"
    )
    
  })
  
  observeEvent(input$uploadedPopData, {
    
    withProgress(session = session, message = "Validating population data", value = .5, {
      
      uploadedPopData <- input$uploadedPopData
      
      if(is.null(uploadedPopData)){
        return(NULL)
      }
      
      populationData <- read.csv(
        uploadedPopData$datapath,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
      
      rV$populationData <- 
        genpk_try(
          session,
          rV$populationData,
          validate_and_prep_pop_data(populationData, input$modelCovariates),
          "Population data success!",
          .error_notification_id = "validate-and-prep-pop-data-genpk-error"
        )
      
      incProgress(.5)
      
    })
    
    rV$populationDataFull <- rV$populationData
  })
  
  
  observeEvent(input$generatePopData, {
    
    withProgress(session = session, message = "Building population data", value = .5, {
      
      rV$populationData <- 
        genpk_try(
          session,
          rV$populationData,
          build_pop_data(input, rV),
          "Population data success!",
          .error_notification_id = "build-pop-data-genpk-error"
        )
      
      incProgress(.5)
      
    })
    
    rV$populationDataFull <- rV$populationData
    
  })
  
  observe({
    if(!is.null(rV$populationData)){
      updateCheckboxInput(session, inputId = "viewPopDataSummary", value = TRUE)
      updateCheckboxInput(session, inputId = "viewDosing", value = TRUE)
    } else {
      updateCheckboxInput(session, inputId = "viewPopDataSummary", value = FALSE)
      updateCheckboxInput(session, inputId = "viewDosing", value = FALSE)
    }
    
  })
  
  observeEvent(rV$nPopFullIDs, {
    updateTextInput(session,
                    inputId = "sampleFromPopSize", 
                    placeholder = rV$nPopFullIDs,
                    value = "")
  })
  
  output$downloadPopData <- downloadHandler(
    filename = function() { 
      paste('PK-Population-Data-', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(rV$populationData, file, row.names = FALSE)
    }
  )
  
  # Determine covariates
  observeEvent(input$modCovarClose, {
    if(any(input$modelCovariates == "no_covariates")) {
      
      rV$catCovs <- NULL
      rV$contCovs <- NULL
      
    } else {
      
      allInputs <- reactiveValuesToList(input)
      
      covariateInputs <- 
        allInputs[grepl("__CovariateType", names(allInputs), fixed = TRUE)]
      
      catCovs <- c()
      contCovs <- c()
      
      for(i in names(covariateInputs)){
        covariate.i <- gsub("__CovariateType", "", i, fixed = TRUE)
        
        if(!(covariate.i %in% input$modelCovariates)) next
        
        if(allInputs[[i]] == "Continuous") {
          contCovs <- c(contCovs, covariate.i)
        }
        
        if(allInputs[[i]] == "Categorical") {
          catCovs <- c(catCovs, covariate.i)
        }
      }
      
      rV$catCovs <- catCovs[!duplicated(catCovs)]
      rV$contCovs <- contCovs[!duplicated(contCovs)]
    }
  })
  
  observe({
    rV$nPopIDs <- length(unique(rV$populationData$USUBJID))
    rV$nPopFullIDs <- length(unique(rV$populationDataFull$USUBJID))
  })
  
  output$numPatientsFull <- renderUI({
    HTML(
      paste0("<h4 style='display:inline'><b>Number of Subjects in Initial Data:</b></h4> ",
             "<h5 style='display:inline'>",
             rV$nPopFullIDs,
             "</h5>")
    )
  })
  
  observeEvent(rV$populationData, {
    
    rV$populationDataSummary <- 
      genpk_try(
        session,
        NULL,
        population_data_summary(rV, input),
        .error_notification_id = "population-data-summary-genpk-error"
      )
    
  })
  
  output$popDataSummary <- renderUI({
    
    rV$populationDataSummary
    
  })
  
  
  output$cov_ex_data <- renderDT({
    
    popDataView <- rV$populationData
    
    popTypes <- lapply(popDataView, typeof)
    doublePopTypes <- popTypes[popTypes == "double"]
    
    popDataView[, names(doublePopTypes)] <- 
      signif(popDataView[, names(doublePopTypes)], digits = 3)
    
    app_table(popDataView, page_len = 10, .filter = "top")
  })
  
  output$cov_ex_data_no_covar <- renderDT({
    if(is.null(rV$populationData)) return(NULL)
    if(input$modelCovariates == "no_covariates"){
      app_table(rV$populationData %>% select(-time), page_len = 10)
    } else {
      return(NULL)
    }
  })
  
  observe({
    if(is.null(rV$populationData)) {
      
      rV$popDataPlots <- NULL
      
    } else {
      
      rV$popDataPlots <- genpk_try(
        session,
        NULL,
        pop_data_plots(rV$populationData, input$model_time),
        .error_notification_id = "pop-data-plots-genpk-error"
      )
      
    }
    
  })
  
  output$cov_ex_data_baseline_hist <- renderPlot({
    
    rV$popDataPlots$cov_ex_data_baseline_hist
    
  })
  
  output$cov_ex_data_time_var <- renderPlot({
    
    rV$popDataPlots$cov_ex_data_time_var
    
  })
  
  
  observeEvent(input$sampleFromPop, {
    
    withProgress(session = session, message = "Sampling from population", value = .5, {
      
      rV$populationData <- genpk_try(
        session,
        rV$populationData,
        sample_from_pop(
          rV$populationDataFull,
          rV$seed, 
          input$sampleFromPopSize
        ),
        "Population sample success!",
        .error_notification_id = "sample-from-pop-genpk-error"
      )
      
      incProgress(.5)
    })
  })
  
  output$continuousCovariateBinningUI <- renderUI({
    
    if(length(rV$contCovs) == 0) return(tags$div())
    
    genpk_try(
      session,
      NULL,
      covariate_binning_ui(rV$contCovs),
      .error_notification_id = "covariate-binning-ui-genpk-error"
    )
    
  })
  
  observeEvent(input$continuousCovariatesBinButton, {
    
    rV$populationData <- genpk_try(
      session,
      rV$populationData,
      mutate_continuous_covar_bins(
        rV$populationData,
        input
      ),
      "Bins created!",
      .error_notification_id = "mutate-continuous-covarbins-genpk-error"
    )
    
  })
  
  
  # Dosing Tab --------------------------------------------------------------
  
  output$dosingScheduleUploadUI <- renderUI({
    
    genpk_try(
      session,
      NULL,
      dosing_schedule_upload_ui(input, rV),
      .error_notification_id = "dosing-schedule-upload-ui-genpk-error"
    )
    
  })
  
  observeEvent(input$doseUploadData, {
    inFile <- input$doseUploadData
    
    if (is.null(inFile))
      return(NULL)
    
    doseUploadData <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
    
    rV$doseUploadDataValidated <-
      genpk_try(
        session,
        rV$doseUploadDataValidated,
        validate_dose_upload(
          doseUploadData,
          rV$populationData
        ),
        .error_notification_id = "validate-dose-upload-genpk-error"
      )
    
    rV$doseUploadData <- rV$doseUploadDataValidated$doses
    rV$samplingTimes <- rV$doseUploadDataValidated$samplingTimes
    
    if(!is.null(rV$samplingTimes)){
      
      updateCheckboxInput(session, inputId = "hideUserDefinedTimes", value = TRUE)
      
      showNotification(
        session = session,
        ui = "Non-dosing rows found in dosing data set. These records will be used as additional observation time points in the simulated data.",
        duration = 12,
        type = "warning"
      ) 
      
      updateTextInput(session = session, inputId = "user_sample_times", value = "")
      
    } else {
      updateCheckboxInput(session, inputId = "hideUserDefinedTimes", value = FALSE)
    }
    
  })
  
  observeEvent(rV$doseUploadData, {
    
    withProgress(session = session, message = "Uploading dose sequence", value = .5, {
      
      rV$dosingSequence <- 
        genpk_try(
          session,
          rV$dosingSequence,
          dose_upload_sequence(
            rV$doseUploadData,
            rV$populationData
          ) %>% filter(REC_TYPE == 1, ID == min(ID)),
          "Dose upload success!",
          .error_notification_id = "dose-upload-sequence-genpk-error"
        )
      
      incProgress(.5)
    })
  })
  
  output$uploadedSchedule <- renderDT({
    app_table(rV$doseUploadData)
  })
  
  observe({
    session$sendCustomMessage(type = "genpkJS", "$('#first__dose_start_time').prop('disabled', true);")
  })
  observeEvent(input$first__period, {
    if(input$first__period == 0){
      updateSelectInput(session, inputId = "second__period", selected = 0)
    }
  })
  
  observeEvent(input$second__period, {
    if(input$second__period == 0){
      updateSelectInput(session, inputId = "third__period", selected = 0)
    }
  })
  
  
  observe({
    
    secondUpdate <- get_start_time("second", "first", input)
    
    if(is.null(secondUpdate)) return(NULL)
    
    showNotification(
      session = session,
      ui = HTML(secondUpdate$noteText),
      duration = 5,
      type = "message"
    ) 
    
    updateNumericInput(
      session,
      inputId = "second__dose_start_time",
      value = secondUpdate$doseStartTime
    )
    
    updateSelectInput(
      session,
      inputId = "second__dose_start_time_unit",
      choices = secondUpdate$doseStartTimeUnit,
      selected = secondUpdate$doseStartTimeUnit
    )
    
    session$sendCustomMessage(
      type = "genpkJS",
      paste0('$("#FirstPrevText").html("', secondUpdate$interventionInfo, '")')
    )
    
  })
  
  observeEvent(input$second__dose_start_time_unit, {
    session$sendCustomMessage(
      type = "genpkJS",
      '$("#second__dose_start_time_unit").next().find(".selectize-input").addClass("disabled");'
    )
  })
  
  
  observe({
    
    thirdUpdate <- get_start_time("third", "second", input)
    
    if(is.null(thirdUpdate)) return(NULL)
    
    showNotification(
      session = session,
      ui = HTML(thirdUpdate$noteText),
      duration = 5,
      type = "message"
    ) 
    
    updateNumericInput(
      session,
      inputId = "third__dose_start_time",
      value = thirdUpdate$doseStartTime
    )
    
    updateSelectInput(
      session,
      inputId = "third__dose_start_time_unit",
      choices = thirdUpdate$doseStartTimeUnit,
      selected = thirdUpdate$doseStartTimeUnit
    )
    
    session$sendCustomMessage(
      type = "genpkJS",
      paste0('$("#SecondPrevText").html("', thirdUpdate$interventionInfo, '")')
    )
    
  })
  
  observeEvent(input$third__dose_start_time_unit, {
    session$sendCustomMessage(
      type = "genpkJS",
      '$("#third__dose_start_time_unit").next().find(".selectize-input").addClass("disabled");'
    )
  })
  
  
  
  
  observeEvent(input$lockInDoses, {
    
    rV$doseUploadDataValidated <- NULL
    rV$doseUploadData <- NULL
    rV$samplingTimes <- NULL
    
    updateCheckboxInput(session, inputId = "hideUserDefinedTimes", value = FALSE)
    
    withProgress(session = session, message = "Compiling dose sequence", value = .5, {
      
      secondDoseStartTime <- as.numeric(input$second__dose_start_time)
      thirdDoseStartTime <- as.numeric(input$third__dose_start_time)
      
      if(!is.na(secondDoseStartTime) & !is.na(thirdDoseStartTime)) {
        
        if((input$second__period != 0) & (input$third__period != 0)){
          
          secondIntStartInDays <-
            as.numeric(
              unclass(as_time(as_time(input$second__dose_start_time,
                                      input$second__dose_start_time_unit),"day"))
            )
          
          thirdIntStartInDays <-
            as.numeric(
              unclass(as_time(as_time(input$third__dose_start_time,
                                      input$third__dose_start_time_unit),"day"))
            )
          
          if(thirdIntStartInDays < secondIntStartInDays){
            showModal(
              modalDialog(
                title = "",
                HTML("<div class ='redselect alert'><b>Third Interventon</b> must start on or after <b>Second Intervention</b></div>")
              )
            )
            return(NULL)
          }
        }
      }
      
      rV$dosingSequence <- 
        genpk_try(
          session,
          rV$dosingSequence,
          dosing_sequence(
            input,
            .d1,.d2,.d3,.p1,.p2,.p3,
            idata = rV$populationData[1, ],
            nid = 1
          ) %>% filter(REC_TYPE == 1),
          "Dose sequence success!",
          .error_notification_id = "dosing-sequence-genpk-error"
        )
      
      incProgress(.5)
      
    })
    
    
  })
  
  
  observeEvent(rV$dosingSequence, {
    
    rV$nDosesSummary <- 
      genpk_try(
        session,
        rV$nDosesSummary,
        calc_n_doses_summary(
          rV$dosingSequence,
          input$model_time, 
          rV$cmtChoices,
          1
        ),
        .error_notification_id = "calc-n-doses-summary-genpk-error"
      )
    
    updateCheckboxInput(session, inputId = "viewDosingSummary", value = TRUE)
    
    if(is.null(rV$nDosesSummary$n_doses$ndosenumeric)){
      rV$nDoseDisplaySidebar <- NULL
    } else {
      rV$nDoseDisplaySidebar <- sum(rV$nDosesSummary$n_doses$ndosenumeric)
    }
    
    
    rV$doseTable <- rV$nDosesSummary$n_doses_table
    
    updateNumericInput(session, 
                       inputId = "max_obs_time",
                       value = rV$nDosesSummary$max_dose_day)
  })
  
  observeEvent(input$first__period, {
    if(input$first__period == 0){
      session$sendCustomMessage(type = "genpkJS", '$("#first__period").next().find(".selectize-input").removeClass("greenselect");')
    } else {
      session$sendCustomMessage(type = "genpkJS", '$("#first__period").next().find(".selectize-input").addClass("greenselect");')
    }
  })
  
  observeEvent(input$second__period, {
    if(input$second__period == 0){
      session$sendCustomMessage(type = "genpkJS", '$("#second__period").next().find(".selectize-input").removeClass("greenselect");')
    } else {
      session$sendCustomMessage(type = "genpkJS", '$("#second__period").next().find(".selectize-input").addClass("greenselect");')
    }
  })
  
  observeEvent(input$third__period, {
    if(input$third__period == 0){
      session$sendCustomMessage(type = "genpkJS", '$("#third__period").next().find(".selectize-input").removeClass("greenselect");')
    } else {
      session$sendCustomMessage(type = "genpkJS", '$("#third__period").next().find(".selectize-input").addClass("greenselect");')
    }
  })
  
  
  
  observeEvent(rV$doseTable, {
    
    rV$timeframeGroupings <- 
      genpk_try(
        session,
        rV$timeframeGroupings,
        build_timeframe_groupings(rV$doseTable, rV$timeframeGroupings),
        .error_notification_id = "build-timeframe-groupings-genpk-error"
      )
    
  })
  
  
  output$doseTable <- DT::renderDataTable({
    render_n_dose_table(rV$doseTable)
  })
  
  
  # Simulation Tab ----------------------------------------------------------
  
  
  observe({
    hasMod <- !(is.null(rV$modelBuild))
    hasPop <- !(is.null(rV$populationData))
    hasDose <- !(is.null(rV$doseTable))
    if(hasMod & hasPop & hasDose){
      updateCheckboxInput(session, "viewSimTab", value = TRUE)
    }
  })
  
  observeEvent(input$sim_id, {
    if(input$sim_id == ""){
      
      session$sendCustomMessage(type = "genpkJS", "simIDValidation('fail');")
      
    } else {
      
      session$sendCustomMessage(type = "genpkJS", "simIDValidation('success');")
    }
  })
  
  
  output$numIDsSim <- renderUI({
    valueBox(
      width = "100%",
      rV$nPopIDs,
      "Subjects", 
      icon = icon("users")
    )
  })
  
  observeEvent(input$simulate, {
    
    withProgress(session = session, message = paste0("Simulating '", input$sim_id, "'"), value = .5, {
      
      rV$genpkSimList <- 
        genpk_try(
          session,
          rV$genpkSimList,
          genpk_sim(rV, input),
          "Simulation run success!",
          .error_notification_id = "genpk-sim-genpk-error"
        )
      
      incProgress(.5)
      
    })
  })
  
  observe({
    
    updateCheckboxGroupInput(session,
                             inputId = "simPlotIds",
                             choices = names(rV$genpkSimList$mrgInputs),
                             selected = NULL,
                             inline = TRUE)
    
  })
  
  observeEvent(rV$genpkSimList$mrgInputs, {
    
    updateCheckboxGroupInput(session,
                             inputId = "downloadSelectedSimID",
                             choices = names(rV$genpkSimList$mrgInputs),
                             selected = names(rV$genpkSimList$mrgInputs))
    
    updateSelectInput(session,
                      inputId = "simIDViewInput",
                      choices = names(rV$genpkSimList$mrgInputs),
                      selected = names(rV$genpkSimList$mrgInputs))
    
  })
  
  output$overlayDataInfo <- renderUI({
    tagList(
      tags$h4("Overlay Data Requirements: "),
      tags$ul(
        tags$li(
          HTML(
            "Required columns: <b>USUBJID</b>, <b>time</b>"
          )
        ),
        tags$li(
          "At least one of these columns required: ",
          HTML(
            paste(c("<b>", paste(rV$modelList$capture, collapse = "</b>, <b>"), "</b>"), collapse = "")
          )
        ),
        tags$li(
          "Any other column found in ", tags$b('User Overlay'), " data that is also in the ", tags$b('Simulated Data'), " will be utilized"
        )
      ),
      tags$br(),
      tags$h4("Once Uploaded: "),
      tags$ul(
        tags$li("This data will have a ", tags$b('SIM_ID'), " = ", tags$b('user-overlay')),
        tags$li(tags$b('ID'), " is generated from matching ", tags$b('USUBJID'), " ( where available in ", tags$b('Simulated Data'), ")"),
        tags$li(tags$b('USUBJID'), " not found in ", tags$b('Simulated Data'), " will have an ", tags$b('ID'), " generated")
      )
    )
  })
  
  observe({
    
  })
  
  observeEvent(input$overlayData, {
    
    inFile <- input$overlayData
    
    if (is.null(inFile)) {
      return(NULL)
    }
    
    withProgress(session = session, message = "Validating overlay data", value = .5, {
      
      rV$overlayDataRaw <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      
      rV$genpkSimList$mrgSimulations <- 
        genpk_try(
          session,
          rV$genpkSimList$mrgSimulations,
          validate_prep_and_bind_overlay_data(rV),
          "Overlay data success!",
          .error_notification_id = "validate-prep-and-bind-overlay-data-genpk-error"
        )
      
      incProgress(.5)
      
    })
    
  })
  
  observeEvent(rV$genpkSimList$mrgSimulations, {
    updateCheckboxInput(session, inputId = "viewModelSims", value = TRUE)
  })
  
  observeEvent(input$clean_plot_id, {
    if(is.null(input$simPlotIds)) return(NULL)
    if(length(input$simPlotIds) == 0) return(NULL)
    mrgSimulations <- rV$genpkSimList$mrgSimulations
    mrgSimulations <- dplyr::filter(mrgSimulations, !(SIM_ID %in% input$simPlotIds))
    if(nrow(mrgSimulations) == 0){
      showModal(
        modalDialog(
          title = tagList(
            tags$span(class = "badge label-danger", icon("remove", lib = "glyphicon")),
            " Simulation Remove Error"
          ),
          HTML(paste0("Cannot remove all remaining SIM_ID(s) in data.<br><br>SIM_ID(s) '", 
                      paste(input$simPlotIds, collapse = "' '"),
                      "' not removed.")
          )
        )
      )
      return(NULL)
    }
    
    rV$genpkSimList$mrgSimulations <- mrgSimulations
    
    for(i in input$simPlotIds){
      
      rV$genpkSimList$mrgInputs[[i]] <- NULL
      
    }
  })
  
  observeEvent(rV$genpkSimList, {
    rV$summarizeVariables <- c(
      "SIM_ID", 
      "USUBJID",
      "SIM_TYPE",
      colnames(rV$genpkSimList$mrgSimulations)[grepl("(Bins)", colnames(rV$genpkSimList$mrgSimulations), fixed = TRUE)],
      # rV$timeframeGroupings,
      "Cumulative Dose Count"
    )
  })
  
  observeEvent(rV$genpkSimList$mrgSimulations, {
    
    if(is.null(rV$genpkSimList$mrgSimulations)) return(NULL)
    
    plotUIChoices <- plot_ui_choices(rV)
    
    for(i in c("Var", "Color")){
      for(j in c("simulationsPlotData", "summaryStatsPlotData")){
        
        if(i == "Color" & j == "summaryStatsPlotData") next
        
        updateSelectInput(
          session,
          inputId = paste0(j, i),
          choices = plotUIChoices[[paste0("plotData", i, "Choices")]],
          selected = plotUIChoices[[paste0("plotData", i, "Selected")]]
        )
      }
    }
    
    for(i in c("simulationsPlotData", "summaryStatsPlotData")){
      
      choices.i <- plotUIChoices$plotDataSummarizeChoices
      
      if(i == "summaryStatsPlotData") {
        choices.i <- choices.i[choices.i != "USUBJID"]
      }
      
      updateCheckboxGroupInput(
        session,
        inputId = paste0(i, "Summarize"),
        choices = choices.i,
        selected = plotUIChoices$plotDataSummarizeSelected
      )
      
      
    }
    
  })
  
  observe({
    updateRadioButtons(
      session,
      inputId = "summaryStatsPlotDataColor",
      choices = c("None" = "no_color", input$summaryStatsPlotDataSummarize),
      selected = "no_color"
    )
  })
  
  
  observeEvent(input$updateResults, {
    
    withProgress(session = session, message = "Compiling results", value = .5, {
      
      rV$simulationResults <- 
        genpk_try(
          session,
          rV$simulationResults,
          build_simulation_results(rV, input),
          .error_notification_id = "build-simulation-results-genpk-error"
        )
      
      incProgress(.5)
      
    })
  })
  
  output$simulationsPlotUI <- renderUI({
    shiny::req(rV$simulationResults)
    render_plot_pages(
      rV$simulationResults$simulationsPlot,
      session
    )
    
  })
  
  observe({
    shiny::req(rV$simulationResults)
    .plots <- rV$simulationResults$simulationsPlot
    for(i in 2:length(.plots)){
      local({
        name.i <- names(.plots)[[i]]
        output[[name.i]] <- renderPlot({
          .plots[[name.i]]
        })
      })
    }
  })
  
  output$predictionIntervalsPlotData <- renderDT({
    shiny::req(rV$simulationResults)
    
  })
  
  
  output$summaryStatsPlot <- renderPlot({
    rV$simulationResults$summaryStatsPlot
  })
  
  output$summaryStatsAvgs <- renderDT({
    if(is.null(rV$simulationResults$summaryStatsDataList)) return(NULL)
    
    rV$simulationResults$summaryStatsAveragesTable %>% 
      mutate(`Summarize by` = gsub("\n", "<br>", `Summarize by`)) %>% 
      plain_table(., .info = FALSE, .columnDefs = list(width = '150px', targets = "_all"))
  })
  
  output$summaryStatsData <- renderDT({
    if(is.null(rV$simulationResults$summaryStatsDataList)) return(NULL)

    rV$simulationResults$summaryStatsDataTable %>%
      mutate(`Summarize by` = gsub("\n", "<br>", `Summarize by`)) %>% 
      app_table(page_len = 10, .filter = "top")
  })
  
  
  observe({
    updateAceEditor(session,
                    editorId = "modelCodeInputs",
                    value = rV$genpkSimList$mrgInputs[[input$simIDViewInput]])
  })
  
  output$populationDataInputs <- renderDT({
    
    popDataView <- rV$genpkSimList$mrgInputs[[input$simIDViewInput]]$populationData
    
    popTypes <- lapply(popDataView, typeof)
    doublePopTypes <- popTypes[popTypes == "double"]
    
    popDataView[, names(doublePopTypes)] <-
      signif(popDataView[, names(doublePopTypes)], digits = 3)
    
    app_table(popDataView, page_len = 5)
  })
  
  
  output$doseTableInputs <-  DT::renderDataTable({
    render_n_dose_table(rV$genpkSimList$mrgInputs[[input$simIDViewInput]]$doseTable)
  })
  
  output$simData <- renderDT({
    withProgress(session = session, message = "Preparing data for display", value = .5, {
    simulations_data_table(rV$genpkSimList$mrgSimulations)
    })
  })
  
  output$exportResultsDownload <- downloadHandler(
    
    filename = function() {
      paste(input$zipName, ".zip", sep="")
    },
    content = function(file) {
      
      withProgress(session = session, message = "Preparing zip", value = .5, {
        
        temp_dir <- tempdir()
        message(temp_dir)
        filesToZip <- c()
        
        simRes <- rV$simulationResults
        mrgIn <- rV$genpkSimList$mrgInputs
        
        
        for(plot.i in names(simRes$simulationsPlot)){
          simRes[[plot.i]] <- simRes$simulationsPlot[[plot.i]]
        }
        
        simRes$simulationsPlot <- NULL
        simRes$summaryStatsDataList <- NULL
        
        for(sim.i in input$downloadSelectedSimID){
          
          for(input.i in names(mrgIn[[sim.i]])){
            
            simRes[[paste0(sim.i, "-", input.i)]] <- mrgIn[[sim.i]][[input.i]]
            
          }
          
        }
        
        ### Current View Downloads
        for(file.i in names(simRes)){
          
          file_class.i <- class(simRes[[file.i]])
          
          if("ggplot" %in% file_class.i){
            action.i <- "createPng"
          } else if("data.frame" %in% file_class.i){
            action.i <- "writeCsv"
          } else if("character" %in% file_class.i){
            action.i <- "writeCpp"
          }
          rm(file_class.i)
          create_this_file <- function(extension, tempDir = temp_dir, fileI = file.i){
            file.path(
              tempDir,
              paste0(gsub("_", "-", fileI, fixed = TRUE), ".", extension)
            )
          }
          
          if(action.i == "createPng"){
            
            this_file.i <- create_this_file("png")
            
            ggsave(filename = this_file.i,
                   plot = simRes[[file.i]],
                   device = "png",
                   width = 60, height = 30, units = "cm")
            
          } else if (action.i == "writeCsv"){
            
            this_file.i <- create_this_file("csv")
            
            dat_out.i <- simRes[[file.i]]
            
            if(file.i == "percentilesPlotData"){
              dat_out.i <- dat_out.i %>% select(-fill)
            }
            
            write.csv(x = dat_out.i, 
                      file = this_file.i,
                      row.names = FALSE)
            
            rm(dat_out.i)
          } else if (action.i == "writeCpp"){
            
            this_file.i <- create_this_file("cpp")
            
            writeLines(simRes[[file.i]], this_file.i)
          }
          
          filesToZip <- unique(c(filesToZip, this_file.i))
          rm(this_file.i)
          
        }
        
        zip(zipfile = file, files = filesToZip, flags = "-r9Xj")
        
        for(file.i in filesToZip){
          file.remove(file.i)
        }
      })
    }
    
  )
  
  
  
  # Results tab -------------------------------------------------------------
  
  
  # observeEvent(input$simData_rows_current, {
  #   message(paste0("rows current " , input$simData_rows_current))
  # })
  
  observe({
    if(!(is.null(rV$genpkSimList$mrgSimulations))){
      updateCheckboxInput(session, "viewResTab", value = TRUE)
      if(!(is.null(input$simData_rows_current))){
        updateCheckboxInput(session, "viewSimRes", value = TRUE)
      }
    }
  })
  
  observeEvent(input$model_time, {
    updateSelectInput(session,
                      inputId = "plotDataTimeUnit", 
                      selected = input$model_time)
  })
  
  observe({
    
    select_at_least_one(
      session = session,
      input = input,
      input_id = "summaryStatsPlotDataStats", 
      choices = summaryStatsPlotDataStatsChoices, 
      label = "Stats Display"
    )
    
     
  })
  
  observe({
    
    select_at_least_one(
      session = session,
      input = input,
      input_id = "summaryStatsPlotDataStatsType", 
      choices = summaryStatsPlotDataStatsTypeChoices, 
      label = "Stats Type"
    )
    
  })
  
}
