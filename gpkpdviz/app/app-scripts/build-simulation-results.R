build_simulation_results <- function(rV, input){
  
  ans <- list()
  
  ans$simulatedData <- rV$genpkSimList$mrgSimulations
  
  # Build plot data ---------------------------------------------------------
  simPlotData <- ans$simulatedData
  
  simPlotData <- simPlotData %>% mutate(SIM_ID_and_ID = paste0(SIM_ID, "-", ID))
  
  if(!is.null(input$simData_rows_all)){
    simPlotData <- simPlotData %>% slice(input$simData_rows_all)
  }
  
  if(nrow(simPlotData) == 0){
    stop("No rows in data after applying filters")
  }
  
  # Simulations Plot --------------------------------------------------------
  simulationsPlotData <- simPlotData
  
  simulationsPlotData$timeDay <- get(paste0("as.day.", input$model_time))(simulationsPlotData$time)
  
  simulationsPlotData$simulationsPlotxVar <- 
    get(paste0("as.", input$simulationsPlotDataTimeUnit, ".day"))(simulationsPlotData$timeDay)
  
  simulationsPlotData$timeDay <- NULL
  
  simulationsPlotData$no_color <- "no_color"
  
  simulationsPlotData <- create_split_col(
    .df = simulationsPlotData,
    plotSplit = input$simulationsPlotDataSummarize,
    yVar = input$simulationsPlotDataVar,
    .colname = "simulationsPlotSplit"
  )
  
  simulationsPlotData <- create_color_col(
    simulationsPlotData,
    plotColor = input$simulationsPlotDataColor,
    .colname = "simulationsPlotColor"
  )
  
  rangeNumbers <- c(as.numeric(as.factor(simulationsPlotData$simulationsPlotSplit))) / input$simulationsPlotNPerPage
  
  simulationsPlotData$simulationsPlotSplitPage <- ifelse(
    floor(rangeNumbers) == rangeNumbers, 
    rangeNumbers, 
    floor(rangeNumbers) + 1
  )
  
  sim_plot_list <- list()
  
  for(i in c(0, unique(simulationsPlotData$simulationsPlotSplitPage))){
    
    # Use zero to store full plot
    if(i == 0){
      simulationsPlotData.i <- simulationsPlotData
      name.i <- paste0("plot", "-all")
    } else {
      simulationsPlotData.i <- simulationsPlotData %>% filter(simulationsPlotSplitPage == i)
      name.i <- paste0("plot", "-", i, "of", max(unique(simulationsPlotData$simulationsPlotSplitPage)))
    }
    
    
    sim_plot.i <- 
      ggplot(data = simulationsPlotData.i,
             aes(x = simulationsPlotxVar,
                 y = yVar)) +
      theme_bw(base_size = 20)
    
    if(input$simulationsPlotScale == "Log") {
      sim_plot.i <- sim_plot.i + scale_y_log10()
    }
    
    # Determine ranges and update if necessary
    x_data_range <- range(simulationsPlotData$simulationsPlotxVar, na.rm = TRUE)
    y_data_range <- range(simulationsPlotData$yVar, na.rm = TRUE)
    
    force_first_not_negative <- function(.range, .simulationsPlotData, .var){
      
      .range[1] <- 
        ifelse(
          .range[1] <= 0,
          min(min(.simulationsPlotData[[.var]][.simulationsPlotData[[.var]] > 0], na.rm = TRUE), 1),
          .range[1]
        )
      
      .range
      
    }
    
    change_limits <- any(is.finite(c(input$simulationsPlotXMin,
                                     input$simulationsPlotXMax,
                                     input$simulationsPlotYMin,
                                     input$simulationsPlotYMax)))
    
    if(change_limits){
      
      if(is.finite(input$simulationsPlotXMin)){
        x_data_range[1] <- input$simulationsPlotXMin
      }
      
      if(is.finite(input$simulationsPlotXMax)){
        x_data_range[2] <- input$simulationsPlotXMax
      }
      
      if(is.finite(input$simulationsPlotYMin)){
        y_data_range[1] <- input$simulationsPlotYMin
      }
      
      if(is.finite(input$simulationsPlotYMax)){
        y_data_range[2] <- input$simulationsPlotYMax
      }
      
      if(input$simulationsPlotScale == "Log") {
        y_data_range <- force_first_not_negative(y_data_range, simulationsPlotData, "yVar")
      }
      
      sim_plot.i <- sim_plot.i + coord_cartesian(xlim = x_data_range, ylim = y_data_range)
      
    } else if(input$simulationsPlotScale == "Log") {
      
      y_data_range <- force_first_not_negative(y_data_range, simulationsPlotData, "yVar")
      
    }
    
    # Lines points percentile -------------------------------------------------
    for(sim_type.i in c("SimulationDisplay", "User_OverlayDisplay", "User_SpecifiedDisplay")){
      
      display.i <- input[[sim_type.i]]
      
      display_data.i <- 
        simulationsPlotData.i %>% 
        filter(
          SIM_TYPE == gsub("_", " ", gsub("Display", "", sim_type.i), fixed = TRUE)
        )
      
      if("lines" %in% display.i){
        
        if(typeof(simulationsPlotData.i$simulationsPlotColor) == "double"){
          
          sim_plot.i <- sim_plot.i + 
            geom_line(data = display_data.i,
                      aes(group = SIM_ID_and_ID, color = simulationsPlotColor),
                      size = 1) +
            scale_color_continuous(low = "green", high = "red")
          
        } else {
          
          sim_plot.i <- sim_plot.i + 
            geom_line(data = display_data.i,
                      aes(group = SIM_ID_and_ID, color = simulationsPlotColor),
                      size = 1, 
                      alpha = 2/3)
          
        }
        
      }
      
      
      if("points" %in% display.i){
        
        if(typeof(simulationsPlotData.i$simulationsPlotColor) == "double"){
          
          sim_plot.i <- 
            sim_plot.i + 
            geom_point(data = display_data.i,
                       aes(group = SIM_ID_and_ID, color = simulationsPlotColor),
                       color = "black",
                       alpha = 1, 
                       shape = 21) +
            scale_color_continuous(low = "green", high = "red")
          
        } else {
          
          sim_plot.i <- 
            sim_plot.i + 
            geom_point(data = display_data.i,
                       aes(group = SIM_ID_and_ID, color = simulationsPlotColor), 
                       alpha = 2/3)
          
        }
        
      }
      
      if("percentiles" %in% display.i){
        lower_quantile <- (1 - input$simulationsPlotRibbonPercentile) / 2
        upper_quantile <- 1 - lower_quantile
        
        sim_plot.i <- 
          sim_plot.i +
          aes(fill = simulationsPlotColor) +
          stat_summary(data = display_data.i,
                       geom="ribbon",
                       fun.ymin = function(x) quantile(x, lower_quantile),
                       fun.ymax = function(x) quantile(x, upper_quantile),
                       alpha = .25,
                       size = 1)
        
        if(input$simulationsPlotIncludeMedian){
          
          sim_plot.i <- 
            sim_plot.i +
            stat_summary(
              data = display_data.i,
              aes(color = simulationsPlotColor),
              geom = "line",
              size = 1, 
              linetype = "dashed", 
              fun.y = median
            )
          
        }
        
      }
      rm(display_data.i)
      rm(display.i)
    }
    
    # Reference line ----------------------------------------------------------
    if(length(input$simulationsPlotRef) > 0){
      if(!is.na(as.numeric(input$simulationsPlotRef))){
        sim_plot.i <- 
          sim_plot.i +
          geom_hline(yintercept = input$simulationsPlotRef,
                     size = 1, 
                     linetype = "dashed")
      }
    }
    
    # Dose Events -------------------------------------------------------------
    if(input$simulationsPlotIncludeDoses){
      
      doseDat.i <- simulationsPlotData.i %>% filter(REC_TYPE == "Dose")
      
      if(nrow(doseDat.i) > 0){
        
        if("simulationsPlotSplit" %in% colnames(simulationsPlotData.i)){
          doseDat.i <- doseDat.i %>% distinct(simulationsPlotxVar, AMT, simulationsPlotSplit)
        } else {
          doseDat.i <- doseDat.i %>% distinct(simulationsPlotxVar, AMT)
        }
        
        sim_plot.i <- 
          sim_plot.i +
          geom_vline(data = doseDat.i, 
                     aes(xintercept = simulationsPlotxVar),
                     color = "darkgrey") +
          geom_label(data = doseDat.i, 
                     aes(label = paste0(paste0("AMT: ", round(AMT,2))),
                         x = simulationsPlotxVar, 
                         y = 0),
                     color = "black",
                     fill = "lightgrey")
        
      }
      rm(doseDat.i)
    }
    
    if("simulationsPlotSplit" %in% colnames(simulationsPlotData.i)){
      
      free_x <- !("share_x" %in% input$simulationsPlotAxisFree)
      free_y<- !("share_y" %in% input$simulationsPlotAxisFree)
      
      scales_call <- case_when(
        free_x & free_y ~ "free",
        free_x & !free_y ~ "free_x",
        !free_x & free_y ~ "free_y",
        TRUE ~ "fixed"
      )
      
      sim_plot.i <- sim_plot.i + facet_wrap(~simulationsPlotSplit, scales = scales_call, nrow = input$simulationsPlotNRowPerPage)
      
      if(scales_call == "fixed" & !change_limits){
        sim_plot.i <- sim_plot.i + coord_cartesian(xlim = x_data_range, ylim = y_data_range)
      }
    }
    
    # Clean Up ----------------------------------------------------------------
    if(input$simulationsPlotDataColor != "no_color"){
      
      sim_plot.i <- 
        sim_plot.i +
        labs(
          y = "",
          x = tools::toTitleCase(input$simulationsPlotDataTimeUnit),
          caption = name.i,
          fill = input$simulationsPlotDataColor, 
          color = input$simulationsPlotDataColor
        )
      
    } else {
      
      sim_plot.i <- 
        sim_plot.i +
        scale_color_manual(values = c("no_color" = "grey5")) +
        scale_fill_manual(values = c("no_color" = "grey5")) +
        theme(legend.position = "none") +
        labs(
          y = "",
          x = tools::toTitleCase(input$simulationsPlotDataTimeUnit),
          caption = name.i
        )
      
    }
    
    
    sim_plot.i <- 
      sim_plot.i + 
      theme(
        strip.text = element_text(size = 18)
      )
    
    sim_plot_list[[name.i]] <- sim_plot.i
    
    rm(sim_plot.i)
    rm(name.i)
  }
  
  ans$simulationsPlot <- sim_plot_list
  
  has_percentiles <-
    "percentiles" %in% c(input$SimulationDisplay,
                         input$User_OverlayDisplay,
                         input$User_SpecifiedDisplay)
  
  if(has_percentiles){
    
    plot_build <- ggplot2::ggplot_build(ans$simulationsPlot$`plot-all`)
    
    # Facet info
    facet_names <- plot_build$layout$layout %>% select(PANEL, simulationsPlotSplit)
    
    facet_names$simulationsPlotSplit <- 
      gsub("\n", "; ", facet_names$simulationsPlotSplit, fixed = TRUE)
    
    # Percentile info
    percentile_used <- (1 - input$simulationsPlotRibbonPercentile) / 2
    yminName <- paste0(percentile_used, " Percentile")
    ymaxName <- paste0(1 - percentile_used, " Percentile")
    
    # Check for percentile datasets based on alpha
    check_for_percentiles <- function(.df){
      
      if(!"alpha" %in% colnames(.df)) return(FALSE)
      if(all(is.na(.df$alpha))) return(FALSE)
      if(!all(.df$alpha == .25, na.rm = TRUE)) return(FALSE)
      
      return(TRUE)
    }
    
    percentile_datasets <- 
      which(purrr::map_lgl(plot_build$data, check_for_percentiles))
    
    percentile_data <- data_frame()
    
    for(i in percentile_datasets){
      percentile_data <-
        bind_rows(
          percentile_data,
          plot_build$data[[i]] %>% select(fill, group, x, ymin, ymax, PANEL)
        )
    }
    
    hasColor <- !all(percentile_data$group == -1)
    
    if(input$simulationsPlotIncludeMedian){
      
      # Check for median based on dashed line
      check_for_median <- function(.df){
        
        if(!"linetype" %in% colnames(.df)) return(FALSE)
        if("xintercept" %in% colnames(.df)) return(FALSE)
        if("yintercept" %in% colnames(.df)) return(FALSE)
        if(all(is.na(.df$linetype))) return(FALSE)
        if(!all(.df$alpha == "dashed", na.rm = TRUE)) return(FALSE)
        
        return(TRUE)
      }
      
      median_datasets <- which(purrr::map_lgl(plot_build$data, check_for_median))
      
      median_data <- data_frame()
      
      for(i in median_datasets){
        median_data <-
          bind_rows(
            median_data,
            plot_build$data[[i]]
          )
      }
      
      if(!hasColor){
        median_data <- median_data %>% mutate(fill = 'grey20')
      }
      
      median_data <- median_data %>% select(fill, group, x, Median = y, PANEL)
      
      percentile_data <- 
        full_join(percentile_data, median_data) %>%
        select(fill, group, x, ymin, Median, ymax, PANEL)
      
    }
    
    colnames(percentile_data)[colnames(percentile_data) == "ymin"] <- yminName
    colnames(percentile_data)[colnames(percentile_data) == "ymax"] <- ymaxName
    
    percentile_data <-
      percentile_data %>% 
      left_join(facet_names) %>% 
      rename(`Summarize by` = simulationsPlotSplit) %>% 
      select(-PANEL)
    
    colnames(percentile_data)[colnames(percentile_data) == "x"] <- 
      paste0("Time (",
             tolower(unlist(strsplit(plot_build$plot$labels$x,
                                     " (", fixed = TRUE))[1]),
             ")")
    
    
    
    if(hasColor){
      
      color_names <- 
        data_frame(
          character_name = 
            unique(as.character((factor(plot_build$plot$data$simulationsPlotColor)))),
          group = 
            unique(as.integer(factor(plot_build$plot$data$simulationsPlotColor)))
        )
      
    } else {
      
      color_names <- 
        data_frame(
          character_name = "continuous",
          group = -1
        )
    }
    
    
    colnames(color_names)[colnames(color_names) == "character_name"] <-
      input$simulationsPlotDataColor
    
    percentile_data <- 
      percentile_data %>%
      left_join(color_names) %>%
      select(!!c("fill", input$simulationsPlotDataColor), everything())
    
    if(!hasColor){
      percentile_data <-
        percentile_data %>% mutate(fill = gsub("20", "", fill, fixed = TRUE))
    }
    
    ans$simulationsPlotPercentilesData <- 
      percentile_data %>%
      select(-group) %>% 
      mutate(`Summarize by` = as.factor(`Summarize by`)) %>% 
      select(-fill) %>% 
      select(`Summarize by`, everything())
    
    # ans$simulationsPlotPercentilesData$DT <- 
    #   app_table(
    #     ans$simulationsPlotPercentilesData$data,
    #     page_len = 25,
    #     .filter = "top",
    #     .columnDefs = list(
    #       list(className = 'dt-center', targets = 0:3),
    #       list(visible=FALSE, targets=c(1))
    #     )
    #   ) %>%
    #   formatStyle(columns = c(1:4), 'text-align' = 'center') %>%
    #   formatRound(c(5:ncol(ans$simulationsPlotPercentilesData$data)), 2) %>%
    #   formatStyle(
    #     columns = input$simulationsPlotDataColor,
    #     backgroundColor = styleEqual(
    #       unique(ans$simulationsPlotPercentilesData$data[[input$simulationsPlotDataColor]]),
    #       unique(ans$simulationsPlotPercentilesData$data$fill)
    #     )
    #   )
    
  } else {
    
    ans$simulationsPlotPercentilesData <- data_frame(
      MESSAGE = "No Percentiles in Plot"
    )
    
  }
  
  
  # Summary stats -----------------------------------------------------------
  
  summaryStatsPlotData <- create_split_col(
    simPlotData,
    plotSplit = input$summaryStatsPlotDataSummarize, 
    yVar = input$summaryStatsPlotDataVar,
    .colname = "summaryStatsPlotSplit"
  )
  
  summaryStatsPlotData$no_color <- "no_color"
  
  summaryStatsPlotData <- create_color_col(
    summaryStatsPlotData,
    plotColor = input$summaryStatsPlotDataColor,
    .colname = "summaryStatsPlotColor"
  )
  
  
  summaryStatsDataList <- list()
  
  summaryStatsDataList$colorVar <- input$colorVar
  
  for(i in threshStats){
    summaryStatsDataList[[paste0(i, "Thresh")]] <- as.numeric(input[[paste0(i, "Thresh")]])
  }
  
  summariseByGroups <- c("ID", "SIM_ID_and_ID", "summaryStatsPlotSplit")
  
  summariseGrouping <- rlang::syms(summariseByGroups)
  
  summaryStatsDataList$summaryData <-
    summaryStatsPlotData %>%
    group_by(!!!summariseGrouping) %>%
    summarise(
      MAX = max(yVar, na.rm = TRUE),
      MIN = min(yVar, na.rm = TRUE),
      LAST = last(yVar),
      AUC = PKPDmisc::auc_partial(idv = time, dv = yVar),
      summaryStatsPlotColor = unique(summaryStatsPlotColor)
    ) %>%
    ungroup()
  
  calc_thresh <- function(.x, .thresh){
    
    threshVal <- summaryStatsDataList[[.thresh]]
    
    if(is.na(threshVal)) return("")
    
    paste0(
      round(sum(.x > threshVal) / length(.x), 3) * 100, 
      "%"
    )
  }
  
  stats_calc <- 
    stringr::str_split_fixed(input$summaryStatsPlotDataStatsType, "/", n = Inf) %>% 
    as_tibble() %>% 
    tidyr::pivot_longer(cols = names(.)) %>%
    distinct(value) %>% 
    pull(value)
    
  stats_calc <- stats_calc[stats_calc != ""]
  
  averages <- data_frame()
  
  for(i in threshStats){
    
    averages.i <- 
      summaryStatsDataList$summaryData %>% 
      group_by(summaryStatsPlotSplit) %>% 
      do({
        .in <- .
        
        .inVec <- .in[[toupper(i)]]
        
        .out <- data_frame(summaryStatsPlotSplit = unique(.in$summaryStatsPlotSplit))
        
        stats.i <- c()
        
        for(stat.i in stats_calc){
          stats.i <-
            c(
              stats.i,
              paste0(
                stat.i, ": ",
                signif(get(tolower(stat.i))(.inVec), 3)
              )
            )
        }

        
        .out[[paste0(toupper(i), " Stats")]] <- paste(stats.i, collapse = "<br>")
        
        rm(stats.i)
        
        .out[[paste0("Percent Above ", toupper(i), " Threshold")]] <-
          calc_thresh(.inVec, paste0(i, "Thresh"))
        
        .out
      }) %>% 
      ungroup()
    
    
    if(nrow(averages) > 0){
      averages <- full_join(averages, averages.i)
    } else {
      averages <- averages.i
    }
    
  }
  
  for(i in threshStats){
    
    if(is.na(summaryStatsDataList[[paste0(i, "Thresh")]])){
      
      averages[[paste0("Percent Above ", toupper(i), " Threshold")]] <- NULL
      
    } else {
      
      colnames(averages)[colnames(averages) == paste0("Percent Above ", toupper(i), " Threshold")] <- 
        paste0(
          "Percent Above ",
          toupper(i),
          " Threshold (",
          summaryStatsDataList[[paste0(i,"Thresh")]],
          ")"
        )
    }
  }
  
  colnames(averages)[(colnames(averages) == c("summaryStatsPlotSplit"))] <- "Summarize by"
  averages$`Summarize by` <- gsub("\n", "<br>", averages$`Summarize by`)
  
  summaryStatsDataList$averages <- averages
  
  ans$summaryStatsDataList <- summaryStatsDataList
  
  summaryStatsPlotData <- 
    summaryStatsDataList$summaryData %>%
    tidyr::gather(Stat, 
                  Value,
                  -contains("summaryStatsPlotSplit"), 
                  -summaryStatsPlotColor,
                  -ID,
                  -SIM_ID_and_ID)
  
  thresholds <- data_frame(Stat = toupper(threshStats))
  
  for(i in threshStats){
    
    thresholds$Threshold[thresholds$Stat == toupper(i)] <- 
      
      summaryStatsDataList[[paste0(i, "Thresh")]]
    
  }
  
  thresholds <- thresholds %>% tidyr::gather(Stat, Threshold)
  
  summaryStatsPlotData <- summaryStatsPlotData %>% left_join(thresholds)
  
  summaryStatsPlotOut <- 
    ggplot(
      summaryStatsPlotData %>% filter(Stat %in% input$summaryStatsPlotDataStats),
      aes(x = summaryStatsPlotSplit,
          y = Value,
          group = summaryStatsPlotSplit,
          fill = summaryStatsPlotColor)) +
    labs(x = "",
         y = "",
         fill = 
           ifelse(
             is.null(input$summaryStatsPlotDataSummarize),
             "",
             input$summaryStatsPlotDataColor
           )
    )
  
  
  
  if(length(unique(summaryStatsPlotData$summaryStatsPlotSplit)) > 4){
    summaryStatsPlotOut <- summaryStatsPlotOut + facet_wrap(~Stat, scales = "free", ncol = 1)
  } else {
    summaryStatsPlotOut <- summaryStatsPlotOut + facet_wrap(~Stat, scales = "free")
  }
  # summaryStatsPlotOut <- 
  #   summaryStatsPlotOut + facet_wrap(~Stat, scales = "free", nrow = 3)
  
  
  
  summaryStatsPlotOut <- 
    summaryStatsPlotOut + 
    theme_bw(base_size = 13) +
    geom_boxplot(alpha = .5, outlier.shape = NA) +
    theme(strip.text.x = element_text(size = 30)) +
    geom_hline(aes(yintercept = Threshold), linetype = "dashed")
  
  if(input$summaryStatsPlotLog == "Log"){
    summaryStatsPlotOut <- summaryStatsPlotOut + scale_y_log10()
  }
  
  if(input$summaryStatsPlotJitter){
    summaryStatsPlotOut <-
      summaryStatsPlotOut + 
      geom_jitter(aes(color = summaryStatsPlotColor), 
                  alpha = .75,
                  show.legend = FALSE,
                  height = 0)
  } 
  
  if(input$summaryStatsPlotDataColor == "no_color"){
    summaryStatsPlotOut <- 
      summaryStatsPlotOut +
      scale_color_manual(values = c("no_color" = "grey10")) +
      scale_fill_manual(values = c("no_color" = "grey10")) +
      theme(legend.position = "none")
  }
  
  ans$summaryStatsPlot <- summaryStatsPlotOut
  
  ans$summaryStatsAveragesTable <- 
    ans$summaryStatsDataList$averages %>% 
    select(
      c("Summarize by",
        contains(paste("Percent Above",input$summaryStatsPlotDataStats, "Threshold")),
        paste(input$summaryStatsPlotDataStats, "Stats"))
    )
  
  summaryStatsDataTable <- 
    ans$summaryStatsDataList$summaryData %>%
    select(-summaryStatsPlotColor) %>%
    select(-SIM_ID_and_ID)
  
  summaryStatsDataTable <- 
    summaryStatsDataTable %>%
    mutate(`Summarize by` = summaryStatsPlotSplit) %>%
    select(-summaryStatsPlotSplit) %>%
    left_join(
      rV$genpkSimList$mrgSimulations %>% distinct(ID, USUBJID)
    ) %>% 
    select(`Summarize by`, USUBJID, ID, everything()) %>% 
    mutate_at(vars(MAX:AUC), round, digits = 3)
  
  ans$summaryStatsDataTable <- summaryStatsDataTable
  
  ans$simPlotData <- simPlotData
  ans$randomSeed <- tibble::data_frame(`Random Seed` = as.character(rV$seed))
  
  if (!any(input$modelCovariates == "no_covariates")) {
    
    if (input$popDataType == "simPop") {
      
      ans$covariateSpecs <- get_covariate_specs(
        .input = reactiveValuesToList(input), 
        .rV = reactiveValuesToList(rV)
      )
      
      if (!is.null(input$correlationMatrix)) {
        ans$correlationMatrix <- data.frame(rhandsontable::hot_to_r(input$correlationMatrix))  
      }
      
    }
    
  }
  
  ans
  
}

