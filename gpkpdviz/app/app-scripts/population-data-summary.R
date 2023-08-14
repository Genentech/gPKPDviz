population_data_summary <- function(rV, input){
  
  if(is.null(rV$populationData)){
    return(NULL)
  }
  
  append_if_one <- function(.df, .covs){
    
    if(length(.covs) == 1){
      names(.df) <- paste(.covs, names(.df), sep = "_")
    }
    
    .df
  }
  
  NnotNA <- function(x, ...){sum(!is.na(x))}
  
  NProportions <- function(x, ...){
    
    tabX <- table(x)
    
    paste0(
      paste(names(tabX), " (",
            paste0(round(tabX / sum(tabX), 3) * 100, "%"),
            collapse = ")<br>", sep = ""
      ),
      ")"
    )
    
  }
  
  clean_NnotNA <- function(.df){
    .df %>% 
      mutate_at(vars(contains("_NnotNA")), funs(as.character(round(., 0))))
  }
  
  gather_vars <- function(.df){
    .df %>% 
      tidyr::gather(key = "key", value = "value") %>%
      tidyr::separate(key, c("Variable", "stat"), sep = "_") %>%
      tidyr::spread(stat, value)
  }
  
  summaries <- list()
  
  
  
  for(summary.i in c("all_obs", "baseline")){
    
    pop_data.i <- rV$populationData
    
    if(summary.i == "baseline"){
      pop_data.i <- 
        pop_data.i %>% 
        group_by(USUBJID) %>% 
        filter(time==min(time)) %>% 
        ungroup()
    }
    
    if(length(rV$contCovs) == 0){
      
      continuousSummary <- data_frame(` ` = "None")
      contCorr <- tibble(` ` = "")
      rownames(contCorr) <- "No continuous variables"
      
    } else {
      
      
      continuousSummary <-
        pop_data.i %>% 
        select(!!rV$contCovs) %>% 
        summarise_all(funs(NnotNA, mean, sd, `cv %`, min, max, median), na.rm = TRUE) %>% 
        append_if_one(., rV$contCovs) %>% 
        mutate_all(round, digits = 2) %>% 
        clean_NnotNA(.)
      
      
      if(length(rV$catCovs) > 0){
        
        continuousSummaryByCat <-
          pop_data.i %>% 
          select(!!c(rV$catCovs, rV$contCovs)) %>% 
          group_by_at(vars(rV$catCovs)) %>%
          summarise_all(funs(NnotNA, mean, sd, `cv %`, min, max, median), na.rm = TRUE) %>% 
          mutate_all(round, digits = 2) %>% 
          clean_NnotNA(.) %>% 
          ungroup()
        
        for(cat.i in rV$catCovs){
          continuousSummary[[cat.i]] <- "All"
        }
        
        continuousSummary <- continuousSummary %>% mutate_all(as.character)
        continuousSummaryByCat <- continuousSummaryByCat %>% mutate_all(as.character)
        
        continuousSummary <- bind_rows(continuousSummaryByCat, continuousSummary)
        
        continuousSummary <- continuousSummary %>% tidyr::pivot_longer(., -c(rV$catCovs))
        
        if (length(rV$contCovs) == 1) {
          
          continuousSummary$name <- paste0(rV$contCovs, "_", continuousSummary$name)
        }
        
        continuousSummary <- 
          continuousSummary %>% 
          mutate(
            Variable = stringr::str_split_fixed(name, "_", n = Inf)[,1],
            Stat = stringr::str_split_fixed(name, "_", n = Inf)[,2]
          ) %>% 
          select(-name) %>% 
          tidyr::pivot_wider(names_from = c("Stat"), values_from = "value")
        
        
      } else {
        
        continuousSummary <- 
          continuousSummary %>% 
          mutate_all(as.character) %>% 
          tidyr::pivot_longer(cols = names(.)) %>% 
          tidyr::separate(name, c("Variable", "Stat")) %>% 
          tidyr::pivot_wider(names_from = c("Stat"), values_from = "value")
      }
      
      
      # if(length(rV$contCovs) == 1){
      #   
      #   colnames(continuousSummary) <- 
      #     paste(rV$contCovs, "_", colnames(continuousSummary), sep = "")
      # }
      

      
      continuousSummary <-
        continuousSummary %>% 
        rename(`n obs` = NnotNA) %>% 
        arrange(Variable) %>% 
        select(everything(), `n obs`)
      
      
      if(length(rV$splitCovariates) > 0) {
        
        contCorr <- 
        pop_data.i %>% 
        group_by_at(vars(!!rV$catCovs)) %>% 
        select(!!rV$contCovs) %>%
        do({
          .in <- .
          .in %>% ungroup() %>% select(-(rV$catCovs)) %>% cor() %>% as_tibble()
        }) %>% 
        as.matrix()
      } else {
        contCorr <- 
          pop_data.i %>% 
          select(!!rV$contCovs) %>%
          cor()
      }
      
      if(length(rV$splitCovariates) & length(rV$contCovs) == 1){
        contCorr <- matrix(1, dimnames = list(rV$contCovs,rV$contCovs))
      }
    
    }

    summaries[[summary.i]]$continuousSummary <- continuousSummary
    summaries[[summary.i]]$contCorr <- contCorr
    
    rm(pop_data.i)
  }
  
  tagList(
    tags$h3(tags$b("Number of subjects: "), prettyNum(rV$nPopIDs, big.mark = ",")),
    tags$br(),
    tabBox(
      width = NULL,
      tabPanel(
        title = "Baseline",
        renderDT({
          plain_table(summaries$baseline$continuousSummary, .info = FALSE)
        }),
        tags$h5("Correlation Matrix"),
        rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(
            summaries$baseline$contCorr, 
            readOnly = TRUE,
            width = "100%"
          )
        })
      ),
      tabPanel(
        title = "Across Time",
        renderDT({
          plain_table(summaries$all_obs$continuousSummary, .info = FALSE)
        }),
        tags$h5("Correlation Matrix"),
        rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(
            summaries$all_obs$contCorr, 
            readOnly = TRUE,
            width = "100%"
          )
        })
      )
    )
  )
}
