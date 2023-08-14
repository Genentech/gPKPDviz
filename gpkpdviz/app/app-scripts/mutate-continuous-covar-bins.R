mutate_continuous_covar_bins <- function(.df, input){
  
  if(is.null(.df)) return(NULL)
  if(nrow(.df) == 0) return(NULL)
  
  n_bin <- input$continuousCovariatesNBins
  
  .min_time_df <- 
    .df %>% group_by(USUBJID) %>% filter(time == min(time)) %>% ungroup()
  
  covar <- input$continuousCovariatesToBin
  
  .df <- .df %>% select(-contains(paste0(covar, " (Bins)")))
  
  if(input$continuousCovariatesBinType == "n_bins"){
    
    cut_try <- 
      try(
        ggplot2::cut_number(
          x = .min_time_df[[covar]],
          n = n_bin
        )
      )
    
    if(class(cut_try) == "try-error"){
      
      stop(as.character(cut_try))
      
    } else {
      
      .min_time_df[[paste0(covar, " (Bins)")]] <- cut_try
      
    }
  }
  
  if(input$continuousCovariatesBinType == "breaks"){
    
    .min_time_df[[paste0(covar, " (Bins)")]] <- 
      cut(
        .min_time_df[[covar]],
        breaks = sort(
          unique(
            c(
              min(.min_time_df[[covar]], na.rm = TRUE),
              as.numeric(trimws(unlist(strsplit(input$continuousCovariatesBreaks, ",")))),
              max(.min_time_df[[covar]], na.rm = TRUE)
            )
          )
        ),
        include.lowest = TRUE
      )
  }
  
  .min_time_df <- 
    .min_time_df %>% select(USUBJID, contains("(Bins)")) %>% distinct()
  
  .df %>% left_join(.min_time_df)
}