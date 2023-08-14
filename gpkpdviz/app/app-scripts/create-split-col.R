create_split_col <- function(.df, plotSplit, yVar, .colname){
  
  for (var.i in yVar) {
    
    .df_var.i <- .df
    
    .df_var.i$Variable <- var.i
    .df_var.i$yVar <- .df_var.i[[var.i]]
    
    .df_var.i[yVar] <- NULL
    
    if (var.i == dplyr::first(yVar)) {
      
      .df_ans <- .df_var.i
      
    } else {
      
      .df_ans <- bind_rows(.df_ans, .df_var.i) 
      
    }
    
    rm(.df_var.i) 
  }
  
  rm(.df)
  
  if(length(plotSplit) == 0){
    .df_ans$`No Summarize` <- "(All Observations)"
    plotSplit <- "No Summarize"
  }
  
  plotSplit <- c(plotSplit, "Variable")
  
  plotSplitSym <- rlang::syms(plotSplit)
  
  .df_ans <-
    .df_ans %>% 
    left_join(
      .df_ans %>% 
        group_by(!!!plotSplitSym) %>% 
        summarise(
          `N Subjects` = length(unique(USUBJID))
        ) %>% 
        ungroup()
    )
  
  plotSplit <- c(plotSplit, "N Subjects")
  
  dense_rank_adj <- function(.x){
    
    if(is.factor(.x)) return(dense_rank(.x) * 1000)
    
    dense_rank(.x)
    
  }
  
  .df_ans$plotSplit <- paste0(plotSplit[1], ": ", .df_ans[[plotSplit[1]]])
  .df_ans$plotSplitRank <- dense_rank_adj(.df_ans[[plotSplit[1]]])
  
  for(i in 2:length(plotSplit)){
    .df_ans$plotSplit <- paste(
      .df_ans$plotSplit,
      paste0(plotSplit[i], ": ", .df_ans[[plotSplit[i]]]),
      sep = "\n"
    )
    
    .df_ans$plotSplitRank <- 
      .df_ans$plotSplitRank + dense_rank_adj(.df_ans[[plotSplit[i]]])
  }
  
  .df_ans$plotSplitRank <- dense_rank(.df_ans$plotSplitRank)
  
  .df_ans$plotSplit <- reorder(as.factor(.df_ans$plotSplit), .df_ans$plotSplitRank)
  
  .df_ans <- .df_ans %>% select(-contains("N Subjects"), -plotSplitRank)
  
  .df_ans[[.colname]] <- .df_ans$plotSplit
  .df_ans$plotSplit <- NULL
  
  .df_ans
}
