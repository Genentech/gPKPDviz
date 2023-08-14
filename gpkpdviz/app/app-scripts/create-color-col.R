create_color_col <- function(.df, plotColorVar, .colname){
  
  .df$plotColor <- .df[[plotColorVar]]
  
  if(grepl("Day", plotColorVar, fixed = TRUE) |
     grepl("Week", plotColorVar, fixed = TRUE) | 
     grepl("Cumulative Dose", plotColorVar, fixed = TRUE) |
     length(unique(.df$plotColor)) < 10){
    .df$plotColor <- factor(.df$plotColor)
  }
  
  .df[[.colname]] <- .df$plotColor
  .df$plotColor <- NULL
  
  .df
}