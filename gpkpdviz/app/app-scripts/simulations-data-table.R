simulations_data_table <- function(.simData){
  
  # Convert character and integer to factor for easy filtering
  colTypes <- 
    lapply(.simData %>% mutate_all(as.character), function(x){readr::guess_parser(x, guess_integer = TRUE)})
  
  toFactor <- names(colTypes)[colTypes %in% c("character", "integer")]
  
  for(col.i in toFactor){
    
    .simData[[col.i]] <- as.factor(.simData[[col.i]])
    
  }
  
  # Round numeric
  roundCols <- colnames(.simData)[!(colnames(.simData) %in% c("time", toFactor))]
  roundCols <- roundCols[roundCols != "covar_propo"] # manually remove
  
  datatable(
    .simData,
    class = "cell-border compact stripe",
    rownames = FALSE,
    filter = "top",
    colnames = c("Covariate Proportion" = "covar_propo"),
    options = list(
      scrollX = TRUE,
      pageLength = 3
    )
  ) %>% formatRound(., columns = roundCols, digits = 3)
}