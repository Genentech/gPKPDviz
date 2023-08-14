correlation_matrix_ui <- function(input, contVars){
  
  if(any(input$modelCovariates == "no_covariates")){
    return(NULL)
  }
  
  # contVars <- rV$contCovs
  
  if(length(contVars) == 0){
    return(rhandsontable::rhandsontable(data_frame()))
  }
  
  corrMat <- diag(length(contVars))#  %>% as.data.frame()
  
  colnames(corrMat) <- contVars
  rownames(corrMat) <- contVars
  
  rhandsontable::rhandsontable(corrMat) %>%
    rhandsontable::hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row == col & value == 1) {
              td.style.background = 'lightgrey';
             } else if (row == col & value != 1) {
                td.style.background = 'red';
              td.style.color = 'white';
             } else if (col > row) {
              td.style.background = 'grey';
              td.style.color = 'grey';
             } else if (value > 1) {
              td.style.background = 'red';
              td.style.color = 'white';
             }
           }")
}