app_table <- function(.df, page_len = 10, .filter = "none", .columnDefs = list(list())){
  datatable(
    .df,
    class = "cell-border compact stripe",
    rownames = FALSE,
    escape = FALSE,
    filter = .filter,
    options = list(
      scrollX = TRUE,
      pageLength = page_len,
      columnDefs = .columnDefs
    )
  )
}

plain_table <- function(.df, .columnDefs = NULL, .info = TRUE){

  if(is.null(.columnDefs)){
    ..columnDefs <- list(list(className = 'dt-center', targets = "_all"))
  } else {
    ..columnDefs <- list(.columnDefs, list(className = 'dt-center', targets = "_all"))
  }
  
  datatable(
    .df,
    escape = FALSE,
    rownames = FALSE,
    class = "cell-border compact striped",
    options = list(paging = FALSE,
                   ordering = FALSE,
                   searching = FALSE,
                   scrollX = TRUE,
                   info = .info,
                   columnDefs = ..columnDefs)
  )
}
