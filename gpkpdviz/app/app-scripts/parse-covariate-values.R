parse_covariate_values <- function(.categorical_covariates, input){
  
  inputs_df <- 
    purrr::map_df(
      .categorical_covariates,
      ~ tibble(
        NAME = .x,
        VALUES = trimws(unlist(strsplit(input[[glue::glue(.x, "cats")]], ",", fixed = TRUE)))
      )
    )
  
  
  input_combos <- expand.grid(
    split(inputs_df$VALUES, inputs_df$NAME),
    stringsAsFactors = FALSE
  )
  
  
  
  combos_list <- 
    purrr::map(
      1:nrow(input_combos), ~ 
        paste(names(input_combos), input_combos[.x,], sep = "_", collapse = "__")
    )
  
  return(combos_list)
  
}
