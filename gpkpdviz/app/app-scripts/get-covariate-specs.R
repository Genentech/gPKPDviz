get_covariate_specs <- function(.input, .rV){
  
  covSpecs <- c("_CovariateType", "cats", "probs", "_mean", "_sd", "_lower", "_upper")
  
  needed_inputs <-
    purrr::map(
      covSpecs, ~ names(.input)[stringr::str_ends(names(.input), .x)]
    ) %>% 
    unlist()
  
  if (length(needed_inputs) == 0) {
    return(tibble::data_frame())
  }
  
  input_df <- bind_rows(.input[needed_inputs]) %>% tidyr::pivot_longer(names(.))
  
  if (nrow(input_df) == 0) {
    return(tibble::data_frame())
  }
  
  input_df <- input_df %>% arrange(name)
  
  cov_names <- list()
  
  for (cov.i in .input$modelCovariates) {
    
    cov_type.i <- 
      input_df %>% 
      filter(name == paste0(cov.i, "__CovariateType")) %>% 
      pull(value)
    
    if (cov_type.i == "Categorical") {
      
      cov_names[[cov.i]] <- 
        ifelse(
          (input_df$name %in% c(paste0(cov.i, "cats"), paste0(cov.i, "probs"))) |
            (input_df$name == paste(cov.i, "__CovariateType", sep = "")),
          cov.i,
          ""
        )
      
    }
    
    if (cov_type.i == "Continuous") {
      
      cov_names[[cov.i]] <- 
        ifelse(
          (substr(input_df$name, 1, nchar(cov.i) + 1) == paste0(cov.i, "_")) |
            (input_df$name == paste(cov.i, "__CovariateType", sep = "")),
          cov.i,
          ""
        )
      
    }
    
  }
  
  input_df$covariate <-
    bind_rows(cov_names) %>% 
    tidyr::unite(covariate, names(.), sep = "") %>% 
    pull(covariate)
  
  input_df <-
    input_df %>% 
    filter(covariate != "") %>% 
    select(covariate, name, value) %>% 
    arrange(covariate, name)
  
  
  if (!is.null(.rV$splitCovariates)) {
    input_df <-
      bind_rows(
        tibble::data_frame(
          covariate = "",
          name = "Covariates Split", 
          value = .rV$splitCovariates
        ),
        input_df
      )
  }
  
  input_df <-
    bind_rows(
      tibble::data_frame(
        covariate = "",
        name = "Covariates in Model", 
        value = paste0(.input$modelCovariates, collapse = "_")
      ),
      input_df
    )
  
  input_df$value <- gsub("-Inf", "Negative Inf", input_df$value, fixed = TRUE)
  
  return(input_df)
}
