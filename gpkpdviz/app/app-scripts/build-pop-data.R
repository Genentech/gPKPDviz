build_pop_data <- function(input, rV){
  
  errorMess <- NULL
  
  populationData <- data_frame(USUBJID = as.integer(1:as.numeric(input$nPopDataSim)),
                               ID = as.integer(1:as.numeric(input$nPopDataSim)))
  
  contVars <- data_frame()
  
  splits <- 
    stringr::str_split_fixed(rV$splitCovariates, "by", n = Inf) %>% 
    as_tibble()
  
  colnames(splits) <- c("covariate", "split")
  
  for(cov.i in input$modelCovariates){
    
    
    if(cov.i %in% rV$contCovs){
      
      
      if(cov.i %in% splits$covariate){
        
        cov_names.i <- 
          splits %>% filter(covariate==cov.i) %>% 
          pull(split) %>% 
          parse_covariate_values(., input)
        
      } else {
        cov_names.i <- list("ALLCATS")
      }
      
      
      
      contVar.i <-
        
        purrr::map_df(
          cov_names.i, ~ 
            tibble(
              VAR = cov.i,
              VAR_CAT = glue::glue("{cov.i}_{.x}"),
              MEAN = as.numeric(input[[glue::glue("{cov.i}_{.x}_mean")]]),
              SD = as.numeric(input[[glue::glue("{cov.i}_{.x}_sd")]]),
              LOWER =  as.numeric(input[[glue::glue("{cov.i}_{.x}_lower")]]),
              UPPER =  as.numeric(input[[glue::glue("{cov.i}_{.x}_upper")]])
            )
        )
      
      
      if(any(is.na(contVar.i))){
        errorMess <- paste0("Non-numeric found in: ", cov.i)
        stop(errorMess)
      }
      
      contVars <- bind_rows(
        contVars,
        contVar.i
      )
      
      
      next
      
      
    } else {
      
      cov_data_cats.i <- 
        trimws(unlist(strsplit(input[[paste0(cov.i, "cats")]], ",")))
      
      cov_data_cats.i <- as.numeric(cov_data_cats.i)
      
      if(any(is.na(cov_data_cats.i))){
        errorMess <- paste0("Categorical variables must be numeric values (", cov.i, ")")
        stop(errorMess)
      }
      
      cov_data_probs.i <- 
        as.numeric(trimws(unlist(strsplit(input[[paste0(cov.i, "probs")]], ","))))
      
      cov_data_probs.i <- cov_data_probs.i / sum(cov_data_probs.i)
      
      cov_vector.i <- rep(cov_data_cats.i,
                          round(cov_data_probs.i * nrow(populationData), 0))
      
      if(rV$seed != "Random"){
        set.seed(as.integer(rV$seed))
      } else {
        set.seed(as.integer(runif(1, 5, 500)))
      }
      
      cov_data.i <- data_frame(
        X = cov_vector.i[sample(1:nrow(populationData), replace = FALSE)]
      )
      
      rm(cov_vector.i)
    }
    
    colnames(cov_data.i) <- cov.i
    
    populationData <- bind_cols(populationData, cov_data.i)
    
    rm(cov_data.i)
    
  }
  
  
  if(nrow(contVars) > 0){
    
    # sigma <- matrix(c(4,2,2,3), ncol=2)
    
    if(rV$seed != "Random"){
      set.seed(as.integer(rV$seed))
    } else {
      set.seed(as.integer(runif(1, 5, 500)))
    }
    
    correlationMatrix <- 
      as.matrix(rhandsontable::hot_to_r(input$correlationMatrix))
    rownames(correlationMatrix) <- colnames(correlationMatrix)
    correlationMatrix <- as.matrix(Matrix::forceSymmetric(correlationMatrix, uplo="L"))
    
    
    catContCombos <-
      expand.grid(contVars$VAR_CAT, contVars$VAR_CAT, stringsAsFactors = FALSE) %>%
      filter(Var1 != Var2) %>% 
      rowwise() %>%
      mutate(Var3 = paste0(sort(c(Var1, Var2)), collapse = "___")) %>%
      distinct(Var3) %>% 
      pull(Var3)
    
    if(length(rV$splitCovariates) == 0){
      
      catContCombos <- 
        paste(sort(unique(unlist(strsplit(catContCombos, "___")))), collapse = "___")
    }
    
    if(length(unique(contVars$VAR)) == 1){
      
      catContCombos <- contVars$VAR_CAT
      
    }
    
    continous_randoms_list <- list()
    
    for(i in 1:length(catContCombos)){
      
      var_cats.i <- (catContCombos[i] %>% strsplit(., "___", fixed=TRUE) %>% unlist())
      
      contVars.i <- contVars %>% filter(VAR_CAT %in% var_cats.i)
      
      if (length(unique(contVars.i$VAR)) == 1 & length(unique(contVars$VAR)) != 1) {
        next
      }
      
      sdVector.i <- contVars.i$SD
      
      if(length(sdVector.i) > 1) {
        
        correlationMatrix.i <- correlationMatrix[contVars.i$VAR, contVars.i$VAR]
        covarianceMatrix.i <- diag(sdVector.i)%*%correlationMatrix.i%*%diag(sdVector.i)
        
        rownames(covarianceMatrix.i) <- rownames(correlationMatrix.i)
        colnames(covarianceMatrix.i) <- colnames(correlationMatrix.i)
        
      } else {
        covarianceMatrix.i <- as.matrix(sdVector.i^2)
      }
      
      
      continous_randoms_mat <- try(tmvtnorm::rtmvnorm(
        n = nrow(populationData), 
        mean = contVars.i$MEAN, 
        sigma = covarianceMatrix.i,
        lower = contVars.i$LOWER,
        upper = contVars.i$UPPER
      ))
      
      if(inherits(continous_randoms_mat, "try-error")){
        errorMess <- as.character(continous_randoms_mat)
        stop(errorMess)
      }
      
      continous_randoms <- as_tibble(continous_randoms_mat)
      
      colnames(continous_randoms) <- (contVars.i$VAR_CAT)
      
      continous_randoms_list[[paste0(sort(contVars.i$VAR_CAT), collapse = "___")]] <- continous_randoms
      
      rm(continous_randoms)
    }
    
    
    if(length(rV$catCovs) > 0){
      
      for(covar.i in rV$contCovs){
        populationData[[covar.i]] <- NA_real_
      }
      
      for(j in 1:nrow(populationData)){
        
        cat_values.j <- 
          populationData %>%
          slice(j) %>%
          select(!!rV$catCovs) %>% 
          tidyr::pivot_longer(cols = rV$catCovs) %>%
          arrange(name) %>% 
          rowwise() %>% 
          mutate(name_value = paste0(name, "_", value)) %>% 
          ungroup() %>% 
          pull(name_value) %>% 
          paste0(., collapse = "__")
        
        covar_list_names.j <- c()
        
        for(covar.j in rV$contCovs){
          
          if(covar.j %in% splits$covariate){
            
            covar_list_name.j <- paste0(covar.j, "_", cat_values.j)
            
          } else {
            
            covar_list_name.j <- paste0(covar.j, "_", "ALLCATS")
          }
          
          covar_list_names.j <- sort(c(covar_list_names.j, covar_list_name.j))
          
        }
        
        covar_list_name_final.j <- paste0(covar_list_names.j, collapse = "___")
        
        continous_randoms_list.j <- continous_randoms_list[[covar_list_name_final.j]] %>% slice(j)
        
        for(col.j in colnames(continous_randoms_list.j)){
          
          var.j <- stringr::str_split_fixed(col.j, "_", n=Inf)[1,1]
          
          populationData[[var.j]][j] <- continous_randoms_list.j[[col.j]]
        }
        rm(continous_randoms_list.j)
        
      }
      
    } else {
      populationData <- populationData %>% cbind(continous_randoms_list[[1]])
      
      names(populationData) <- gsub("_ALLCATS", "", names(populationData), fixed=TRUE)
    }
    
  
  }
  
  
  
  if(!is.null(errorMess)){
    stop(errorMess)
  } else {
    return(populationData %>% select(-contains("_")) %>% mutate(time=0))
  }
}