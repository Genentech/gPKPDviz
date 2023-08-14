sample_from_pop <- function(populationDataFull, r_seed, sampleFromPopSize){
  
  if(r_seed != "Random"){
    set.seed(as.integer(r_seed))
  } else {
    set.seed(as.integer(runif(1, 5, 500)))
  }
  
  idDF <- data_frame(USUBJID = populationDataFull$USUBJID) %>% distinct()
  
  idKeep <- sample_n(
    tbl = idDF,
    size = as.numeric(sampleFromPopSize),
    replace = as.numeric(sampleFromPopSize) > nrow(idDF)
  )
  
  ans <- data_frame()
  
  for(i in 1:nrow(idKeep)){
    
    ans <- 
      ans %>% 
      bind_rows(
        populationDataFull %>%
          filter(USUBJID == idKeep$USUBJID[i]) %>%
          select(-USUBJID, -ID) %>% 
          mutate(ID = as.integer(i))
      )
    
  }
  
  ans %>% 
    mutate(
      USUBJID = ID
    ) %>% 
    select(USUBJID, ID, everything())
  
}