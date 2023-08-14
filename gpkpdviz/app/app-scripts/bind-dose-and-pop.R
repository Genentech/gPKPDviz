bind_dose_and_pop <- function(x, pop_dat){
  
  x <- x %>% left_join(pop_dat %>% select(USUBJID, ID) %>% distinct())
  x <- bind_rows(x %>% mutate(REC_TYPE = 1),
                 pop_dat %>% mutate(evid=2, amt=0, REC_TYPE = 0, covar_propo = ""))
  
  sort_and_fill_covars(x)
}