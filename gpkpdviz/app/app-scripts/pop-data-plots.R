pop_data_plots <- function(.pop_data, .model_time){
  
  .pop_data_long <- 
    .pop_data %>% 
    select(-contains("(Bins)")) %>% 
    tidyr::gather(key, value, -USUBJID, -ID, -time)
  
  ans <- list()
  
  xLab <- paste0("time "," (in ", .model_time, "s - model time unit)")
  
  .ans_font_size <- min(
    ceiling(max(60 / length(unique(.pop_data_long$key)), 13)),
    18
  )
  
  ans$cov_ex_data_baseline_hist <- 
    .pop_data_long %>% 
    group_by(USUBJID) %>% 
    mutate(is_bl = time == min(time)) %>% 
    ungroup() %>% 
    filter(is_bl) %>% 
    select(-is_bl) %>% 
    ggplot(., aes(x = value, fill = key)) +
    geom_histogram(color = "black") +
    facet_wrap(~key, scales = "free") +
    theme_bw(base_size = .ans_font_size) +
    theme(legend.position = "none") + 
    labs(title = "First (time) obs. per ID = baseline value",
         caption = "*Covariates with a single unique value result in a blank canvas")
  
  ans$cov_ex_data_time_var <-
    .pop_data_long %>% 
    ggplot(., aes(x = time, y = value, color = key, group = ID)) +
    geom_line() + 
    geom_point() + 
    facet_wrap(~key, scales = "free") +
    xlab(paste0("time "," (in ", .model_time, "s - model time unit)")) +
    theme_bw(base_size = .ans_font_size) +
    theme(legend.position = "none")
  
  
  ans
}