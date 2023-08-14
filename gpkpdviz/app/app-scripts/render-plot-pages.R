render_plot_pages <- function(.plot_list, session){
  
  plots_show <- names(.plot_list)[-1]
  
  names(plots_show) <- split_and_grab(plots_show, "-", 2) %>% split_and_grab(., "of", 1)
  
  plots_show <- plots_show[order(as.numeric(names(plots_show)))]
  
  updateRadioButtons(
    session,
    inputId = "simulationsPlotPage",
    choices = plots_show,
    selected = plots_show[1], inline = TRUE
  )
  
  ans <- tagList()
  
  for(i in 2:length(.plot_list)){
    name.i <- names(.plot_list)[[i]]
    ans <- 
      tagAppendChild(
        ans, 
        conditionalPanel(
          paste0("input.simulationsPlotPage == '", name.i, "'"),
          plotOutput(name.i, height = "825px")
        )
      )
    rm(name.i)
  }
  
  ans
}