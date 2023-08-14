build_timeframe_groupings <- function(dose_table, current_groupings){
  
  intervals <- dose_table %>% select(contains("Dose Interval"))
  
  timeframeGroupingChoices <- c()

  for(i in 1:nrow(intervals)){

    val.i <- paste0(
      intervals[i, ],
      " ",
      gsub(")", "", unlist(strsplit(colnames(intervals), "(", fixed = TRUE))[2], fixed = TRUE),
      "s (Dosing Interval)"
    )

    if(val.i %in% timeframeGroupingChoices){
      next
    }

    timeframeGroupingChoices <- c(
      timeframeGroupingChoices,
      val.i
    )
    rm(val.i)
  }

  unique(c(timeframeGroupingChoices, current_groupings))
}