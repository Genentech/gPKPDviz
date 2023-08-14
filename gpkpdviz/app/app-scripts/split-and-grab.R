split_and_grab <- function(.x, .split, .grab){
  unlist(lapply(strsplit(.x, .split, fixed = TRUE), function(x){x[.grab]}))
}
