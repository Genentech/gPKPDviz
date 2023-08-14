
sig <- function(x,dig=3,maxex=NULL) {
  namez <- names(x)
  x <- formatC(signif(x,digits=dig), digits=dig, format='g', flag='#')
  
  if(is.numeric(maxex)) {
    if(dig!=maxex) {
      ex <- "([-]*[0-9]\\.[0-9]+)e([+-][0-9]{2})"
      subit <- grepl(ex,x,perl=TRUE)
      a <- as.numeric(gsub(ex, "\\1", x))
      b <- as.numeric(gsub(ex, "\\2", x))
      subit <- subit & abs(b) < maxex
      x <- ifelse(subit,formatC(signif(as.numeric(x),digits=dig),digits=dig, format="fg",flag="#"),x)
    }
  }
  x <- gsub("\\.$", "", x, perl=TRUE)
  names(x) <- namez
  return(x)
}

stile <- function(x,y,maxex=6,...) sig(quantile(x,y),maxex=maxex,...)

obs_days <- function(days, delta=4) {
  des <- seq(0,24,delta)
  des[des == 24] <- 23.99999 # used in floor function when calculating data
  sort(unique(sapply(24*(days-1), FUN=function(d) des+d)))
}
