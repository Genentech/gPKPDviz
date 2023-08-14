

library(mrgsolve)
library(dplyr)

mod <- mread('genpk', "models-library/") %>% zero_re
see(mod)



idata <- data_frame(F1 = rev(seq(0.1,1,0.1)))
idata <- data_frame(WT = seq(40,140,40), WTCL = 0, WTVP = 0, WTQ = 1, WTVC=0,Q=1)
mod %>% ev(amt=100, ii=24, addl=4) %>% mrgsim(end=240,idata=idata) %>% plot

