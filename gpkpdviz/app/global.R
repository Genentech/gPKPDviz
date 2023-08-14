library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinyAce)
library(mrgsolve)
library(dmutate)
library(R6)
library(DT)

options(shiny.maxRequestSize=30*1024^2)

models <- list.files("models-library")
for(script.i in list.files("app-scripts")){
  source(file.path("app-scripts", script.i))
}
#source("functions.R")

##' Time classes and methods from metrumrg
#source("time.R")

##' continuous covariates
# source("covariate.R")
# .wt <- covariate$new("wt", min=40, max=140, mean=70, cv=30)
# .cont <- covariate$new("cont", min=10, max = 150, mean = 100, cv = 30)

##' Dosing interventions
#source("dosing.R")
.d1 <- dosing$new("first")
.d2 <- dosing$new("second")
.d3 <- dosing$new("third")

##' Dosing interventions
#source("period.R")
.p1 <- period$new("first",1)
.p2 <- period$new("second",2)
.p3 <- period$new("third",3)


logbr <- function() {
  x <- 10^seq(-10,10)
  c(x,3*x)
}

sptxt <- function(x) {
  x <- as.numeric(strsplit(x, "\\s+|,")[[1]])
  x[!is.na(x)]
}

spnl <- function(x) {
  x <- unlist(strsplit(x,"\n"),use.names=FALSE)
  x[!(x %in% c("", NA))]
}

dosing_event_names <- c(
  "1 (Dose)" = "1",
  "2 (Other type event)" = "2",
  "3 (System reset)" = "3",
  "4 (System reset and dose)" = "4",
  "8 (Replace the amount in a specific compartment)" = "8"
)

threshStats <- c(
  "auc", 
  "last",
  "max",
  "min"
)

# Averages calculations
`cv %` <- function(.x, na.rm = TRUE) {
  (sd(.x, na.rm = na.rm) / mean(.x, na.rm = na.rm)) * 100
}
`5th percentile` <- function(.x){quantile(.x, .05)}
`95th percentile` <- function(.x){quantile(.x, .95)}
`geo mean` <- function(x, na.rm=TRUE) {
  if (na.rm)
    x <- stats::na.omit(x)
  if (any(is.na(x))) {
    as.numeric(NA)
  } else if (any(x == 0)) {
    0
  } else if (any(x < 0)) {
    # Protect from overflows by using the logarithm
    prod(sign(x))*exp(sum(log(abs(x)))/length(x))
  } else {
    exp(sum(log(x))/length(x))
  }
}

`geo sd` <- function(x, na.rm=TRUE){
  exp(stats::sd(log(x), na.rm=na.rm))
}

`geo cv %` <- function(x, na.rm=TRUE){
  sqrt(exp(stats::sd(log(x), na.rm=na.rm)^2)-1)*100
}

summaryStatsPlotDataStatsChoices <- c("AUC", "LAST", "MAX", "MIN")

summaryStatsPlotDataStatsTypeChoices <- 
  c("Mean/SD", 
    "Median/CV %", 
    "Median/5th Percentile/95th Percentile", 
    "Geo Mean/Geo CV %")
