as_time <- function(x,unit) {
  do.call(paste0("as.",unit),list(x))
}

to_model_time <- function(x,unit,input) {
  unclass(as_time(as_time(x,unit),input$model_time))
}

get_obs_duration <- function(input) {
  as_time(as_time(input$observation_time_value,
                  input$observation_time_unit),
          input$model_time_unit)
}

as.second         <- function(x,...)UseMethod('as.second')
as.minute         <- function(x,...)UseMethod('as.minute')
as.hour           <- function(x,...)UseMethod('as.hour')
as.day            <- function(x,...)UseMethod('as.day')
as.week           <- function(x,...)UseMethod('as.week')
as.month          <- function(x,...)UseMethod('as.month')
as.year           <- function(x,...)UseMethod('as.year')

as.second.numeric <- function(x,...)structure(x,class=c('second','duration','timeline',class(x)))
as.minute.numeric <- function(x,...)structure(x,class=c('minute','duration','timeline',class(x)))
as.hour.numeric   <- function(x,...)structure(x,class=c('hour','duration','timeline',class(x)))
as.day.numeric    <- function(x,...)structure(x,class=c('day','duration','timeline',class(x)))
as.week.numeric   <- function(x,...)structure(x,class=c('week','duration','timeline',class(x)))
as.month.numeric  <- function(x,...)structure(x,class=c('month','duration','timeline',class(x)))
as.year.numeric   <- function(x,...)structure(x,class=c('year','duration','timeline',class(x)))

format.duration <- function(x,...)as.numeric(x)
print.duration <- function(x,...)print(format(x))

as.second.minute  <- function(x,...)as.second(as.numeric(x*60))
as.minute.second  <- function(x,...)as.minute(as.numeric(x/60))
as.minute.hour    <- function(x,...)as.minute(as.numeric(x*60))
as.hour.minute    <- function(x,...)as.hour(as.numeric(x/60))
as.hour.day       <- function(x,...)as.hour(as.numeric(x*24))
as.day.hour       <- function(x,...)as.day(as.numeric(x/24))
as.day.week       <- function(x,...)as.day(as.numeric(x*7))
as.week.day       <- function(x,...)as.week(as.numeric(x/7))
as.day.month      <- function(x,...)as.day(as.numeric(x*28))
as.month.day      <- function(x,...)as.month(as.numeric(x/28))
as.day.year       <- function(x,...)as.day(as.numeric(x*365.25))
as.year.day       <- function(x,...)as.year(as.numeric(x/365.25))

as.second.second  <- function(x,...)x
as.minute.minute  <- function(x,...)x
as.hour.hour      <- function(x,...)x
as.day.day        <- function(x,...)x
as.week.week      <- function(x,...)x
as.month.month    <- function(x,...)x
as.year.year      <- function(x,...)x
as.second.hour    <- function(x,...)as.second(as.minute(x))
as.second.day     <- function(x,...)as.second(as.hour(x))
as.second.duration <- function(x,...)as.second(as.day(x))
as.minute.duration <- function(x,...)as.minute(as.second(x))
as.hour.second    <- function(x,...)as.hour(as.minute(x))
as.hour.duration   <- function(x,...)as.hour(as.day(x))                                       
as.day.duration    <- function(x,...)as.day(as.hour(x))                                      
as.week.duration   <- function(x,...)as.week(as.day(x))                                  
as.month.duration  <- function(x,...)as.month(as.day(x))
as.year.duration   <- function(x,...)as.year(as.day(x))

