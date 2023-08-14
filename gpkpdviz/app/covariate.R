# covariate_ui <- function(id, range, cv, mean, values) {
#   
#   stopifnot(length(range)==3)
#   stopifnot(length(cv)==2)
#   stopifnot(length(cv)==2)
#   
#   pick_limits <- sliderInput(
#     inputId = paste0(id,"__range"),
#     label="Range",
#     value = values[c(1,2)], 
#     min = range[1],  
#     max = range[2], 
#     step = range[3]
#   )
#   pick_mean <- numericInput(
#     inputId = paste0(id,"__mean"),
#     label = "Median", 
#     value = values[3], 
#     min = mean[1], 
#     max = mean[2]
#   )
#   pick_cv <- numericInput(
#     inputId = paste0(id,"__cv"),
#     label = "CV Percent",
#     value = values[4], 
#     min = cv[1], 
#     max = cv[2]
#   )
#   pick_log <- checkboxInput(
#     inputId=paste0(id,"__log"),
#     label="Log Normal",
#     value=as.logical(values[5])
#   )
#   tagList(
#     fluidRow(column(6,pick_mean),column(6,pick_cv)),
#     fluidRow(column(8,pick_limits),column(4,br(),br(),pick_log) )
#   )
# }
# 
# 
# covariate <- R6Class("covariate",
#                      public = list(
#                        name = NULL,
#                        min = NULL,
#                        max = NULL,
#                        mean = NULL,
#                        cv = NULL,
#                        log = NULL,
#                        initialize = function(name, min, max, mean = 70, cv = 30, log = FALSE) {
#                          self$name <- name
#                          self$min <- min
#                          self$max <- max
#                          self$mean <- mean
#                          self$cv <- cv
#                          self$log <- log
#                        }, 
#                        get_dist = function() {
#                          if(self$log==TRUE) {
#                            return("rlnorm") 
#                          } else {
#                            return("rnorm")
#                          }
#                        },
#                        make_ui = function(range,mean=c(0,0,0),cv = c(0,0,0)) {
#                          covariate_ui(self$name,range=range,mean=mean,cv=cv,
#                                       values=c(self$min,self$max,self$mean,
#                                                self$cv,self$log))
#                        },
#                        get_sd = function() {
#                          if(self$log==TRUE) {
#                            return(signif(self$cv/100,5))
#                          } else {
#                            return(signif(self$mean*self$cv/100,5))
#                          }
#                        },
#                        get_mu = function() {
#                          if(self$log==TRUE) {
#                            return(signif(log(self$mean))) 
#                          } else {
#                            return(signif(self$mean))
#                          }
#                        }, 
#                        inputs = function() {
#                          paste0(self$name, "__", c("range", "mean", "cv", "log"))
#                        }, 
#                        get_input = function(input) {
#                          input <- reactiveValuesToList(input)
#                          x <- input[self$inputs()]
#                          self$min <- as.numeric(x[[1]][1])
#                          self$max <- as.numeric(x[[1]][2])
#                          self$mean <- as.numeric(x[[2]])
#                          self$cv <- as.numeric(x[[3]])
#                          self$log <- as.logical(x[[4]])
#                        }
#                      )
# )
# 
# 
# ##' Form the left hand side of the formula
# lhs <- function(var,lower,upper,limits=TRUE) {
#   if(!limits) return(var)
#   paste0(var,"[", lower, ',', upper, "]")
#   
# }
# ##' Form the right hand side of the formula
# rhs <- function(dist,...) {
#   args <- list(...)
#   args <- paste(args,collapse=",")
#   paste0(dist, "(", args, ")")
# }
# 
# ##' Simulate covariates
# sim_covariates <- function(data,input) {
#   
#   wt <- .wt$get_input(input)
#   cont <- .cont$get_input(input)
#   validate(need(.wt$mean  >= .wt$min & .wt$mean <= .wt$max,
#                 message="Please make the mean weight greater than the min and less than max"))
#   validate(need(.cont$mean >= .cont$min & .cont$mean <= .cont$max,
#                 message="Please make the mean cont greater than the min and less than max"))
#   
#   a <- lhs("WT",   .wt$min,   .wt$max)
#   b <- lhs("CONT", .cont$min, .cont$max)
#   left <- paste0(a,"+",b)
#   
#   mu <- c(.wt$mean,.cont$mean)
#   sig1 <- .wt$cv/100
#   sig2 <- .cont$cv/100
#   
#   if(.wt$log) {
#     mu <- log(mu)
#     right <- "~rlmassnorm(mu,Sigma)"
#   } else {
#     sig1 <- sig1*mu[1]
#     sig2 <- sig2*mu[2]
#     right <- "~rmassnorm(mu,Sigma)"
#   }
#   Sigma <- cmat(sig1^2,input$weight_cont_cor,sig2^2)
#   P <- as.numeric(input$percent_cat)/100
#   cov1 <- paste0(left,right)
#   cov2 <- "CAT ~ rbinomial(P)"
#   env <- list(mu=mu,Sigma=Sigma,P = P)
# 
#   data <- try(dmutate::mutate_random(data,covset(cov1,cov2),envir=env))
#   validate(need(class(data) != "try-error", message="Please adjust covariate settings"))
#   return(data)
# }
