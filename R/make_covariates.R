#' Create covariates for the experiment
#'
#' @param ... A list of DGP_objects (of the form variable = declare_DGP(...)), or functions that generate random numbers, of the form variable = function()rng(...)
#' @param design_object The object created by declare_design
#' @param N The number of observations.   
#' @export
#' 
#' 

declare_covariates <-
  function(...,
           design_object = NULL,
           N = design_object$N) {
                 variable_list <- list(...)
                 variable_names <- names(variable_list)
             make_X_matrix <-
               function(){
             
                 
                 
                 fun.list <- lapply(variable_list,function(variable) {
                   
                   if (!class(variable) %in% c("function","DGP_object")) {
                     stop(
                       "Variables should either be random number functions or of DGP_object (see declare_DGP())"
                     )
                   }
                   if (class(variable) == "function") {
                     return(variable)
                   }
                   if (variable$distribution == "normal") {
                     return(function(){rnorm(n=N,mean = variable$mean, sd = variable$sd)})
                   }
                   if (variable$distribution == "binary") {
                     return(function(){binom_out <- rbinom(n = N,size = 1,prob =variable$probability);if(!is.null(variable$categories)){binom_out <- factor(binom_out,c(0,1),variable$categories)};return(binom_out)})
                   }
                   if (variable$distribution == "multinomial") {
                     return(function(){multinom_out <- apply(rmultinom(n = N,size = 1,prob =variable$probability),2,function(i)which(i==1));if(!is.null(variable$categories)){multinom_out <- factor(multinom_out,levels = 1:length(variable$categories),labels = variable$categories)};return(multinom_out)})
                   }
                 })
                 
                 x <- lapply(fun.list,function(f)f())
                 X.char <- do.call(cbind.data.frame,x)
                 X <- data.frame(matrix(rep(NA,dim(X.char)[1]*dim(X.char)[2]),nrow = dim(X.char)[1]))
                 for(i in 1:dim(X.char)[2]){
                   X[,i] <- X.char[i][,1]
                 }
                 names(X) <- variable_names
                 return(X)
               }
             
             covariate_object <- list(make_X_matrix = make_X_matrix,
                                      variable_names = variable_names,
                                      call = match.call())
             
             class(covariate_object) <- "covariate_object"
             return(covariate_object)
           }

#' @export
make_covariates <- function(covariates) {
  return(covariates$make_X_matrix())
}

