#' Create covariates for the experiment
#'
#' @param ... A list of DGP_objects (of the form variable = declare_DGP(...)), or functions that generate random numbers, of the form variable = function()rng(...)
#' @param design_object The object created by declare_design
#' @param N The number of observations.   
#' @export

make_covariates <-
  function(...,
           design_object = NULL,
           N = design_object$N) {
             variable_list <- list(...)
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
                 return(eval(
                   parse(text =
                           "function(){rnorm(n=N,mean = variable$mean, sd = variable$sd)}")
                 ))
               }
               if (variable$distribution == "binary") {
                 return(eval(
                   parse(text =
                           "function(){binom_out <- rbinom(n = N,size = 1,prob =variable$probability);if(!is.null(variable$categories)){binom_out <- factor(binom_out,c(0,1),variable$categories)};return(binom_out)}")
                 ))
               }
               if (variable$distribution == "multinomial") {
                 return(eval(
                   parse(text =
                           "function(){multinom_out <- apply(rmultinom(n = N,size = 1,prob =variable$probability),2,function(i)which(i==1));if(!is.null(variable$categories)){multinom_out <- factor(multinom_out,levels = 1:length(variable$categories),labels = variable$categories)};return(multinom_out)}")
                 ))
               }
             })
             make_X_matrix <-
               function()as.data.frame(lapply(fun.list,function(f)f()))
             variable_names <- names(variable_list)
             covariate_object <- list(make_X_matrix = make_X_matrix,
                                      variable_names = variable_names,
                                      call = match.call())
             class(covariate_object) <- "covariate_object"
             return(covariate_object)
           }

# Demo

covariate_object_1 <- make_covariates(
  X1 = declare_DGP(),
  X2 = declare_DGP(),
  event = declare_DGP(binary_probability = .5,
                      binary_categories = c("happened","did not")),
  income = function()rnorm(n = design_object$N,mean = 0,sd = 1),
  count = function()rpois(n = design_object$N,lambda = 30),
  party_id = declare_DGP(
    multinomial_probabilities = c(.4,.4,.2),
    multinomial_categories = c("D","R","I")),
  design_object = design_object
)





