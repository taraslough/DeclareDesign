#' Declare an experimental analysis
#'
#' Description
#' @param method either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm
#' @param test_result a function that extracts the binary result of a statistical test (did it pass = 1, if not = 0)
#' @param formula an optional formula object to define analyses such as linear regressions with covariates
#' @param ... additional options to be sent to the analysis function and the test_result function
#' @return a list containing a function to conduct the analysis and a function to extract the result of the test
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @export
declare_analysis <- function(method, test_result, alpha = .05, ...){
  
  if(class(method) == "character") {    
    ## if user provides a string as a method, this invokes default analyses we define
    if(method == "lm")
      analysis <- function(formula, data, ...) lm(formula = formula, data = data, ... = ...)
    else if(method == "glm")
      analysis <- function(formula, data, family, ...) glm(formula = formula, data = data, family = family, ... = ...)
    
  }
  
  if(class(test_result) == "character" | !exists(test_result)){
    ## if user provides a string, this invokes default test_result functions
    if(!exists(test_result) & (method == "lm" | method == "glm"))
      test_result <- "first-coefficient-significant"
    
    if(test_result == "first-coefficient-significant")
      test_result <- function(results, alpha) summary(results)$coefficients[k,4] < alpha
    ## note this code definitely works for lm, glm
  }
  
  return.object <- list(analysis = analysis, test_result = test_result, call = match.call())
  
  class(return.object) <- "analysis"
  
  return()
  
}

## NOTE this does not work yet, until we get the design and data objects up and running
## once it does it will work like this:
##
## analysis_1 <- declare_analysis(method = "lm-no-covariates") ## creates an object with a function to run a diff-in-means analysis and a function to extract p-value
## run_analysis(analysis_1) ## in this case returns an lm object alone
## get_p_value(analysis_1) ## extracts the p-value for the treatment effect from the lm object (note this runs run_analysis() and then extracts p-value) -- this can be used for power

#' Return the result of a test for treatment effect(s) from an experimental analysis
#'
#' Description
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_y
#' @param design design object created by declare_design
#' @return a numeric scalar or vector of p-values
#' @examples
#' # Some examples will go here
#' @export
test_result.analysis <- function(Y, Z, analysis, data, alpha = .05){
  
  ## first runs the analysis then extracts the test result based on the analysis
  
  return(analysis$test_result(results = analysis$analysis(Y, Z, data), alpha = alpha))
  
}

#' Runs a pre-defined experimental analysis
#'
#' Description
#' @param Y name of the outcome for the analysis, a character object
#' @param Z name of the treatment indicator(s) for the analysis, a character object or vector of character objects
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_y
#' @return a numeric scalar or vector of p-values
#' @examples
#' Some examples will go here
#' @export
run_analysis.analysis <- function(formula, analysis, design, data){
  
  ## check that the treatment indicator is the first variable, which is required by the test_result function
  Z <- treatment_indicator_name(design)
  
  formula.rhs <- attr(terms.formula(formula), "term.labels")
  
  if(formula.rhs[1] != Z)
    stop(paste("The treatment indicator (", Z, ") should appear as the first variable in the formula.", sep = ""))
      
  return(analysis$analysis(formula, data))
  
}





