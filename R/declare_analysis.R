#' Declare an experimental analysis
#'
#' Description
#' @param method either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm
#' @param test_success a function that extracts the binary result of a statistical test (did it pass = 1, if not = 0)
#' @param formula an optional formula object to define analyses such as linear regressions with covariates
#' @param ... additional options to be sent to the analysis function and the test_success function
#' @return a list containing a function to conduct the analysis and a function to extract the result of the test
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @rdname declare_analysis
#' @export
declare_analysis <- function(formula, treatment_variable = "Z", method, design, Y, Z, 
                             test_success = "treatment-coefficient-significant", alpha = .05, ...){
  
  ##substitute(y ~ z + z*p, list(z = as.name = "q"))
  
  if(class(method) == "character") {    
    ## if user provides a string as a method, this invokes default analyses we define
    if(method == "lm")
      analysis <- function(data) lm(formula = formula, data = data)
    else if(method == "glm"){
      if(missing(family)){
        family <- "binomial('logit')"
        warning("Family was not specified for glm. Binomial logistic regression was chosen as the default.")
      }
      analysis <- function(data) glm(formula = formula, data = data, family = family)
    }
  }
  
  if(class(test_success) == "character" | !exists(test_success)){
    ## if user provides a string, this invokes default test_success functions
    if(!exists(test_success) & (method == "lm" | method == "glm"))
      test_success <- "treatment-coefficient-significant"
    
    if(test_success == "treatment-coefficient-significant"){
      treat_coef_num <- which(attr(terms.formula(formula), "term.labels") == treatment_variable)
      
      test_success <- function(results, k = treat_coef_num, alpha = alpha) summary(results)$coefficients[k,4] < alpha
      ## note this code definitely works for lm, glm
    }
  }
  
  return.object <- list(analysis = analysis, test_success = test_success, call = match.call())
  
  class(return.object) <- "analysis"
  
  return(return.object)
  
}

#' Return the result of a test for treatment effect(s) from an experimental analysis
#'
#' Description
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_y
#' @param design design object created by declare_design
#' @return a numeric scalar or vector of p-values
#' @examples
#' # Some examples will go here
#' @rdname declare_analysis
#' @export
test_success <- function(analysis, finished_analysis = NULL, data, alpha = .05){
    
  if(class(analysis) != "analysis") 
    stop("Can only run analyses created by declare_analysis.")
  
  ## first runs the analysis then extracts the test result based on the analysis
  
  if(is.null(finished_analysis))
    finished_analysis <- run_analysis(analysis = analysis, data = data)
  
  ##save(analysis, finished_analysis, data, file = "~/downloads/tmp44.RData")
  
  return(analysis$test_success(results = finished_analysis, alpha = alpha))
  
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
#' ##Some examples will go here
#' @rdname declare_analysis
#' @export
run_analysis <- function(analysis, data){
  
  if(class(analysis) != "analysis") 
    stop("Can only run analyses created by declare_analysis.")
  
  return(analysis$analysis(data))
  
}





