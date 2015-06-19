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
declare_analysis <- function(formula, treatment_variable = "Z", method = "lm", 
                             test_success = "treatment-coefficient-significant", alpha = .05, ...){
  
  ## NOTES FOR GRAEME -- THIS WILL GO IN ALL ANALYSIS FUNCTIONS
  ##if(!missing(Y) & Y != outcome_variable)
  ##  formula2 <- substitute(formula, list(outcome_variable = as.name(Y)))
  ##if(!missing(Z) & Z != treatment_variable)
  ##  formula <- substitute(formula, list(treatment_variable = as.name(Z)))
  formula_has_intercept <- attr(terms.formula(formula), "intercept")
  formula_rhs <- attr(terms.formula(formula), "term.labels")
  if(formula_has_intercept == 1)
    formula_rhs <- c("(Intercept)", formula_rhs)
  
  if(!any(formula_rhs== treatment_variable))
    stop(paste("The treatment variable set in treatment_variable,", treatment_variable, ", 
               does not appear in the formula.", sep = ""))
  
  outcome_variable <- all.vars(formula[[2]])
  
  if(class(method) == "character") {    
    ## if user provides a string as a method, this invokes default analyses we define
    
    if(method == "lm") {
      
      analysis <- function(data #, Y = NULL, Z = NULL
                           ) {
        lm(formula = formula, data = data)
      }
      
    } else if(method == "glm"){
      
      if(!exists(family)){
        family <- "binomial('logit')"
        warning("Family was not specified for glm. Binomial logistic regression was chosen as the default.")
      }
      analysis <- function(data #,Y = NULL, Z = NULL
                           ) {
        ##if(!missing(Y) & Y != outcome_variable)
        ##  formula2 <- substitute(formula, list(outcome_variable = as.name(Y)))
        ##if(!missing(Z) & Z != treatment_variable)
        ##  formula <- substitute(formula, list(treatment_variable = as.name(Z)))
        glm(formula = formula, data = data, family = family)
      }
      
    }
    
  }
  
  if(class(test_success) == "character" | !exists(test_success)){
    ## if user provides a string, this invokes default test_success functions
    if(!exists(test_success) & (method == "lm" | method == "glm"))
      test_success <- "treatment-coefficient-significant"
    
    if(test_success == "treatment-coefficient-significant"){      
      treat_coef_num <- which(formula_rhs == treatment_variable)
      
      test_success <- function(results, k = treat_coef_num #Y = NULL, Z = NULL, 
                               ) {
        summary(results)$coefficients[k,4] < alpha
      }
      ## note this code definitely works for lm, glm
    }
  }
  
  return.object <- list(analysis = analysis, test_success = test_success, 
                        treatment_variable = treatment_variable, outcome_variable = outcome_variable,
                        method = method, alpha = alpha,
                        call = match.call())
  
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
test_success <- function(analysis, finished_analysis = NULL, data, Y = NULL, Z = NULL){
    
  if(class(analysis) != "analysis") 
    stop("Can only run analyses created by declare_analysis.")
  
  ## first runs the analysis then extracts the test result based on the analysis
  
  if(is.null(finished_analysis))
    finished_analysis <- run_analysis(analysis = analysis, data = data) ## Y = Y, Z = Z)
  
  ##if(is.null(Z))
  ##  Z <- analysis_treatment_variable(analysis)
    
  return(analysis$test_success(results = finished_analysis))  ##, Z = Z))
  
}

#' Runs a pre-defined experimental analysis
#'
#' Description
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_data_frame
#' @return a numeric scalar or vector of p-values
#' @examples
#' ##Some examples will go here
#' @rdname declare_analysis
#' @export
run_analysis <- function(analysis, data, Y = NULL, Z = NULL){
  
  if(class(analysis) != "analysis") 
    stop("Can only run analyses created by declare_analysis.")
  
  return(analysis$analysis(data = data)) #, Y = Y, Z = Z))
  
}

#' @export
analysis_outcome_variable <- function(analysis) {
  return(analysis$outcome_variable)
}

#' @export
analysis_treatment_variable <- function(analysis) {
  return(analysis$treatment_variable)
}



