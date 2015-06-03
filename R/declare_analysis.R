#' Declare an experimental analysis
#'
#' Description
#' @param method either string "diff-in-means", "lm-with-covariates" or a function object that takes as arguments data, design and spits out in the simplest version a single treatment effect estimates (for a multi-arm study spits out something more complex)
#' @param get_p_value a function that extracts the test statistic from the analysis
#' @return a list containing a function to conduct the analysis and a function to extract the p value
#' @examples
#' # these examples don't work yet
#' # declare_analysis(method = "diff-in-means")
#' # declare_analysis(method = function(design, data) lm(paste(outcome_variable_name(data) ~ treatment_variable_name(design), data = data))
#' @export
declare_analysis <- function(method, get_p_value){
  
  if(!class(get_p_value)=="function")
    stop("Currently only functions can be used to extract p values.")
  
  if(class(method) == "character") {
    if(method == "diff-in-means") {
      method <- function(data, design){
        
        analysis <- function(design, data) lm(paste(outcome_var_name(design), "~", treat_var_name(design)), data = data)
        
        test_statistic <- function(analysis) sqrt(diag(vcov(analysis)))[1, 1]
        
        return.object <- list(analysis = analysis, get_p_value = get_p_value)
        
      }
    } 
  } else if(class(method) == "function"){
        
    return.object <- list(analysis = method, test_statistic = test_statistic)
    
  }
  
  class(return.object) <- "analysis"
  return()
    
}

#' Return the p value for treatment effect(s) from an experimental analysis
#'
#' Description
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_y
#' @param design design object created by declare_design
#' @return a numeric scalar or vector of p-values
#' @examples
#' # Some examples will go here
#' @export
get_p_value.analysis <- function(analysis, data, design){
  
  ## first runs the analysis then extracts the p-value from the analysis
  
  return(analysis$get_p_value(analysis$analysis(design, data)))
  
}

#' Runs a pre-defined experimental analysis
#'
#' Description
#' @param analysis analysis object created by declare_analysis
#' @param data data object created by make_y
#' @param design design object created by declare_design
#' @return a numeric scalar or vector of p-values
#' @examples
#' Some examples will go here
#' @export
run_analysis.analysis <- function(analysis, data, design){
  
  return(analysis$analysis(design, data))
  
}





