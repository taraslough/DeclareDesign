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


declare_analysis <- function(formula, treatment_variable = "Z", method = "lm", subset = "X == 1",
                             formula_estimand = NULL, method_estimand = "lm", outcome_variable = NULL, 
                             weights_variable = NULL, qoi = "ATE", qoi_only = TRUE, ...) {
  
  ## should weights be able to be different for estimate and estimand functions?
  
  formula_has_intercept <- attr(terms.formula(formula), "intercept")
  formula_rhs <- attr(terms.formula(formula), "term.labels")
  if(formula_has_intercept == 1)
    formula_rhs <- c("(Intercept)", formula_rhs)
  
  if(is.null(formula_estimand))
    formula_estimand <- formula
  
  if(is.null(treatment_variable))
    stop("The treatment variable must be declared in the treatment_variable argument.")
  
  if(qoi_only == FALSE){
    
    if(class(estimand) == "character") {
      
      if(method == "lm") {
        ## default estimate function is lm
        estimate <- function(data) {
          ## change this so it can take any R model function and send the ... options to it
          if(!is.null(subset))
            data <- subset(data, eval(parse(text = subset)))
          if(!is.null(weights_variable)){
            wts <- data[, weights_variable]
            lm(formula = formula, data = data, weights = wts)
          } else {
            lm(formula = formula, data = data)
          }
        }
      }
      
    }
    
    if(class(estimand) == "character"){ 
      
      if(estimand == "ATE" | estimand == "CATE"){
        ## default estimand function is exactly the same as estimate
        estimand <- function(data) {
          if(estimand == "CATE"){
            if(!is.null(subset)){
              data <- subset(data, eval(parse(text = subset)))
            } else {
              stop("The chosen estimand CATE needs to know which subset to estimate the CATE on.")
            }
          }
          if(!is.null(weights_variable)){
            wts <- data[, weights_variable]
            lm(formula = formula_estimand, data = data, weights = wts)
          } else {
            lm(formula = formula_estimand, data = data)
          }
        } 
      }
      
    }
    
    if(class(qoi) == "character"){
      
      if(qoi == "ATE"){
        ## default qoi is based on the treatment indicator coefficient 
        qoi <- function(x){
          ##treat_coef_num <- which(formula_rhs == treatment_variable)
          treat_coef_num <- 2
          df <- df.residual(x)
          est <- coef(x)[treat_coef_num]
          se <- sqrt(diag(vcov(x)))[treat_coef_num]
          p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
          conf_int <- confint(x)[treat_coef_num, ]
          return(list(est = est, se = se, p = p, ci.lower = conf_int[1], ci.upper = conf_int[2], df = df))
        }
      }
      
      return_object <- list(estimate = estimate, estimand = estimand, qoi = qoi, formula_estimand = formula_estimand, call = match.call())
      
    }
    
  } else {
    
    ## case where it is qoi only -- they define one function that does estimates and qoi's
    
    if(is.null(outcome_variable))
      stop("If you declare a custom quantity of interest (qoi) function, you must set declare both the treatment_variable and outcome_variable arguments.")
    
    return_object <- list(qoi = qoi, call = match.call())
    
  }
  
  class(return_object) <- "analysis"
  
  return(return_object)
  
}

truth_data_frame <- function(formula = NULL, outcome_variable = "Y", treatment_variable = "Z", subset = NULL, data = data) {
  ##formula <- analysis$call$formula ##_estimand
  
  if(is.null(formula))
    formula <- paste(outcome_variable, "~", treatment_variable)
  
  warning("question for us: should subset happen before or after obtaining list of treatment conditions")
  if(!is.null(subset))
    data <- subset(data, eval(parse(text = subset)))
  
  treatment_conditions <- unique(data[, treatment_variable])
  
  ## create replicated data frame with only the right variables
  data <- data[, all.vars(formula)]
  data_rep <- do.call("rbind", replicate(length(treatment_conditions), data, simplify = FALSE))
  
  ## replace treatment with the replicated treatment
  data_rep[, treatment_variable] <- rep(treatment_conditions, each = nrow(data))
  
  data_rep[, outcome_variable] <- observed_outcome(outcome = outcome_variable, 
                                                   treatment_assignment = treatment_variable,
                                                   data = data_rep, design = design)
  return(data_rep)
  
}

get_estimates_model <- function(analysis, data){
  return(analysis$estimate(data = data))
}

get_estimands_model <- function(analysis, data){
  return(analysis$estimand(data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
}

get_estimates <- function(analysis, qoi = NULL, data) {
  if(analysis$call$qoi_only == FALSE){
    return(qoi(get_estimates_model(analysis = analysis, data = data)))
  } else {
    return(analysis$qoi(data = data))
  }
}

get_estimands <- function(analysis, qoi = NULL, data) {
  
  ## analysis can be an analysis object or a list of them
  
  if(class(analysis) == "list") {
    if(is.null(qoi))
      stop("To get a quantity of interest from multiple analyses, please specify a function to do so in the qoi argument.")
    return(qoi(analysis, data = data))
  } else {
    ## when analysis is a single object, take qoi and other objects from the analysis object
    if(analysis$call$qoi_only == FALSE){
      return(qoi(get_estimands_model(analysis = analysis, data = truth_data_frame(formula = analysis$formula_estimand, data = data))))
    } else {
      return(analysis$qoi(data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
    }
  }
  
}

#' @export
analysis_outcome_variable <- function(analysis) {
  return(analysis$outcome_variable)
}

#' @export
analysis_treatment_variable <- function(analysis) {
  return(analysis$treatment_variable)
}





