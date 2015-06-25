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
                             formula_estimand = NULL, weights_variable = NULL, qoi_function, ...) {
  
  ## should weights be able to be different for estimate and estimand functions?
  
  formula_has_intercept <- attr(terms.formula(formula), "intercept")
  formula_rhs <- attr(terms.formula(formula), "term.labels")
  if(formula_has_intercept == 1)
    formula_rhs <- c("(Intercept)", formula_rhs)
  
  if(is.null(formula_estimand))
    formula_estimand <- formula
    
  ## default estimate function is lm
  estimate <- function(data) {
    if(!is.null(weights_variable)){
      wts <- data[, weights_variable]
      lm(formula = formula, data = data, weights = wts)
    } else {
      lm(formula = formula, data = data)
    }
  }
  
  ## default estimand function is exactly the same as estimate
  estimand <- function(data) {
    if(!is.null(weights_variable)){
      wts <- data[, weights_variable]
      lm(formula = formula_estimand, data = data, weights = wts)
    } else {
      lm(formula = formula_estimand, data = data)
    }
  }
  
  ## default qoi is based on the treatment indicator coefficient 
  qoi <- function(x){
    treat_coef_num <- which(formula_rhs == treatment_variable)
    df <- df.residual(x)
    est <- coef(x)[treat_coef_num]
    se <- sqrt(diag(vcov(x)))[treat_coef_num]
    p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
    conf_int <- confint(x)[treat_coef_num, ]
    return(list(est = est, se = se, p = p, ci.lower = conf_int[1], ci.upper = conf_int[2], df = df))
  }
  
  return(list(estimate = estimate, estimand = estimand, qoi = qoi, call = match.call()))
  
}

get_estimate <- function(analysis, data){
  return(analysis$estimate(data = data))
}

get_estimand <- function(analysis, data){
  
  treatment_conditions <- unique(data[, analysis_treatment_variable(analysis)])
  formula <- analysis$call$formula ##_estimand
  
  ## create replicated data frame with only the right variables
  data <- data[, all.vars(formula)]
  data.rep <- do.call("rbind", replicate(length(treatment_conditions), data, simplify = FALSE))
  
  ## replace treatment with the replicated treatment
  data.rep[, analysis_treatment_variable(analysis)] <- rep(treatment_conditions, each = nrow(data))
  
  data.rep[, analysis_outcome_variable(analysis)] <- observed_outcome(outcome = analysis_outcome_variable(analysis), 
                                                                      treatment_assignment = analysis_treatment_variable(analysis),
                                                                      data = data.rep, design = design)
  
  return(analysis$estimand(data = data.rep))
  
}

#' @export
get_qoi <- function(analysis, data, output = "estimate"){
  ## in principle we could integrate multiple analyses here
  if(output == "estimate")
    output <- estimate
  else if (output == "estimand")
    output <- estimand
  return(analysis$qoi(output(analysis = analysis, data = data)))
}

#' @export
analysis_outcome_variable <- function(analysis) {
  return(analysis$outcome_variable)
}

#' @export
analysis_treatment_variable <- function(analysis) {
  return(analysis$treatment_variable)
}



