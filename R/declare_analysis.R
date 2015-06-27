#' Declare an experimental analysis
#'
#' Description
#' @param method either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm
#' @param treatment_variable
#' @param method
#' @param subset a string indicating which subset of the data to take for analyses
#' @param weights
#' @param estimand
#' @param formula_estimand
#' @param outcome_variable
#' @param qoi
#' @param qoi_only
#' @param formula an optional formula object to define analyses such as linear regressions with covariates
#' @param ... additional options to be sent to the analysis function and the test_success function
#' @return a list containing a function to conduct the analysis and a function to extract the result of the test
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @rdname declare_analysis
#' @export
declare_analysis <- function(formula, treatment_variable = "Z", method = "lm", subset = NULL, weights = NULL, 
                             estimand = "ATE", formula_estimand = NULL, outcome_variable = NULL, 
                             qoi = "ATE", qoi_only = FALSE, qoi_labels = NULL, ...) {
  
  ## should weights be able to be different for estimate and estimand functions?
  
  formula_has_intercept <- attr(terms.formula(formula), "intercept")
  formula_rhs <- attr(terms.formula(formula), "term.labels")
  if(formula_has_intercept == 1)
    formula_rhs <- c("(Intercept)", formula_rhs)
  
  if(is.null(qoi_labels) & class(qoi) == "character")
    qoi_labels <- qoi
  
  outcome_variable <- all.vars(formula[[2]])
  
  if(is.null(formula_estimand))
    formula_estimand <- formula
  
  if(is.null(treatment_variable))
    stop("The treatment variable must be declared in the treatment_variable argument.")
  
  if(qoi_only == FALSE){
    
    if(class(method) == "character") {
      
      if(method == "lm") {
        ## default estimate function is lm
        estimate <- function(data) {
          ## change this so it can take any R model function and send the ... options to it
          if(!is.null(subset))
            data <- subset(data, eval(parse(text = subset)))
          if(!is.null(weights)){
            lm(formula = formula, data = data, weights = data[, weights])
          } else {
            lm(formula = formula, data = data)
          }
        }
      }
      
    }
    
    if(class(estimand) == "character"){ 
      
      if(estimand == "ATE"){
        ## default estimand function is exactly the same as estimate
        estimand <- function(data) {
          if(!is.null(weights)){
            lm(formula = formula_estimand, data = data, weights = data[, weights])
          } else {
            lm(formula = formula_estimand, data = data)
          }
        }
      }
      
    }
    
    if(class(qoi) == "character"){
      
      if(qoi == "ATE"){
        ## default qoi is based on the treatment indicator coefficient 
        qoi <- function(x, stats = c("est", "se", "p", "ci_lower", "ci_upper", "df")){
          treat_coef_num <- which(formula_rhs == treatment_variable)
          coef_name <- names(coef(x))[treat_coef_num]
          df <- df.residual(x)
          est <- coef(x)[treat_coef_num]
          se <- sqrt(diag(vcov(x)))[treat_coef_num]
          p <- 2 * pt(abs(est/se), df = df, lower.tail = FALSE)
          conf_int <- confint(x)[treat_coef_num, ]
          
          output <- matrix(c(est, se, p, conf_int, df), 
                           dimnames = list(c("est", "se", "p", "ci_lower", "ci_upper", "df"), 
                                           paste(outcome_variable, "~", coef_name, "_", qoi_labels, sep = "")))
          return(output[which(rownames(output) %in% stats), , drop = FALSE])
        }
      }
      
    }
    
    return_object <- list(estimate = estimate, estimand = estimand, qoi = qoi, 
                          formula_estimand = formula_estimand, treatment_variable = treatment_variable,
                          outcome_variable = outcome_variable,
                          method = method, qoi_only = qoi_only, call = match.call())
    
  } else {
    
    ## case where it is qoi only -- they define one function that does estimates and qoi's
    
    if(is.null(outcome_variable))
      stop("If you declare a custom quantity of interest (qoi) function, you must set declare both the treatment_variable and outcome_variable arguments.")
    
    return_object <- list(qoi = qoi, qoi_only = TRUE, call = match.call())
    ## this must take as arguments an analysis object created by declare_analysis and a data frame
    ## this must return a matrix of rows # of statistics and columns # of qoi's
    
  }
  
  class(return_object) <- "analysis"
  
  return(return_object)
  
}

#' @export
truth_data_frame <- function(formula = NULL, treatment_variable = "Z", 
                             subset = NULL, data = data, sep = "_") {
  ##formula <- analysis$call$formula ##_estimand
  
  if(is.null(formula))
    stop("Formula must be provided.")
  
  outcome_variable <- all.vars(formula[[2]])
  
  covariate_variable_names <- all.vars(formula[[3]])[!(all.vars(formula[[3]]) %in% treatment_variable)]
  
  treatment_conditions <- unique(data[, treatment_variable])
  
  potential_outcome_variable_names <- paste(outcome_variable, sep, treatment_conditions, sep = "")
  
  ## create replicated data frame with only the right variables
  ##data <- data[, all.vars(formula)]
  data <- data[, c(potential_outcome_variable_names, covariate_variable_names)]
  data_rep <- do.call("rbind", replicate(length(treatment_conditions), data, simplify = FALSE))
  
  ## replace treatment with the replicated treatment
  data_rep[, treatment_variable] <- rep(treatment_conditions, each = nrow(data))
  
  data_rep[, outcome_variable] <- observed_outcome(outcome = outcome_variable, 
                                                   treatment_assignment = treatment_variable,
                                                   data = data_rep, sep = sep)
  
  if(!is.null(subset))
    data <- subset(data, eval(parse(text = subset)))
  
  return(data_rep)
  
}

#' @export
get_estimates_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  return(analysis$estimate(data = data))
}

#' @export
get_estimands_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  return(analysis$estimand(data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
}

#' @export
get_estimates <- function(analysis, qoi = NULL, data) {
  
  analysis_labels <- paste0("analysis", sprintf(paste0("%0",nchar(as.character(length(analysis))),"d"),(1:length(analysis))))
  
  if(!is.null(qoi)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(qoi(analysis, data = data))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimates_list <- list()
      for(i in 1:length(analysis)) {
        if(analysis[[i]]$qoi_only == FALSE){
          estimates_list[[i]] <- analysis[[i]]$qoi(get_estimates_model(analysis = analysis[[i]], data = data))
        } else {
          estimates_list[[i]] <- analysis[[i]]$qoi(data = data)
        }
        colnames(estimates_list[[i]]) <- paste(colnames(estimates_list[[i]]), analysis_labels[i], sep = "_")
      }
      return(do.call("cbind", estimates_list))
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(analysis$qoi_only == FALSE){
        estimates_matrix <- analysis$qoi(get_estimates_model(analysis = analysis, data = data))
      } else {
        estimates_matrix <- analysis$qoi(data = data)
      }
      colnames(estimates_matrix) <- paste(colnames(estimates_matrix), analysis_labels[1], sep = "_")
      return(estimates_matrix)
    }
  }
}

#' @export
get_estimands <- function(analysis, qoi = NULL, data, stats = "est"){
  
  analysis_labels <- paste0("analysis", sprintf(paste0("%0",nchar(as.character(length(analysis))),"d"),(1:length(analysis))))
  
  if(!is.null(qoi)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(qoi(analysis, data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      warning("Need to fix this function so that it neatly handles different sets of parameters -- use merge")
      
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimands_list <- list()
      for(i in 1:length(analysis)){
        if(analysis[[i]]$qoi_only == FALSE){
          estimands_list[[i]] <- analysis[[i]]$qoi(get_estimands_model(analysis = analysis[[i]], data = data), stats = stats)
          ## get_estimands_model does truth_data_frame, so just sending it data
        } else {
          estimands_list[[i]] <- analysis[[i]]$qoi(truth_data_frame(formula = analysis[[i]]$formula_estimand, data = data))
        }
        colnames(estimands_list[[i]]) <- paste(colnames(estimands_list[[i]]), analysis_labels[i], sep = "_")
      }
      ## when it is sent back to get_estimands() it will run truth_data_frame, so just sending it data
      return(do.call("cbind", estimands_list))
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(analysis$qoi_only == FALSE){
        estimands_matrix <- analysis$qoi(get_estimands_model(analysis = analysis, data = data), stats = stats)
        ## get_estimands_model does truth_data_frame, so just sending it data
      } else {
        estimands_matrix <- analysis$qoi(truth_data_frame(formula = analysis$formula_estimand, data = data))
      }
      colnames(estimands_matrix) <- paste(colnames(estimands_matrix), analysis_labels[1], sep = "_")
      return(estimands_matrix)
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





