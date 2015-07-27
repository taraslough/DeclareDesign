#' Declare an experimental analysis
#'
#' Description
#' @param formula A standard R formula as a string or formula object, i.e. Y ~ Z, that indicates the outcome and the treatment indicator as well as covariates, block indicators, etc. that are used to construct estimates and/or estimands. By default, the formula provided is used for estimates and estimands.
#' @param treatment_variable The variable name of the treatment indicator as a string. Defaults to "Z".
#' @param outcome_variable The variable name of the outcome variable as a string. Defaults to "Y".
#' @param method The method used in the analysis to construct estimates and, by default, estimands. Indicate either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm. Defaults to "lm".
#' @param subset A string indicating the subset of the data to take in estimates and, by default, estimands.
#' @param weights A string indicating the name of the weights variable to be used in weighted estimators such as WLS.
#' @param estimand defaults to "ATE".
#' @param formula_estimand
#' @param method_estimand either string (i.e. "lm" or "glm") or a function object that takes as arguments data, design and spits out a standard R class such as lm or glm
#' @param subset_estimand A string indicating the subset of the data to take for estimands, if the user wishes to have a different subset used in calculating estimands than the subset used for the estimates.
#' @param weights_estimand A string indicating the name of the weights variable to be used in weighted estimators such as WLS for calculating estimands only, if the user desires a different weights variable than used to calculate estimates.
#' @param qoi defaults to "ATE".
#' @param qoi_only
#' @param qoi_labels
#' @return a list containing a function to conduct the analysis and a function to extract the result of the test
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @rdname declare_analysis
#' @export
declare_analysis <- function(formula, treatment_variable = "Z", outcome_variable = NULL, 
                             method = "lm", subset = NULL, weights = NULL, 
                             estimand = "ATE", formula_estimand = NULL, method_estimand = NULL,
                             subset_estimand = NULL, weights_estimand = NULL,
                             qoi = "ATE", qoi_only = FALSE, qoi_labels = NULL) {
  
  ## should weights be able to be different for estimate and estimand functions?
  
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
            lm(formula = stats::formula(unclass(formula)), data = data, weights = data[, weights])
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
        
        ## determines which coef is the treatment variable, and adds 1 if there is an intercept
        
        treat_coef_num <- which(attr(terms.formula(formula), "term.labels") == treatment_variable) + 
          as.numeric(attr(terms.formula(formula), "intercept") == 1)
        
        qoi <- function(x, stats = c("est", "se", "p", "ci_lower", "ci_upper", "df")){
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

  if(is.null(formula))
    stop("Formula must be provided.")
  
  outcome_variable <- all.vars(formula[[2]])
  
  covariate_variable_names <- all.vars(formula[[3]])[!(all.vars(formula[[3]]) %in% treatment_variable)]
  
  treatment_conditions <- unique(data[, treatment_variable])
  
  potential_outcome_variable_names <- paste(outcome_variable, sep, treatment_conditions, sep = "")
  
  ## create replicated data frame with only the right variables
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

#' @rdname declare_analysis
#' @export
get_estimates_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  return(analysis$estimate(data = data))
}

#' @rdname declare_analysis
#' @export
get_estimands_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  return(analysis$estimand(data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
}

#' @rdname declare_analysis
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
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimates_matrix <- estimates_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimates_matrix <- merge(estimates_matrix, estimates_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimates_matrix) <- estimates_matrix[,1]
          estimates_matrix <- estimates_matrix[, 2:ncol(estimates_matrix), drop = F]
        }
      }
      return(estimates_matrix)
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

#' @rdname declare_analysis
#' @export
get_estimands <- function(analysis, qoi = NULL, data, stats = "est"){
  
  analysis_labels <- paste0("analysis", sprintf(paste0("%0",nchar(as.character(length(analysis))),"d"),(1:length(analysis))))
  
  if(!is.null(qoi)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(qoi(analysis, data = truth_data_frame(formula = analysis$formula_estimand, data = data)))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
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
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimands_matrix <- estimands_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimands_matrix <- merge(estimands_matrix, estimands_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimands_matrix) <- estimands_matrix[,1]
          estimands_matrix <- estimands_matrix[, 2:ncol(estimands_matrix), drop = F]
        }
      }
      return(estimands_matrix)
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




