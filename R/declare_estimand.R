#' Declare Estimand
#' 
#' This function creates an estimand object. 
#'
#' @param estimand_function A function, possibly of data, that returns a (possibly single-valued) vector of quantities to be estimated.
#' @param estimand_text A character string that contains an expression that can be evaluated on the data.  For example, you can provide "mean(Y_Z1 - Y_Z0)" to set the estimand as the average difference between the Y_Z1 potential outcome and the Y_Z0 potential outcome.
#' @param estimand_options A list of named options to be used by the estimand function.
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param condition_names A string vector indicating which conditions to create potential outcomes for.
#' @param subset A character string that contains an expression that can be passed to the subset operator.  For example subset = "income > 50".
#' @param weights_variable_name The name of the weighting variable (optional).
#' @param label A character string for the estimand's label.
#' 
#' @return An estimand object.
#'
#' @export
declare_estimand <- function(estimand_function = NULL, estimand_text = NULL,
                             ..., fixed = FALSE,
                             potential_outcomes, noncompliance = NULL, attrition = NULL,
                             condition_names = NULL,
                             subset = NULL, weights_variable_name = NULL, 
                             label = NULL, description = NULL) {
  
  estimand_options <- list(...)
  
  # Checks -------------------------------------------------
  potential_outcomes <- clean_inputs(potential_outcomes, "potential_outcomes", accepts_list = TRUE)
  noncompliance <- clean_inputs(potential_outcomes, "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(potential_outcomes, "attrition", accepts_list = FALSE)
  
  if(!is.null(weights_variable_name)){
    stop("Weighted estimands are not yet implemented. Please contact the authors if you are interested in using them.")
  }
  
  if(!is.null(estimand_function) & !is.null(estimand_text)){
    stop("Please provide either estimand_function or estimand_text.")
  }
  
  if(is.null(potential_outcomes)){
    stop("Please provide a potential_outcomes object. This is used to create the potential outcomes before calculating the estimand.")
  }
  
  if(!is.null(condition_names)){
    condition_names <- clean_condition_names(condition_names)
  }
  
  if(!is.null(estimand_text)){
    
    if(class(estimand_text) != "character"){
      stop("Please provide a character string to the estimand_text argument.")
    }
    
    ## if no custom estimand is provided
    
    if(is.null(label)){
      label <- as.character(estimand_text)
    }
    
    if(!is.character(eval(estimand_text))){
      estimand_text <- quote(estimand_text)
    } else {
      estimand_text <- parse(text = estimand_text)
    }
    
    estimand_function_internal <- function(data){
      if(!is.null(subset))
        data <- subset(data, subset = eval(parse(text = subset)))
      ##if(!is.null(weights_variable_name))
      ##  estimator_options$weights <- data[, weights_variable_name]
      return(eval(estimand_text, envir = data))
    }
  } else {
    
    if(class(estimand_function) != "function"){
      stop("Please provide a function to the estimand_function argument.")
    }
    
    ## if a custom estimand is provided
    
    estimand_function_internal <- function(data){
      argument_names <- names(formals(estimand_function))
      options_internal <- list()
      if(!is.null(subset) & "subset" %in% argument_names)
        options_internal$subset <- with(data, eval(parse(text = subset)))
      if(!is.null(weights_variable_name) & "weights" %in% argument_names)
        options_internal$weights <- data[, weights_variable_name]
      if(length(estimand_options) > 0){
        for(i in 1:length(estimand_options)){
          if(names(estimand_options)[[i]] %in% argument_names){
            options_internal[[names(estimand_options)[[i]]]] <- estimand_options[[i]]
          }
        }
      }
      options_internal$data <- data
      
      return(do.call(estimand_function, args = options_internal))
    }
    
  }
  
  structure(list(estimand = estimand_function_internal, potential_outcomes = potential_outcomes, noncompliance = noncompliance, attrition = attrition,
                 fixed = fixed, condition_names = condition_names, label = label, description = description, call = match.call()), class = "estimand")
  
}

#' Obtain Estimands
#' 
#' @param estimand An estimand object or list of estimand objects, created with \code{\link{declare_estimand}}.
#' @param estimator An estimantor object or a list of estimator objects, created with \code{\link{declare_estimator}}.
#' @param data A data.frame, created by \code{\link{draw_sample}} or \code{\link{draw_population}} depending on the target of the estimate (sample or population).
#'
#' @export
get_estimands <- function(estimand = NULL, estimator = NULL, data){
  
  if(!is.null(estimand) & !is.null(estimator)){
    stop("Please either send estimand(s) or estimator(s) only to get_estimands().")
  }
  
  if(!is.null(estimator) & class(estimator) == "list"){
    estimand <- lapply(1:length(estimator), function(j) estimator[[j]]$estimand)
  } else if(!is.null(estimator) & class(estimator) == "estimator"){
    estimand <- estimator$estimand
  }
  
  if(class(estimand) == "list"){
    estimand_labels <- c(lapply(1:length(estimand), function(j) ifelse(is.null(estimand[[j]]$label), "", estimand[[j]]$label)), recursive = TRUE)
    estimand_labels[which(estimand_labels == "")] <- paste(substitute(estimand)[-1L])[which(estimand_labels == "")]
  } else {
    estimand_labels <- estimand$label
    if(is.null(estimand_labels)){
      estimand_labels <- paste(substitute(estimand))
    }
  }
  
  if(class(estimand) == "estimand"){
    estimand <- list(estimand)
  }
  
  estimands_list <- list()
  if(!is.null(estimand)){
    for(i in 1:length(estimand)){
      if(!is.null(estimand[[i]])){
        ## if there is an estimand defined
        if(estimand[[i]]$fixed == FALSE){
          
          has_potential_outcomes <- has_potential_outcomes(data = data, potential_outcomes = estimand[[i]]$potential_outcomes, attrition = estimand[[i]]$attrition, noncompliance = estimand[[i]]$noncompliance,
                                                           condition_names = estimand[[i]]$condition_names)
          
          if(has_potential_outcomes == TRUE){
            ## if PO's already exist, do not create them
            estimands_list[[i]] <- estimand[[i]]$estimand(data = data)
          } else {
            ## otherwise, use draw_potential_outcomes to create them
            estimands_list[[i]] <- estimand[[i]]$estimand(data = draw_potential_outcomes(data = data, potential_outcomes = estimand[[i]]$potential_outcomes,  
                                                                                         attrition = estimand[[i]]$attrition, noncompliance = estimand[[i]]$noncompliance,
                                                                                         condition_names = estimand[[i]]$condition_names))
          }
        } else {
          estimands_list[[i]] <- estimand[[i]]$estimand(data = data)
        }
        names(estimands_list[[i]]) <- estimand_labels[i]
      } else {
        ## if there is NOT an estimand defined
        estimands_list[[i]] <- NA
      }
    }
    estimands_vector <- c(estimands_list, recursive = T)
    
  } else {
    ## if estimand is null (i.e. an estimator did not have an estimand)
    
    estimands_vector <- NA
  }
  
  return(estimands_vector)
  
}

