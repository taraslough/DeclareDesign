

#' @export
default_potential_outcomes_function <- function(formula, data){
  
  return(eval(expr = formula[[3]], envir = data))
  
}

#' @export
declare_potential_outcomes <- function(potential_outcomes_function = default_potential_outcomes_function,
                                       formula = NULL, outcome_name = NULL, condition_names = NULL, sep = "_", 
                                       treatment_variable_name = NULL,
                                       potential_outcomes_type = "outcome",
                                       ...){
  
  function_options <- list(...)
  
  arguments <- mget(names(formals()), sys.frame(sys.nframe()))
  arguments$... <- NULL
  if(length(function_options) > 0) {
    for(k in 1:length(function_options))
      arguments[[names(function_options)[[k]]]] <- function_options[[k]]
  }
  
  if(class(potential_outcomes_function) != "function"){
    stop("Please provide a function in the potential_outcomes_function argument.")
  }
  
  potential_outcomes_function_internal <- function(data = NULL){
    argument_names <- names(formals(potential_outcomes_function))
    if(!is.null(formula) & "formula" %in% argument_names)
      function_options$formula <- formula
    if(!is.null(data) & "data" %in% argument_names)
      function_options$data <- data
    if(!is.null(condition_names) & "condition_names" %in% argument_names)
      function_options$condition_names <- condition_names
    if(!is.null(treatment_variable_name) & "treatment_variable_name" %in% argument_names)
      function_options$treatment_variable_name <- treatment_variable_name
    
    return(do.call(potential_outcomes_function, args = function_options))
  }
  
  if(is.null(outcome_name) & !is.null(formula)){
    outcome_name <- as.character(formula[[2]])
  }
  
  return_object <- list(potential_outcomes_function = potential_outcomes_function_internal, 
                        outcome_name = outcome_name, sep = "_", condition_names = condition_names,
                        treatment_variable_name = treatment_variable_name,
                        arguments = arguments, 
                        potential_outcomes_type = potential_outcomes_type,
                        call = match.call())
  
  structure(return_object, class = "potential_outcomes")
  
}



