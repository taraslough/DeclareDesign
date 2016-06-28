# Declare Diagnosand


#' Diagnosand
#'
#' @param diagnostic_statistic_text 
#' @param diagnostic_statistic_function 
#' @param summary_function 
#' @param label 
#' @param description 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
declare_diagnosand <- function(diagnostic_statistic_text,
                               diagnostic_statistic_function,
                               summary_function = mean,
                               label = NULL, description = NULL,
                               ...){
  
  diagnostic_statistic_options <- list(...)
  
  if(!is.null(diagnostic_statistic_text)){
    
    if(class(diagnostic_statistic_text) != "character"){
      stop("Please provide a character string to the diagnostic_statistic_text argument.")
    }
    
    ## if no custom diagnostic_statistic is provided
    
    if(is.null(label)){
      label <- paste0(substitute(summary_function), "(", as.character(diagnostic_statistic_text), ")")
    }
    
    if(!is.character(eval(diagnostic_statistic_text))){
      diagnostic_statistic_text <- quote(diagnostic_statistic_text)
    } else {
      diagnostic_statistic_text <- parse(text = diagnostic_statistic_text)
    }
    
    diagnostic_statistic_function_internal <- function(simulations){
      return(eval(diagnostic_statistic_text, envir = simulations))
    }
    
  } else {
    
    if(class(diagnostic_statistic_function) != "function"){
      stop("Please provide a function to the diagnostic_statistic_function argument.")
    }
    
    ## if a custom diagnostic_statistic is provided
    
    diagnostic_statistic_function_internal <- function(simulations){
      argument_names <- names(formals(diagnostic_statistic_function))
      options_internal <- list()
      if(length(diagnostic_statistic_options) > 0){
        for(i in 1:length(diagnostic_statistic_options)){
          if(names(diagnostic_statistic_options)[[i]] %in% argument_names){
            options_internal[[names(diagnostic_statistic_options)[[i]]]] <- diagnostic_statistic_options[[i]]
          }
        }
      }
      options_internal$simulations <- simulations
      
      return(do.call(diagnostic_statistic_function, args = options_internal))
    }
    
  }
  
  return(structure(list(diagnostic_statistic = diagnostic_statistic_function_internal, 
                        summary = summary_function,
                        label = label, description = description, 
                        call = match.call()), class = "diagnosand"))
  
}

#' Title
#'
#' @param diagnosand 
#' @param simulations 
#'
#' @return
#' @export
#'
#' @examples
get_diagnosand <- function(diagnosand, simulations){
  
  diagnosand <- clean_inputs(diagnosand, object_class = "diagnosand", accepts_list = TRUE)
  
  diagnosands_list <- list()
  for(i in 1:length(diagnosand)){
    simulations[, diagnosand[[i]]$label] <- diagnosand[[i]]$diagnostic_statistic(simulations = simulations)
    diagnosands_list[[i]] <- aggregate(as.formula(paste0("cbind(diagnosand = `", diagnosand[[i]]$label, "`)", 
                                                         " ~ estimand_label + estimand_level + estimator_label + estimate_label")), 
                                       data = simulations, FUN = diagnosand[[i]]$summary)
    diagnosands_list[[i]]$diagnosand_label <- diagnosand[[i]]$label
  }
  
  diagnosands <- diagnosands_list[[1]]
  if(length(diagnosands_list) > 1){
    for(j in 2:length(diagnosands_list)){
      diagnosands <- merge(diagnosands, diagnosands_list[[j]], by = c("estimand_label", "estimand_level", "estimator_label", "estimate_label"))
    }
  }
  
  return(diagnosands)
  
}
