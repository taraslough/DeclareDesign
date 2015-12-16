#' Declare interference 
#' 
#' @param exposure_function Function to reveal exposure to treatment
#' @param formula Formula used in exposure function
#' @param exposure_variable_name Name of variable indicating realized exposure to treatment
#' @param condition_names Condition name vector
#' @param sep Separator for naming exposure potential outcomes
#' @param description Describe the interference pattern declared
#' @param ... A set of additional options for the \code{exposure_function}.
#' 
#' @export
declare_interference <- function(exposure_function = default_exposure_function,
                                 formula = NULL, exposure_variable_name = NULL, 
                                 condition_names, sep = "_", 
                                 description = NULL, ...){
  
  if(missing(condition_names)){
    stop("Please provide condition_names.")
  }
  
  if(is.null(formula) & is.null(exposure_variable_name)){
    stop("If you do not provide a formula, please provide the name of the exposure variable as a character string to exposure_variable_name.")
  }

  if(is.null(exposure_variable_name) & !is.null(formula)){
    exposure_variable_name <- as.character(formula[[2]])
  }
  
  outcomes_object <- 
    declare_potential_outcomes(potential_outcomes_function = exposure_function, 
                               formula = formula, 
                               outcome_variable_name = exposure_variable_name, 
                               condition_names = condition_names, 
                               sep = sep, 
                               description = description,
                               ... = ...)
  class(outcomes_object) <- "interference"
  return(outcomes_object)
}


