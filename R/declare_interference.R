#' @export
declare_interference <- function(exposure_function = default_exposure_function,
                                 formula = NULL, 
                                 exposure_variable_name = "E", 
                                 condition_names = NULL, sep = "_", ...,
                                 description = NULL){
  
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


