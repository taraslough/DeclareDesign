
#' @export
default_attrition_function <- function(data, condition_names, 
                                           treatment_variable,
                                           proportion_always_reporters = NULL,
                                           reporting_proportions = NULL){
  
  if(!is.null(reporting_proportions) & !is.null(proportion_always_reporters)){
    stop("Please do not specify reporting_proportions and proportion_always_reporters together. Instead, specify one or the other.")
  }
  if(is.null(reporting_proportions) & is.null(proportion_always_reporters)){
    stop("Please specify either reporting_proportions and proportion_always_reporters.")
  }
  
  Z <- data[,treatment_variable]
  R <- rep(NA, nrow(data))  
  
  if(!is.null(reporting_proportions)){
  for(i in 1:length(condition_names)){
    R[Z==condition_names[i]] <- rbinom(n = sum(Z==condition_names[i]), 
                                       size = 1, 
                                       prob = reporting_proportions[i])
  }
  }
  
  if(!is.null(proportion_always_reporters)){
    R <- rbinom(n = nrow(data), 1, prob = proportion_always_reporters)
  }
  
  return(R)
  
}



#' @export
declare_attrition <- function(attrition_function = default_attrition_function,
                                  formula = NULL, 
                                  outcome_name = "R", 
                                  condition_names = NULL, sep = "_", 
                                  treatment_variable = NULL, ...){
    
    outcomes_object <- 
      declare_potential_outcomes(potential_outcomes_function = attrition_function, 
                                 formula = formula, 
                                 outcome_name = outcome_name, 
                                 condition_names = condition_names, 
                                 sep = sep, 
                                 treatment_variable = treatment_variable, 
                                 potential_outcomes_type = "attrition",
                                 ... = ...)
    return(outcomes_object)
}



