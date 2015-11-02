### NOT YET WRITTEN:

# is wrapper for po.  may specify baseline condition

#' @export
default_noncompliance_function <- function(data, condition_names, 
                                           baseline_condition, 
                                           treatment_variable,
                                           compliance_proportions){
  
  Z <- data[,treatment_variable]
  D <- rep(baseline_condition, nrow(data))
  
  for(i in 1:length(condition_names)){
    D[Z==condition_names[i]] <- sample(x = c(baseline_condition, condition_names[i]),
                                       size = sum(Z==condition_names[i]),
                                       replace = TRUE,
                                       prob = c(1-compliance_proportions[i], compliance_proportions[i]))
  }
  
  return(D)
  
}



#' @export
declare_noncompliance <- function(noncompliance_function = default_noncompliance_function,
                                  formula = NULL, 
                                  outcome_name = "D", 
                                  condition_names = NULL, sep = "_", 
                                  treatment_variable = NULL, ...){
    
    outcomes_object <- 
      declare_potential_outcomes(potential_outcomes_function = noncompliance_function, 
                                 formula = formula, 
                                 outcome_name = outcome_name, 
                                 condition_names = condition_names, 
                                 sep = sep, 
                                 treatment_variable = treatment_variable, 
                                 potential_outcomes_type = "noncompliance",
                                 ... = ...)
    return(outcomes_object)
}



