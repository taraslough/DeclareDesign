
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



#' Attrition declarations
#' 
#' Attrition declarations are formally identical to potential_outcomes declarations. You provide a function of data (which includes a treatment assignment).  This function returns a vector of length N with 1's for units that report their outcomes and 0's for units that attrit.
#' The built-in default function allows for two kinds of noncompliance: the proportion of subjects who report may vary with the treatment assignment, or the proportion of subjects who report (regardless of their condition, i.e. Always-Reports) can be set.
#' 
#' @param attrition_function A function of data that returns a vector of 0s and 1s that indicate which subjects report their outcomes.
#' @param formula An optional formula to be passed to attrition_function
#' @param outcome_name The name of the variable describing whether outcomes are reported. Defaults to "R".
#' @param condition_names An optional vector of treatment conditions to be passed to attrition_function.
#' @param sep A character string describing the separator for concatenating outcomes and conditions. Defaults to "_".
#' @param treatment_variable The name of the treatment assignment variable
#' @param ... optional additional arguments to be passed to attrition_function.
#' 
#' @examples 
#'  population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' attrition_1 <- declare_attrition(condition_names = c(0,1), 
#'                                  outcome_name = "R1",
#'                                  treatment_variable = "Z", 
#'                                  reporting_proportions = c(.5, .7))
#' 
#' attrition_2 <- declare_attrition(condition_names = c(0,1), 
#'                                  outcome_name = "R2",
#'                                  treatment_variable = "Z", 
#'                                  proportion_always_reporters = .8)
#' 
#' assignment <- declare_assignment(condition_names = c(0,1))
#' 
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' smp_draw <- draw_outcome(data = smp_draw, 
#'                          potential_outcomes = list(attrition_1, attrition_2))
#' with(smp_draw, table(Z, R1))
#' with(smp_draw, table(Z, R2))
#' 
#' @return A potential_outcomes object

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



