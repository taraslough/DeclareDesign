#' Built-in noncompliance function (one-sided noncompliance)
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @param condition_names A vector describing the conditions to which subjects can be assigned. Often inherited from \code{\link{declare_noncompliance}}.
#' @param baseline_condition The value of condition_names that represents the "baseline" condition. This is the condition that subjects will be in if they do not comply with their treatment status. Often a "control" condition.
#' @param assignment_variable_name The name of the treatment variable.
#' @param compliance_proportions A vector of proportions that describes the proportion of subjects who comply in each condition. Each entry in this vector must be a number between 0 and 1.
#' 
#' @examples 
#' population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' noncompliance <- declare_noncompliance(condition_names = c(0,1), 
#'                                        assignment_variable_name = "Z", 
#'                                        compliance_proportions=c(1, .5), 
#'                                        baseline_condition=0)
#' assignment <- declare_assignment(condition_names = c(0,1))
#' 
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = noncompliance)
#'
#' head(smp_draw)
#' with(smp_draw, table(Z, D))
#' @export
default_noncompliance_function <- function(data, condition_names, 
                                           baseline_condition, 
                                           assignment_variable_name,
                                           compliance_proportions){
  
  Z <- data[,assignment_variable_name]
  D <- rep(baseline_condition, nrow(data))
  
  for(i in 1:length(condition_names)){
    D[Z==condition_names[i]] <- sample(x = c(baseline_condition, condition_names[i]),
                                       size = sum(Z==condition_names[i]),
                                       replace = TRUE,
                                       prob = c(1-compliance_proportions[i], compliance_proportions[i]))
  }
  
  return(D)
  
}


#' Noncompliance declarations
#' 
#' Noncompliance declarations are formally identical to potential_outcomes declarations. You provide a function of data (which includes a treatment assignment).  This function returns a vector of length N with the treatment as received.
#' The built-in default function implements one-sided noncompliance in which some subjects do not comply with their assigned treatment status.  Instead, they revert to a baseline condition (usually a control condition).
#' 
#' @param noncompliance_function A function of data that returns a vector of treatments as received.
#' @param formula An optional formula to be passed to noncompliance_function
#' @param outcome_variable_name The name of the variable describing treatment receipt. Defaults to "D".
#' @param condition_names An optional vector of treatment conditions to be passed to noncompliance_function
#' @param sep A character string describing the separator for concatenating outcomes and conditions. Defaults to "_".
#' @param assignment_variable_name The name of the treatment assignment variable
#' @param ... optional additional arguments to be passed to noncompliance_function
#' 
#' @return A potential_outcomes object
#' 
#' @examples 
#' population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' noncompliance <- declare_noncompliance(condition_names = c(0,1), 
#'                                        assignment_variable_name = "Z", 
#'                                        compliance_proportions=c(1, .5), 
#'                                        baseline_condition=0)
#' assignment <- declare_assignment(condition_names = c(0,1))
#' 
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = noncompliance)
#'
#' head(smp_draw)
#' with(smp_draw, table(Z, D))
#' @export
declare_noncompliance <- function(noncompliance_function = default_noncompliance_function,
                                  formula = NULL, 
                                  outcome_variable_name = "D", 
                                  condition_names, sep = "_", 
                                  assignment_variable_name = "Z", ...,
                                  description = NULL){
  
  if(missing(condition_names)){
    stop("Please provide condition_names.")
  }
  
  outcomes_object <- 
    declare_potential_outcomes(potential_outcomes_function = noncompliance_function, 
                               formula = formula, 
                               outcome_variable_name = outcome_variable_name, 
                               condition_names = condition_names,
                               sep = sep, 
                               assignment_variable_name = assignment_variable_name, 
                               description = description,
                               ... = ...)
  class(outcomes_object) <- "noncompliance"
  return(outcomes_object)
}



