#' Default attrition function
#' 
#' This default function allows the user to specifiy two common forms of attrition.  If you use the proportion_always_reporters argument, you specificy the portion of the experimental sample that reports their outcomes, regardless of their treatment assignment.  If you use the reporting_proportions argument, you provide a vector of proportions that is equal in length to condition_names.
#' 
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @param condition_names A vector describing the conditions to which subjects can be assigned. Often inherited from \code{\link{declare_attrition}}.
#' @param assignment_variable_name The name of the treatment variable.
#' @param proportion_always_reporters The proportion of the sample that reports their outcomes regardless of their treatment condition.
#' @param reporting_proportions A vector of proportions that describes the proportion of subjects who report in each condition. Each entry in this vector must be a number between 0 and 1.
#'
#' @examples 
#' population <- declare_population(noise = declare_variable(), size = 1000)
#' sampling <- declare_sampling(n = 500)
#' attrition <- declare_attrition(condition_names = c(0,1), 
#'                                outcome_variable_name = "R",
#'                                assignment_variable_name = "Z", 
#'                                reporting_proportions = c(.5, .7))
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z", 
#'                                                  attrition = attrition)
#'                                                    
#' assignment <- declare_assignment(condition_names = c(0,1))
#' 
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes,
#'                          attrition = attrition)
#' with(smp_draw, table(Z, R))
#'
#' @export
default_attrition_function <- function(data, condition_names, 
                                       assignment_variable_name,
                                       proportion_always_reporters = NULL,
                                       reporting_proportions = NULL){
  
  if(!is.null(reporting_proportions) & !is.null(proportion_always_reporters)){
    stop("Please do not specify reporting_proportions and proportion_always_reporters together. Instead, specify one or the other.")
  }
  if(is.null(reporting_proportions) & is.null(proportion_always_reporters)){
    stop("Please specify either reporting_proportions and proportion_always_reporters.")
  }
  
  Z <- data[,assignment_variable_name]
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
#' @param outcome_variable_name The name of the variable describing whether outcomes are reported. Defaults to "R".
#' @param condition_names An optional vector of treatment conditions to be passed to attrition_function.
#' @param sep A character string describing the separator for concatenating outcomes and conditions. Defaults to "_".
#' @param assignment_variable_name The name of the treatment assignment variable
#' @param description A description of the attrition function in words.
#' @param ... optional additional arguments to be passed to attrition_function.
#' 
#' @examples 
#' population <- declare_population(noise = declare_variable(), size = 1000)
#' sampling <- declare_sampling(n = 500)
#' attrition <- declare_attrition(condition_names = c(0,1), 
#'                                outcome_variable_name = "R",
#'                                assignment_variable_name = "Z", 
#'                                reporting_proportions = c(.5, .7))
#' 
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  assignment_variable_name = "Z", 
#'                                                  attrition = attrition)
#'                                                    
#' assignment <- declare_assignment(condition_names = c(0,1))
#' 
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes,
#'                          attrition = attrition)
#' with(smp_draw, table(Z, R))
#' 
#' @return A potential_outcomes object
#' @export
declare_attrition <- function(attrition_function = default_attrition_function,
                              formula = NULL, 
                              outcome_variable_name = "R", 
                              condition_names, sep = "_", 
                              assignment_variable_name = "Z",
                              description = NULL, ...){
  
  if(missing(condition_names)){
    stop("Please provide condition_names.")
  }
  
  outcomes_object <- 
    declare_potential_outcomes(potential_outcomes_function = attrition_function, 
                               formula = formula, 
                               outcome_variable_name = outcome_variable_name, 
                               condition_names = condition_names,
                               sep = sep, 
                               assignment_variable_name = assignment_variable_name, 
                               description = description, ... = ...)
  class(outcomes_object) <- "attrition"
  return(outcomes_object)
}



