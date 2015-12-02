
#' Compare designs
#' 
#' Create a diagnosis of two or more designs
#' 
#' @param design 
#' @param population 
#' @param sampling 
#' @param assignment 
#' @param estimator 
#' @param potential_outcomes 
#' @param inputs 
#' @param size 
#' @param labels 
#' @param population_draws 
#' @param sample_draws 
#'
#' @export
compare_designs <- function(design, population = NULL, sampling = NULL, assignment = NULL, estimator = NULL, 
                            potential_outcomes = NULL, inputs = NULL, size = NULL, labels = NULL,
                            population_draws = 5, sample_draws = 5){
  
  design <- check_inputs(design, "design", accepts_list = TRUE)
  
  population <- clean_inputs(population, "population", accepts_list = TRUE)
  sampling <- clean_inputs(sampling, "sampling", accepts_list = TRUE)
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  estimator <- clean_inputs(estimator, "estimator", accepts_list = TRUE)
  potential_outcomes <- clean_inputs(potential_outcomes, c("potential_outcomes", "interference"), accepts_list = TRUE)
  inputs <- clean_inputs(inputs, "inputs", accepts_list = TRUE)
  
  comparison_counts <- c(length(size), length(population), length(sampling), length(assignment),
                         length(estimator), length(potential_outcomes), length(inputs))
  
  if(any(comparison_counts) > 0 & length(design) > 1){
    stop("If you provide more than one design, please do not provide other components of a design, including population, sampling, assignment, estimator, potential_outcomes, or inputs.")
  }
  
  if(length(design) == 1 & all(comparison_counts) == 0){
    stop("Please provide either more than one design or more than one components of a design for comparison.")
  }
  
  if(any(comparison_counts != max(comparison_counts) & comparison_counts > 1))
    stop("Please provide either no inputs for a given option (for example, not providing any size), one input (for example a single value of size), or more than one input (for example size = c(500, 1000)). The set of varying inputs must be the same length (you cannot provide three values of size and two values of assignment).")
  
  population_compare <- population
  sampling_compare <- sampling
  assignment_compare <- assignment
  estimator_compare <- estimator
  potential_outcomes_compare <- potential_outcomes
  inputs_compare <- inputs
  
  ## loop over different configurations of experiments
  diagnoses <- list()
  for(e in 1:max(comparison_counts)){
    
    if(!is.null(size)){
      population_compare[[min(length(population_compare), e)]] <- substitute_input(population_compare[[min(length(population_compare), e)]], "size", size[e])
    }
    
    tmp_design <- declare_design(population = population_compare[[min(length(potential_outcomes_compare), e)]],
                                 sampling = sampling_compare[[min(length(potential_outcomes_compare), e)]],
                                 assignment = assignment_compare[[min(length(potential_outcomes_compare), e)]],
                                 estimator = estimator_compare[[min(length(potential_outcomes_compare), e)]],
                                 potential_outcomes = potential_outcomes_compare[[min(length(potential_outcomes_compare), e)]],
                                 inputs = inputs_compare[[min(length(potential_outcomes_compare), e)]],
                                 label = labels[[e]])
    
    diagnoses[[e]] <- diagnose_design(design = tmp_design, population_draws = population_draws, sample_draws = sample_draws)
    
  }
  
  class(diagnoses) <- "diagnosis.list"
  
  return(diagnoses)
  
}

exists_input <- function(object, input_name){
  call <- object$call
  
  return(is.null(call[[input_name]]))
}

substitute_input <- function(object, input_name, input_value){
  call <- object$call
  
  if(!(is.null(input_value) & is.null(call[[input_name]])))
    call[[input_name]] <- input_value
  
  eval(call)
}
