#' Draw mock data based on a design
#'
#' @param population 
#' @param sampling 
#' @param assignment 
#' @param potential_outcomes 
#' @param random_seed 
#'
#' @export
draw_data <- function(design, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  population <- design$population
  potential_outcomes <- design$potential_outcomes
  sampling <- design$sampling
  assignment <- design$assignment
  
  data <- draw_population(population = population, potential_outcomes = potential_outcomes)
  
  if(!is.null(sampling)){
    data <- draw_sample(data = data, sampling = sampling)
  }
  
  if(!is.null(assignment)){ 
    data <- assign_treatment(data = data, assignment = assignment)
    
    data <- draw_outcome(data = data, potential_outcomes = potential_outcomes)
  }
  
  return(data)
  
}
