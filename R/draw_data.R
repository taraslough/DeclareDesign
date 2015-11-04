#' @export
draw_data <- function(population, sampling = NULL, assignment = NULL, potential_outcomes = NULL, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
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
