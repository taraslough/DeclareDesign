#' @export
reveal_design <- function(data, sampling = NULL, assignment = NULL, potential_outcomes = NULL, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  if(!is.null(sampling)){
    data <- draw_sample(data = data, sampling = sampling)
  }
  
  if(!is.null(assignment)){ 
    data <- assign_treatment(data = data, assignment = assignment)
    
    data <- draw_outcome(data = data, potential_outcomes = potential_outcomes)
  }
  
  return(data)
  
}
