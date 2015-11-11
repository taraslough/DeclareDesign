#' Implement a declared sampling strategy and assignment procedure
#' 
#' @param data 
#' @param design 
#' @param random_seed 
#'
#' @return data.frame of the sample if there is a sampling strategy declared and with assigned treatment vector, if defined
#'
#' @export
implement_design <- function(data, design, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  sampling <- design$sampling
  assignment <- design$assignment

  if(!is.null(sampling)){
    data <- draw_sample(data = data, sampling = sampling)
  }
  
  if(!is.null(assignment)){ 
    data <- assign_treatment(data = data, assignment = assignment)
  }
  
  return(data)
  
}
