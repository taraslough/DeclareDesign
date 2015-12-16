#' Implement a declared sampling strategy and assignment procedure
#' 
#' @param data A data.frame used to draw a sample and assign treatment(s) based on a design.
#' @param design A design object created by \code{declare_design}.
#'
#' @return data.frame of the sample if there is a sampling strategy declared and with assigned treatment vector, if defined
#'
#' @export
implement_design <- function(data, design) {
  
  design <- clean_inputs(design, object_class = "design", accepts_list = FALSE)
  
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
