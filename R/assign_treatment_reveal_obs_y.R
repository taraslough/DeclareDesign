#' Assign treatment status
#'
#' Description
#' @param design A design object created by declare_design; or a function that assigns treatment
#' @param ... Options to override defaults in random assignmetn function
#' @return a random assignment vector
#' @examples
#' # some examples go here
#' @export
assign_treatment <- function(design, ...) {
  
  ## should be expanded to take either a design object or a function
  ## may be need to have this respond to the characteristics of the data frame, i.e. N and m?

  return(design$ra_fun(...))
  
}

#' Reveal observed outcomes based on a given treatment assignment
#'
#' Description
#' @param outcome A character string
#' @param treatment_assignment A string indicating the name of the realized treatment assignment vector 
#' @param data A data frame including the outcome and realized treatment assignment vectors indicating by outcome and treatment_assignment
#' @return an outcome vector of observed y
#' @examples
#' # some examples go here
#' @export

observed_outcome <- function(outcome = "Y", treatment_assignment, design, data, sep = "_"){
  
  if(any(is.na(data[,treatment_assignment]))>0)
    warning("There are NA's in the treatment assignment vector.")

  observed_y <- rep(NA, nrow(data))
  condition_names <- design$condition_names 
  for(v in condition_names){
    treat_cond <- data[,treatment_assignment] == (which(condition_names %in% v) - 1)
    observed_y[treat_cond] <- data[treat_cond, paste0(outcome, sep, v)]
  }
  
  return(observed_y)
  
}
