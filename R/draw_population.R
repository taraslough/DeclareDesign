#' Draw a population data frame based on a declared data structure
#' 
#' @param population A population object describing the population data structure created by the \code{\link{declare_population}}.
#' @param potential_outcomes A potential outcomes object created by \code{\link{declare_potential_outcomes}}.
#' @param condition_names A character vector indicating which conditions to draw potential outcomes for (for example c("Z0", "Z1") would create "Y_Z0" and "Y_Z1" if the outcome is named "Y").
#' @param noncompliance A noncompliance object created by \code{\link{declare_noncompliance}} (optional).
#' @param attrition A noncompliance object created by \code{\link{declare_attrition}} (optional).
#' 
#' @return A data.frame
#' 
#' @examples
#' population <- declare_population(size = 850)
#' sampling <- declare_sampling(n=500)
#' pop_draw <- draw_population(population = population)
#'
#' @export
draw_population <- function(population, 
                            condition_names = NULL,
                            potential_outcomes = NULL, 
                            noncompliance = NULL,
                            attrition = NULL) {
  
  # Do checks ---------------------------------------------------------------
  
  population <- clean_inputs(population, object_class = "population", accepts_list = FALSE)
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = c("potential_outcomes", "interference"), accepts_list = TRUE)
  noncompliance <- clean_inputs(noncompliance, object_class = "noncompliance", accepts_list = FALSE)
  attrition <- clean_inputs(attrition, object_class = "attrition", accepts_list = FALSE)
  
  # Check whether sample provided
  if(missing(population)){
    stop("You must provide an argument to population, created with declare_population().")
  }
  
  # Get the covariates ------------------------------------------------------
  
  data <- draw_covariates(population = population)
  
  # Make potential outcomes -------------------------------------------------
  
  if(!is.null(potential_outcomes)){
    if(any(sapply(potential_outcomes, FUN = function(x) class(x)=="interference"))){
      warning("In the presence of interference, stable potential outcomes will not be drawn.")
    }else{
      data <- draw_potential_outcomes(data = data,
                                      condition_names = condition_names,
                                      potential_outcomes = potential_outcomes, 
                                      noncompliance = noncompliance,
                                      attrition = attrition)
    }
  }
  
  # Return data -------------------------------------------------------------
  
  return(data)
  
}

draw_covariates <- function(population){
  population <- clean_inputs(population, object_class = "population", accepts_list = FALSE)
  if(class(population$population) == "function")
    return(population$population())
  else if(class(population$population) == "data.frame")
    return(population$population)
}

