#' Draw a population data frame based on a declared data structure
#' 
#' @param population A population object describing the population data structure created by the \code{declare_population} function.
#' @param potential_outcomes A potential outcomes object created by the \code{potential_outcomes} object.
#' @param condition_names A character vector indicating which conditions to draw potential outcomes for (for example c("Z0", "Z1") would create "Y_Z0" and "Y_Z1" if the outcome is named "Y").
#'
#' @export
draw_population <- function(population, 
                            potential_outcomes = NULL, 
                            condition_names = NULL,
                            attrition = NULL) {
  
  # Do checks ---------------------------------------------------------------
  
  # Check whether sample provided
  if(missing(population)){
    stop("You must provide an argument to population, created with declare_population().")
  }
  
  # Get the covariates ------------------------------------------------------
  
  if(population$super_population == FALSE){
    current_seed <- .Random.seed
    set.seed(population$random_seed)
  }
  
  data <- draw_covariates(population = population)
  
  # Make potential outcomes -------------------------------------------------
  
  if(!is.null(potential_outcomes)){
    data <- draw_potential_outcomes(data = data,
                                    potential_outcomes = potential_outcomes, 
                                    condition_names = condition_names,
                                    attrition = attrition)
  }
  
  if(population$super_population == FALSE){
    set.seed(current_seed)
  }
  
  # Return data -------------------------------------------------------------
  
  return(data)
  
}

draw_covariates <- function(population){
  if (class(population) != "population" )
    stop("Please send the population argument an object created using declare_population. You can send just a data frame to declare_population to use your own fixed data.")
  if(class(population$population) == "function")
    return(population$population())
  else if(class(population$population) == "data.frame")
    return(population$population)
}

