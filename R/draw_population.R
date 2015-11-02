#' Make the full dataset or just a sample
#'
#' @export
draw_population <- function(population, 
                            potential_outcomes = NULL, 
                            condition_names = NULL) {
  
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
  
  data <- get_covariates(population = population)
  
  # Make potential outcomes -------------------------------------------------
  
  if(!is.null(potential_outcomes)){
    data <- draw_potential_outcomes(data = data, potential_outcomes = potential_outcomes, condition_names = condition_names)
  }
  
  if(population$super_population == FALSE){
    set.seed(current_seed)
  }
  
  # Return data -------------------------------------------------------------
  
  return(data)
  
}




#' @export
get_covariates <- function(population){
  if (class(population) != "population" )
    stop("Please send the population argument an object created using declare_population. You can send just a data frame to declare_population to use your own fixed data.")
  if(class(population$population) == "function")
    return(population$population())
  else if(class(population$population) == "data.frame")
    return(population$population)
}


#' @export
make_proportions <- function(population_proportions,N){
  
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  
  con_names <- rownames(population_proportions)
  
  outcomes <- apply(counts,2,function(times){
    sample(
      rep(con_names,times = times)
    )
  })
  
  colnames(outcomes) <- colnames(population_proportions)
  
  outcomes <- integerize(as.data.frame(outcomes))
  
  return(outcomes)
}


