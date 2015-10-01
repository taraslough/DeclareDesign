#' Make the full dataset or just a sample
#'
#' @export
draw_population <- function(population, potential_outcomes = NULL) {
  
  # Do checks ---------------------------------------------------------------
  
  # Check whether sample provided
  if(missing(population)){
    stop("You must provide an argument to population, created with declare_population().")
  }
  
  # Get the covariates ------------------------------------------------------
  
  if(population$super_population == TRUE){
    set.seed(population$random_seed)
  }
  
  covariates <- get_covariates(population = population)
  
  # Ignore POs if super_population is not chosen or POs are missing -------------------------------------------------
  
  if(is.null(potential_outcomes)){
    
    population_data <- covariates
    
  } else {
    
    # Make potential outcomes -------------------------------------------------
    
    outcomes <- loop_potential_outcomes(
      potential_outcomes = population$potential_outcomes,
      covariates = covariates)
    
    population_data <- data.frame(outcomes, covariates)
    
  }
  
  # Return data -------------------------------------------------------------
  
  return(population_data)
  
}


#' @export
draw_sample <- function(population = NULL, population_data = NULL, sampling = NULL, noncompliance = NULL) {
  
  # Do checks ---------------------------------------------------------------
  
  if(all(is.null(population), is.null(population_data))){
    stop("Please provide either a population object created with declare_population() to population or a data frame created with draw_population() to population_data.")
  }
  
  if(all(!is.null(population), !is.null(population_data))){
    stop("Please only provide either a population object created with declare_population() to population or a data frame created with draw_population() to population_data.")
  }
  
  # Get the covariates ------------------------------------------------------
  
  if(is.null(population_data)){
    population_data <- draw_population(population = population)
  }
  
  # Construct strata and clusters if custom functions --------------------------------------------------
  
  if(!is.null(sampling$custom_cluster_function)){
    population_data[, sampling$cluster_variable_name] <- sampling$custom_cluster_function
  }
  
  if(!is.null(sampling$custom_strata_function)) { 
    population_data[, sampling$strata_variable_name] <- sampling$custom_strata_function
  }
  
  # Draw the sample ------------------------------------------------------
  
  sample_indicator <- draw_sample_indicator(sampling = sampling, population_data = population_data)
  
  population_data <- data.frame(population_data, sample_indicator)
  
  sample_data <- subset(population_data, sampled == 1, select = -c(sampled))
  
  # Return data -------------------------------------------------------------
  
  return(sample_data)
  
}
