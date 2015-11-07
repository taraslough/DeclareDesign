#' Diagnose the properties of a research design
#' 
#' @param design A design object created by \code{\link{declare_design}}.
#' @param diagnosis A diagnosis object created by \code{\link{diagnose_design}}. This allows you to calculate additional summary statistics for existing diagnosis simulations.
#' @param statistics A list of statistic functions that take a list of estimates, sample_estimands, and/or population_estimands and return a statistic (scalar) and a label.
#' @param population_draws Number of draws of the population, if using super-population.
#' @param sample_draws Number of draws of a sample from each population draw.
#' @param ... 
#'
#' @importFrom foreach foreach registerDoSEQ getDoParWorkers %dopar%
#' @importFrom doRNG %dorng%
#' @export
diagnose_design <- function(design = NULL, diagnosis = NULL, statistics = list(calculate_mean_PATE, calculate_sd_PATE, calculate_superpopulation_RMSE, calculate_superpopulation_bias, calculate_superpopulation_coverage, calculate_superpopulation_type_S_rate, calculate_superpopulation_exaggeration_ratio,
                                                                               calculate_population_RMSE, calculate_population_bias, calculate_population_coverage, calculate_population_type_S_rate, calculate_population_exaggeration_ratio,
                                                                               calculate_mean_SATE, calculate_sd_SATE, calculate_sample_RMSE, calculate_sample_bias, calculate_sample_coverage, calculate_sample_type_S_rate, calculate_sample_exaggeration_ratio,
                                                                               calculate_mean_power, calculate_sd_power, calculate_mean_estimate, calculate_sd_estimate),
                            population_draws = 10, sample_draws = 5, ...){
  
  ## extract names of statistics objects
  if(class(statistics) == "list")
    labels <- paste(substitute(statistics)[-1L])
  else
    labels <- paste(substitute(statistics))
  
  if(class(statistics) != "list"){ statistics <- list(statistics) }
  
  if( (is.null(design) & is.null(diagnosis)) | (!is.null(design) & !is.null(diagnosis)) ){
    stop("Please provide either a design object created by declare_design or a diagnosis object created with diagnose_design, and not both.")
  }
  
  if(is.null(diagnosis)){
    
    population <- design$population
    sampling <- design$sampling
    assignment <- design$assignment
    estimator <- design$estimator
    potential_outcomes <- design$potential_outcomes
    label <- design$label
    
    super_population <- population$super_population
    
    if(super_population==FALSE){
      population_data <- draw_population(population = population, 
                                         potential_outcomes = potential_outcomes)
    }
    
    ## this is the function that recombines the lists of estimators and estimands during the parallel loop
    comb <- function(x, ...) {
      lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
    }
    
    if(getDoParWorkers() == 1){ 
      registerDoSEQ()
    }
    
    simulations_list <- foreach(i = 1:population_draws, .combine = 'comb', .multicombine = TRUE, .init = list(list(), list(), list())) %dorng% {
      
      if(super_population==TRUE){
        population_data <- draw_population(population = population, potential_outcomes = potential_outcomes)
      }
      
      population_estimands <- get_estimands(estimator = estimator, data = population_data)
      
      if(!is.null(sampling)){
        
        estimates <- list()
        sample_estimands <- list()
        for(j in 1:sample_draws){
          sample_data <- draw_sample(data = population_data, sampling = sampling)
          
          sample_data <- assign_treatment(data = sample_data, assignment = assignment)
          
          sample_data <- draw_outcome(data = sample_data, potential_outcomes = potential_outcomes)
          
          estimates[[j]] <- get_estimates(estimator = estimator, data = sample_data)
          
          sample_estimands[[j]] <- get_estimands(estimator = estimator, data = sample_data)
        }
        
      } else {
        
        population_data <- assign_treatment(data = population_data, assignment = assignment)
        
        population_data <- draw_outcome(data = population_data, potential_outcomes = potential_outcomes)
        
        estimates <- list(get_estimates(estimator = estimator, data = population_data))
        
        sample_estimands <- list(NA)
        
      }
      
      return(list(population_estimands, sample_estimands, estimates))
      
    }
    
    population_estimands <- simulations_list[[1]]
    sample_estimands <- simulations_list[[2]]
    estimates <- simulations_list[[3]]
    
  } else {
    
    ## if an existing diagnosis is provided, instead take the existing simulations
    
    population_estimands <- diagnosis$population_estimands
    sample_estimands <- diagnosis$sample_estimands
    estimates <- diagnosis$estimates
    
  }
  
  return_matrix <- matrix(NA, nrow = length(statistics), ncol = ncol(estimates[[1]][[1]]), 
                          dimnames = list(labels, colnames(estimates[[1]][[1]])))
  for(k in 1:length(statistics)){
    statistic <- statistics[[k]](estimates = estimates, population_estimands = population_estimands, sample_estimands = sample_estimands)
    return_matrix[k, ] <- as.matrix(statistic$statistic)
    if(!is.null(statistic$label)){
      rownames(return_matrix)[k] <- statistic$label
    }
  }
  
  return_list <- list(diagnosis = return_matrix, estimates = estimates, population_estimands = population_estimands, sample_estimands = sample_estimands)
  
  structure(return_list, class = "diagnosis")
  
}

#' @export
print.diagnosis <- function(x, ...){
  print(x$diagnosis)
  return()
}

#' @export
summary.diagnosis <- function(object, ...){
  print(object$diagnosis)
  return()
}
