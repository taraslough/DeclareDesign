#' Diagnose the properties of a research design
#' 
#' @param design 
#' @param simulations 
#' @param statistics 
#' @param labels 
#' @param sims 
#' @param ... 
#'
#' @export
diagnose_design <- function(design = NULL, diagnosis = NULL, statistics = list(calculate_PATE, calculate_sd_SATE, calculate_power, 
                                                                               calculate_RMSE, calculate_bias, calculate_coverage, 
                                                                               calculate_type_S_rate, calculate_exaggeration_ratio), 
                            sims = 5, ...){
  
  ## extract names of statistics objects
  if(is.null(labels)){
    if(class(statistics) == "list")
      labels <- paste(substitute(statistics)[-1L])
    else
      labels <- paste(substitute(statistics))
  }
  
  if(class(statistics) != "list"){ statistics <- list(statistics) }
  
  if( (is.null(design) & is.null(simulations)) | (!is.null(design) & !is.null(simulations)) ){
    stop("Please provide either a design object created by declare_design or a diagnosis object created with diagnose_design, and not both.")
  }
  
  if(is.null(diagnosis)){
    
    super_population <- population$super_population
    
    if(is.null(design)){
      if(class(estimator) == "list"){
        estimator_object_names <- paste(substitute(estimator)[-1L])
        for(i in 1:length(estimator)){
          if(is.null(estimator[[i]]$labels)){
            estimator[[i]]$labels <- estimator_object_names[i]
          }
        }
      } else if(class(estimator) == "estimator"){
        if(is.null(estimator$labels)){
          estimator$labels <- paste(substitute(estimator))
        }
      }
    }
    
    comb <- function(x, ...) {
      lapply(seq_along(x),
             function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
    }
    
    if(getDoParWorkers() == 1){ 
      registerDoSEQ()
    }
    
    if(super_population==FALSE){
      population_data <- draw_population(population = population, 
                                         potential_outcomes = potential_outcomes)
    }
    
    simulations_list <- foreach(i = 1:sims, .combine = 'comb', .multicombine = TRUE, .init = list(list(), list())) %dorng% {
      
      if(super_population==TRUE){
        population_data <- draw_population(population = population, potential_outcomes = potential_outcomes)
      }
      
      if(!is.null(sampling)){
        
        sample_data <- draw_sample(data = population_data, sampling = sampling)
        
        sample_data <- assign_treatment(data = sample_data, assignment = assignment)
        
        sample_data <- draw_outcome(data = sample_data, potential_outcomes = potential_outcomes)
        
        estimates <- get_estimates(estimator = estimator, data = sample_data)
        
        estimands <- get_estimands(estimator = estimator,
                                   sample_data = sample_data,
                                   population_data = population_data)
        
      } else {
        
        population_data <- assign_treatment(data = population_data, assignment = assignment)
        
        population_data <- draw_outcome(data = population_data, potential_outcomes = potential_outcomes)
        
        estimates <- get_estimates(estimator = estimator, data = population_data)
        
        estimands <- get_estimands(estimator = estimator, population_data = population_data)
        
      }
      
      return(list(estimates, estimands))
      
    }
    
    estimates <- simulations_list[[1]]
    estimands <- simulations_list[[2]]
    
  }
  
  return_matrix <- matrix(NA, nrow = ncol(estimates[[1]]), ncol = length(statistics), dimnames = list(colnames(estimates[[1]]), labels))
  for(k in 1:length(statistics)){
    statistic <- as.matrix(statistics[[k]](estimates = estimates, estimands = estimands))
    return_matrix[ , k] <- statistic$statistic
    colnames(return_matrix)[k] <- ifelse(is.null(statistic$label), label[k], statistic$label)
  }
  
  return_list <- list(diagnosis = return_matrix, simulations = simulations)
  
  structure(return_list, class = "diagnosis")
  
}

#' @export
print.diagnosis <- function(x, ...){
  print(x$diagnosis)
  return()
}

#' @export
summary.diagnosis <- function(x, ...){
  print(x$diagnosis)
  return()
}
