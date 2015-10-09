#' Declare the data-generating process of a variable
#'
#' @param population A population object, made with \code{\link{declare_population}}
#' @param sampling A sampling object, made with \code{\link{declare_sampling}}
#' @param sims number of iterations
#' @param label label for the simulation
#' @param analysis_labels labels for each analysis
#' @importFrom foreach foreach registerDoSEQ getDoParWorkers %dopar%
#' @importFrom doRNG %dorng%
#' @export
diagnose <- function(population = NULL, sampling = NULL, assignment, estimator, 
                     potential_outcomes = NULL,
                     sims = 5, label = NULL){
  
  super_population <- population$super_population
  
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
      
      sample_data <- draw_sample(population_data = population_data, sampling = sampling)
      
      sample_data <- reveal_assignment(data = sample_data, assignment = assignment)
      
      estimates <- get_estimates(estimator = estimator,data = sample_data)
      
      estimands <- get_estimands(estimator = estimator,
                                 sample_data = sample_data,
                                 population_data = population_data)
      
    } else {
      
      population_data <- reveal_assignment(data = population_data, assignment = assignment)
      
      estimates <- get_estimates(estimator = estimator,data = population_data)
      
      estimands <- get_estimands(estimator = estimator,
                                 population_data = population_data)
      
    }
    
    return(list(estimates, estimands))
    
  }
  
  return_object <- list(estimates = simulations_list[[1]], 
                        estimands = simulations_list[[2]],
                        label = label)
  
  class(return_object) <- "diagnosis"
  
  return(return_object)
  
}



#' @export
calculate_PATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]))
  
  if(class(SATE) == "matrix")
    PATE <- apply(SATE, 1, mean, na.rm = T)
  else
    PATE <- mean(SATE, na.rm = T)
  
  return(PATE)
}


#' @export
calculate_sd_SATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]))
  
  if(class(SATE) == "matrix")
    sd_SATE <- apply(SATE, 1, sd, na.rm = T)
  else
    sd_SATE <- sd(SATE, na.rm = T)
  
  return(sd_SATE)
}

#' @export
calculate_power <- function(estimates, ...){
  p <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["p", , drop = FALSE]))
  
  if(class(p) == "matrix")
    power <- apply(p < .05, 1, mean, na.rm = T)
  else
    power <- mean(p < .05, na.rm = T)
  
  return(power)
}

#' @export
calculate_RMSE <- function(estimates, estimands, ...){
  error <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - 
                                                                estimands[[i]]))
  
  if(class(error) == "matrix")
    RMSE <- apply(error, 1, function(x) sqrt(mean(x^2, na.rm = T)))
  else
    RMSE <- sqrt(mean(error^2, na.rm = T))
  
  return(RMSE)
}

#' @export
calculate_bias <- function(estimates, estimands, ...){
  
  PATE <- calculate_PATE(estimands = estimands)
  
  est_PATE_diff <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - PATE))
  
  if(class(est_PATE_diff) == "matrix")
    bias <- apply(est_PATE_diff, 1, mean, na.rm = T)
  else
    bias <- mean(est_PATE_diff, na.rm = T)
  
  return(bias)
}

#' @export
calculate_coverage <- function(estimates, estimands, ...){
  PATE <- calculate_PATE(estimands = estimands)
  
  ci_covers_estimate <- sapply(1:length(estimates), function(i) as.numeric(PATE <= estimates[[i]]["ci_upper", , drop = FALSE] & 
                                                                             PATE >= estimates[[i]]["ci_lower", , drop = FALSE]))
  
  if(class(ci_covers_estimate) == "matrix")
    coverage <- apply(ci_covers_estimate, 1, mean, na.rm = T)
  else
    coverage <- mean(ci_covers_estimate, na.rm = T)
  
  return(coverage)
}


#' @export
summary.diagnosis <- function(object, statistics = list(calculate_PATE, calculate_sd_SATE, calculate_power, 
                                                        calculate_RMSE, calculate_bias, calculate_coverage), 
                              labels = c("PATE", "sd(SATE)", "Power", "RMSE", "Bias", "Coverage"), ...){
  
  ## extract names of statistics objects
  if(is.null(labels)){
    if(class(statistics) == "list")
      labels <- paste(substitute(statistics)[-1L])
    else
      labels <- paste(substitute(statistics))
  }
  
  if(class(statistics) != "list"){ statistics <- list(statistics) }
  
  estimates <- object$estimates
  estimands <- object$estimands

  return_matrix <- matrix(NA, nrow = ncol(estimates[[1]]), ncol = length(statistics), dimnames = list(colnames(estimates[[1]]), labels))
  for(k in 1:length(statistics))
    return_matrix[ , k] <- as.matrix(statistics[[k]](estimates = estimates, estimands = estimands))
  
  return(return_matrix)
}

#' @export
print.summary.diagnosis <- function(x, ...){
  print(summary.diagnosis(x, ... = ...))
  return()
}


#' @export
print.diagnosis <- function(x, ...){
  print(summary.diagnosis(x, ... = ...))
  return()
}

reorient <- function(x) {
  obj <- c(x)
  names(obj) <- rep(paste(rownames(x), colnames(x), sep = "_"), each = ncol(x))
  return(obj)
}

