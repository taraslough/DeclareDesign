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
diagnose <- function(population = NULL, sampling = NULL, 
                     design, analysis, sims = 5, label = NULL, analysis_labels = NULL){
  
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  if(class(analysis)=="analysis"){analysis <- list(analysis)}
  
  comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  
  if(getDoParWorkers() == 1){ 
    registerDoSEQ()
  }
  
  simulations_list <- foreach(i = 1:sims, .combine = 'comb', .multicombine = TRUE, .init = list(list(), list(), list())) %dorng% {
    
    population_data <- draw_population(population = population)
    
    if(!is.null(sampling)){
      
      sample_data <- draw_sample(population_data = population_data, sampling = sampling)
      
      sample_data <- reveal_design(data = sample_data, design = design)
      
      estimates <- get_estimates(analysis, data = sample_data, analysis_labels = analysis_labels)
      sample_estimands <- get_estimands(analysis, data = sample_data, analysis_labels = analysis_labels,
                                        design = design)
      
    } else {
      
      population_data <- reveal_design(data = population_data, design = design)
      
      estimates <- get_estimates(analysis, data = population_data, analysis_labels = analysis_labels)
      sample_estimands <- get_estimands(analysis, data = population_data, analysis_labels = analysis_labels,
                                        design = design)
      
    }
    
    population_estimands <- get_estimands(analysis, data = population_data, 
                                          analysis_labels = analysis_labels,
                                          design = design)
    
    return(list(estimates, sample_estimands, population_estimands))
    
  }
  
  return_object <- list(estimates = simulations_list[[1]], 
                        sample_estimands = simulations_list[[2]],
                        population_estimands = simulations_list[[3]], 
                        label = label)
  
  class(return_object) <- "diagnosis"
  
  return(return_object)
  
}

#' @export
calculate_PATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]["est", , drop = FALSE]))
  
  if(class(SATE) == "matrix")
    PATE <- apply(SATE, 1, mean, na.rm = T)
  else
    PATE <- mean(SATE, na.rm = T)
  
  return(PATE)
}


#' @export
calculate_sd_SATE <- function(estimands, ...){
  SATE <- sapply(1:length(estimands), function(i) as.numeric(estimands[[i]]["est", , drop = FALSE]))
  
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
    power <- apply(p, 1, mean, na.rm = T)
  else
    power <- mean(p, na.rm = T)
  
  return(power)
}

#' @export
calculate_RMSE <- function(estimates, estimands, ...){
  error <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - 
                                                                estimands[[i]]["est", , drop = FALSE]))
  
  if(class(error) == "matrix")
    RMSE <- apply(error, 1, function(x) sqrt(mean(x^2, na.rm = T)))
  else
    RMSE <- sqrt(mean(error^2, na.rm = T))
  
  return(RMSE)
}

#' @export
calculate_bias <- function(estimates, estimands, ...){
  
  est_PATE_diff <- sapply(1:length(estimates), function(i) as.numeric(estimates[[i]]["est", , drop = FALSE] - 
                                                                        calculate_PATE(estimands = estimands)))
  
  if(class(est_PATE_diff) == "matrix")
    bias <- apply(est_PATE_diff, 1, mean, na.rm = T)
  else
    bias <- mean(est_PATE_diff, na.rm = T)
  
  return(bias)
}

#' @export
calculate_coverage <- function(estimates, estimands, ...){
  ci_covers_estimate <- sapply(1:length(estimates), function(i) as.numeric(calculate_PATE(estimands = estimands) <= 
                                                                             estimates[[i]]["ci_upper", , drop = FALSE] & 
                                                                             calculate_PATE(estimands = estimands) >= estimates[[i]]["ci_lower", , drop = FALSE]))
  
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
  sample_estimands <- object$sample_estimands
  population_estimands <- object$population_estimands
  
  return_matrix_population <- matrix(NA, nrow = ncol(estimates[[1]]), ncol = length(statistics), dimnames = list(colnames(estimates[[1]]), labels))
  for(k in 1:length(statistics))
    return_matrix_population[ , k] <- as.matrix(statistics[[k]](estimates = estimates, estimands = population_estimands))
  
  if(!is.null(sample_estimands)){
    return_matrix_sample <- matrix(NA, nrow = ncol(estimates[[1]]), ncol = length(statistics), dimnames = list(colnames(estimates[[1]]), labels))
    for(k in 1:length(statistics))
      return_matrix_sample[ , k] <- as.matrix(statistics[[k]](estimates = estimates, estimands = sample_estimands))
  }
  
  return(list(population = return_matrix_population,
              sample = return_matrix_sample))
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

