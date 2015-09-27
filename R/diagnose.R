#' Declare the data-generating process of a variable
#'
#' @param population A population object, made with \code{\link{declare_population}}
#' @param sampling A sampling object, made with \code{\link{declare_sampling}}
#' @param potential_outcomes A potential outcomes object, made with \code{\link{declare_potential_outcomes}}
#' @param sims number of iterations
#' @param label label for the simulation
#' @param analysis_labels labels for each analysis
#' @importFrom foreach foreach registerDoSEQ getDoParWorkers %dopar%
#' @importFrom doRNG %dorng%
#' @export
diagnose <- function(population = NULL, sampling = NULL, potential_outcomes = NULL, 
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
  
  simulations_list <- foreach(i = 1:sims, .combine = 'comb', .multicombine = TRUE, 
                              .init = list(list(), list(), list())) %dorng% {
    
    population_data <- draw_population(population = population)
    
    if(!is.null(sampling)){
      
      sample_data <- draw_sample(population_data = population_data, sampling = sampling)
      
      sample_data <- reveal_design(data = sample_data, design = design)
      
      estimates <- get_estimates(analysis, data = sample_data, analysis_labels = analysis_labels)
      sample_estimands <- get_estimands(analysis, data = sample_data, analysis_labels = analysis_labels)
      
    } else {
      
      population_data <- reveal_design(data = population_data, design = design)
      
      estimates <- get_estimates(analysis, data = population_data, analysis_labels = analysis_labels)
      sample_estimands <- get_estimands(analysis, data = population_data, analysis_labels = analysis_labels)
      
    }
    
    population_estimands <- get_estimands(analysis, data = population_data, analysis_labels = analysis_labels)
    
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
PATE <- function(x, ...){
  
}

#' @export
sd_SATE <- function(x, ...){
  
}

#' @export
power <- function(x, ...){
  
}

#' @export
RMSE <- function(x, ...){
  
}

#' @export
bias <- function(x, ...){
  
}

#' @export
coverage <- function(x, q, ...){
  ci_covers_estimate <- lapply(1:length(x), function(i) sample_estimands[[i]]["est", q] <= estimates[[i]]["ci_upper", q] & 
                                 sample_estimands[[i]]["est", q] >= estimates[[i]]["ci_lower", q])
  
  coverage <- apply(summary_array[,"ci_covers_est", , drop = FALSE], 1, mean)
  
  return(coverage)
}


#' @export
summary_new.diagnosis <- function(object, statistics = list(PATE, sd_SATE, power, RMSE, bias, coverage), labels = NULL, ...){
  
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
  
  return_vector <- rep(NA, length(statistics))
  for(k in 1:length(statistics))
    return_vector[k] <- statistics[[k]](estimates = estimates, sample_estimands = sample_estimands, population_estimands = population_estimands)
  
  return(return_vector)
}


#' @export
summary.diagnosis <- function(object, ...) {
  
  estimates <- object$estimates
  sample_estimands <- object$sample_estimands
  
  summary_array <- array(NA, dim = c(ncol(estimates[[1]]), 5, length(estimates)),
                         dimnames = list(colnames(estimates[[1]]),
                                         c("sate", "sate_hat", "error", "p", "ci_covers_est"), 
                                         1:length(estimates)))
  for(i in 1:length(object$estimates)){
    
    for(q in 1:ncol(estimates[[i]])){
      
      sate <- sample_estimands[[i]]["est", q]
      sate_hat <- estimates[[i]]["est", q]
      error <- sate_hat - sate
      p <- estimates[[i]]["p", q]
      ci_covers_est <- sample_estimands[[i]]["est", q] <= estimates[[i]]["ci_upper", q] & 
        sample_estimands[[i]]["est", q] >= estimates[[i]]["ci_lower", q]
      
      summary_array[q, , i] <- c(sate, sate_hat, error, p, ci_covers_est)
      
    }
  }
  
  PATE <- apply(summary_array[,"sate",, drop = FALSE], 1 , mean)
  sd_SATE <- apply(summary_array[,"sate",, drop = FALSE], 1 , sd)
  power <- apply(summary_array[,"p", , drop = FALSE], 1 , function(x) mean(x < 0.05))
  RMSE <- apply(summary_array[,"sate", , drop = FALSE] - summary_array[,"sate_hat", , drop = FALSE], 1 , function(x) sqrt(mean(x^2)))
  bias <- apply(summary_array[,"sate_hat", , drop = FALSE] - PATE, 1, mean)
  coverage <- apply(summary_array[,"ci_covers_est", , drop = FALSE], 1, mean)
  
  summ <- cbind(PATE, sd_SATE, power, RMSE, bias, coverage)
  
  return(summ)
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

