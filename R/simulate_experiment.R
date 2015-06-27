#' Declare the data-generating process of a variable
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param sims number of iterations
#' @export
simulate_experiment <- function(data = NULL, potential_outcomes = NULL, covariates = NULL, blocks = NULL, clusters = NULL,
                                design, analysis, sims = 100){
  
  if(is.null(data) & is.null(covariates) & is.null(potential_outcomes)) {
    stop("You must provide one or more of data, covariates, and potential_outcomes.")
  }
  
  if(!is.null(data) & !is.null(covariates)){
    warning("You specified both data and covariates. The function will resample covariates, replacing those in the data argument.")
  }
  
  resample_covariates <- !is.null(covariates) & is.null(data)
  resample_potential_outcomes <- !is.null(potential_outcomes)
  
  if(!is.null(data))
    data_sim <- data
  
  estimates_list <- list()
  estimands_list <- list()
  for(i in 1:sims){
    
    if(resample_covariates == TRUE & resample_potential_outcomes == TRUE){
      data_sim <- make_data(potential_outcomes = potential_outcomes, covariates = covariates, blocks = blocks, clusters = clusters)
    }
    
    #if(resample_covariates == FALSE & resample_potential_outcomes = TRUE){
    #  data_sim <- make_data(potential_outcomes = potential_outcomes, covariates = data_sim, blocks = blocks, clusters = clusters)
    #}
    ##if(resample_covariates == FALSE & resample_potential_outcomes = FALSE){
    ##  data_sim <- make_data(potential_outcomes = data_sim, covariates = data_sim, blocks = blocks, clusters = clusters)
    ##}
    
    dummy_assignment <- assign_treatment(design = design, data = data_sim)
   
    if(class(analysis)=="analysis"){analysis <- list(analysis)}
    
    for(j in 1:length(analysis)){
      data_sim[, analysis_treatment_variable(analysis = analysis[[j]])] <- dummy_assignment
      
      data_sim[, analysis_outcome_variable(analysis = analysis[[j]])] <- 
        observed_outcome(outcome = analysis_outcome_variable(analysis[[j]]),  
                         treatment_assignment = analysis_treatment_variable(analysis[[j]]),
                         data = data_sim)
    }
    
    estimates_list[[i]] <- get_estimates(analysis, data = data_sim)
    estimands_list[[i]] <- get_estimands(analysis, data = data_sim)
    
  }
  
  ##estimates <- do.call("rbind", lapply(estimates_list, reorient))
  ##estimands <- do.call("rbind", lapply(estimands_list, reorient))
  
  return_object <- list(estimates = estimates_list, estimands = estimands_list)
  class(return_object) <- "experiment_simulations"
  
  return(return_object)
  
}

#' @export
summary.experiment_simulations <- function(object, ...) {
  
  estimates <- object$estimates
  estimands <- object$estimands
  
  summary_array <- array(NA, dim = c(ncol(estimates[[1]]), 4, length(estimates)),
                         dimnames = list(colnames(estimates[[1]]),c("sate", "sate_hat", "error", "p"), 
                                         1:length(estimates)))
  
  for(i in 1:length(object$estimates)){
    
    for(q in 1:ncol(estimates[[i]])){
      
      sate <- estimands[[i]]["est", q]
      sate_hat <- estimates[[i]]["est", q]
      error <- sate_hat - sate
      p <- estimates[[i]]["p", q]
      
      summary_array[q, , i] <- c(sate, sate_hat, error, p)
      
    }
  }
  
  
  if(dim(summary_array)[1]==1){
    PATES <- mean(summary_array[,1,])
    simulation_errors <- sd(summary_array[,1,])
    powers <- mean(summary_array[,4,] < 0.05)
    RMSES <- sqrt(mean((summary_array[,1,] - summary_array[,2,])^2))
    bias <- mean(summary_array[,1,] - PATES)
    
    summ <- cbind(PATES, simulation_errors, powers, RMSES, bias)
    
    #structure(summ, class = c("summary.experiment_simulations", class(summ)))
    return(summ)
  }
  
  PATES <- apply(summary_array[,1,], 1 , mean)
  simulation_errors <- apply(summary_array[,1,], 1 , sd)
  powers <- apply(summary_array[,4,], 1 , function(x) mean(x < 0.05))
  RMSES <- apply(summary_array[,1,] - summary_array[,2,], 1 , function(x) sqrt(mean(x^2)))
  bias <- apply(summary_array[,2,] - PATES, 1, mean)
  
  summ <- cbind(PATES, simulation_errors, powers, RMSES, bias)
  
  #structure(summ, class = c("summary.experiment_simulations", class(summ)))
  return(summ)
}

#' @export
print.summary.experiment_simulations <- function(x, ...){
  ## prints paragraph describing design
  print("Design Descri")
}

reorient <- function(x) {
  obj <- c(x)
  names(obj) <- rep(paste(rownames(x), colnames(x), sep = "_"), each = ncol(x))
  return(obj)
}

#' Plot power across values of a parameter like the sample size
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param vary_parameter indicating which parameter is varied, default is "N"
#' @param vary_sequence set of values of the parameter to calculate power for
#' @param sims number of iterations
#' @export
plot_power <- function(data, design, analysis, vary_parameter = "N", vary_sequence){
  
  if(vary_parameter != "N")
    stop("The power analysis plot is only implemented for varying N at first.")
  
  if(missing(vary_sequence)){
    vary_sequence <- seq(from = 0, to = 1, by = .1)
  }
  
  power_sequence <- rep(NA, length(vary_sequence))
  for(parameter in vary_sequence)
    power_sequence[i] <- power(data = data, design = design, analysis = analysis,
                               N = parameter)
  
  return(power_sequence)
  ##plot(vary_sequence, power_sequence, axes = F, las = 1)
  ##axis(1)
  ##axis(2)
  
}

