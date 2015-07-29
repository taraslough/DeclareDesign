#' Declare the data-generating process of a variable
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param sims number of iterations
#' @export
simulate_experiment <- function(data = NULL, potential_outcomes = NULL, sample_frame = NULL, 
                                blocks = NULL, clusters = NULL, design, analysis, 
                                bootstrap_data = FALSE, N_bootstrap, sims = 5, label = NULL){
  
  if(is.null(sample_frame) & is.null(data))
    stop("Please provide either a sample_frame argument or a data argument.")
  if(!is.null(sample_frame) & is.null(potential_outcomes))
    stop("If you provide a sample_frame argument, please also provide a potential_outcomes argument.")
  if(!is.null(data) & !is.null(sample_frame) & !is.null(potential_outcomes))
    warning("You provided arguments for sample_frame, potential_outcomes, and data. The function will resample the covariates from the sample_frame argument, resample the potential_outcomes from the potential_outcomes argument, and ignore the data argument. To avoid this warning, please do not specify the data argument.")
  
  if(!is.null(data) & is.null(sample_frame) & !is.null(potential_outcomes))
    resample <- "potential_outcomes"
  if(!is.null(sample_frame) & !is.null(potential_outcomes))
    resample <- "both"
  if(is.null(sample_frame) & is.null(potential_outcomes) & !is.null(data))
    resample <- "neither"
  
  if(bootstrap_data == TRUE & is.null(data))
    stop("Please provide a data frame in the data argument to enable data bootstrapping, 
         or set bootstrap_data to FALSE.")
  
  if(!is.null(data))
    data_sim <- data
  
  estimates_list <- list()
  estimands_list <- list()
  for(i in 1:sims){
    
    if(resample == "potential_outcomes"){
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample_frame = data, 
                            blocks = blocks, clusters = clusters)
      if(resample_data == TRUE){
        ## for now N_resample is preset, but could be set according to a design object
        N_bootstrap <- nrow(data_sim) 
        data_sim <- data_sim[sample(1:nrow(data_sim), N_bootstrap, replace = TRUE)]
      }
    } else if (resample == "both"){
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample_frame = sample_frame, blocks = blocks, clusters = clusters)
    } else if (resample == "neither"){
      stop("Neither is not yet implemented. Please fix make_data.")
    }
    
    z_sim <- assign_treatment(design = design, data = data_sim)
    
    if(class(analysis)=="analysis"){analysis <- list(analysis)}
    
    for(j in 1:length(analysis)){
      data_sim[, analysis_treatment_variable(analysis = analysis[[j]])] <- z_sim
      
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
  
  return_object <- list(estimates = estimates_list, estimands = estimands_list, label = label)
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
    PATE <- mean(summary_array[,1,])
    simulation_error <- sd(summary_array[,1,])
    power <- mean(summary_array[,4,] < 0.05)
    RMSE <- sqrt(mean((summary_array[,1,] - summary_array[,2,])^2))
    bias <- mean(summary_array[,1,] - PATE)
    
    summ <- cbind(PATE, simulation_error, power, RMSE, bias)
    
    #structure(summ, class = c("summary.experiment_simulations", class(summ)))
    return(summ)
  }
  
  PATE <- apply(summary_array[,1,], 1 , mean)
  simulation_error <- apply(summary_array[,1,], 1 , sd)
  power <- apply(summary_array[,4,], 1 , function(x) mean(x < 0.05))
  RMSE <- apply(summary_array[,1,] - summary_array[,2,], 1 , function(x) sqrt(mean(x^2)))
  bias <- apply(summary_array[,2,] - PATE, 1, mean)
  
  summ <- cbind(PATE, simulation_error, power, RMSE, bias)
  
  #structure(summ, class = c("summary.experiment_simulations", class(summ)))
  return(summ)
}

#' @export
print.summary.experiment_simulations <- function(x, ...){
  ## prints paragraph describing design
}


#' @export
print.experiment_simulations <- function(x, ...){
  print(summary.experiment_simulations(x, ... = ...))
  return()
}

reorient <- function(x) {
  obj <- c(x)
  names(obj) <- rep(paste(rownames(x), colnames(x), sep = "_"), each = ncol(x))
  return(obj)
}

