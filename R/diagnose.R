#' Declare the data-generating process of a variable
#'
#' @param data An optional user provided dataframe (not made with \code{\link{make_data}})
#' @param potential_outcomes A potential outcomes object, made with \code{\link{declare_potential_outcomes}}
#' @param sample A sample frame object, made with \code{\link{declare_sample}}
#' @param blocks A blocks object, made with \code{\link{declare_blocks}} (optional).
#' @param clusters A clusters object, made with \code{\link{declare_clusters}} (optional).
#' @param design design object, made with \code{\link{declare_design}}.
#' @param analysis analysis object, made with \code{\link{declare_analysis}}, or a list of such objects.
#' @param bootstrap_data Logical
#' @param N_bootstrap Number of bootstrap sims to conduct
#' @param sims number of iterations
#' @param label label for the simulation
#' @param analysis_labels labels for each analysis
#' @export
diagnose <- function(data = NULL, potential_outcomes = NULL, sample = NULL, 
                     blocks = NULL, clusters = NULL, design, analysis, 
                     bootstrap_data = FALSE, N_bootstrap, sims = 5, label = NULL,
                     analysis_labels = NULL){
  
  if(is.null(blocks))
    blocks <- design$blocks
  if(is.null(clusters))
    clusters <- design$clusters
  
  if(is.null(sample) & is.null(data))
    stop("Please provide either a sample argument or a data argument.")
  if(!is.null(sample) & is.null(potential_outcomes))
    stop("If you provide a sample argument, please also provide a potential_outcomes argument.")
  if(!is.null(data) & !is.null(sample) & !is.null(potential_outcomes))
    warning("You provided arguments for sample, potential_outcomes, and data. The function will resample the covariates from the sample argument, resample the potential_outcomes from the potential_outcomes argument, and ignore the data argument. To avoid this warning, please do not specify the data argument.")
  
  if(!is.null(data) & is.null(sample) & !is.null(potential_outcomes))
    resample <- "potential_outcomes"
  if(!is.null(sample) & !is.null(potential_outcomes))
    resample <- "both"
  if(is.null(sample) & is.null(potential_outcomes) & !is.null(data))
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
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample = data, design = design)
      if(bootstrap_data == TRUE){
        ## for now N_resample is preset, but could be set according to a design object
        N_bootstrap <- nrow(data_sim) 
        data_sim <- data_sim[sample(1:nrow(data_sim), N_bootstrap, replace = TRUE)]
      }
    } else if (resample == "both"){
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample = sample, design = design)
    } else if (resample == "neither"){
      stop("Neither is not yet implemented. Please fix make_data.")
    }
    
    z_sim <- assign_treatment(design = design, data = data_sim)
    
    if(is.null(analysis_labels)){
      if(class(analysis) == "list")
        analysis_labels <- paste(substitute(analysis)[-1L])
      else
        analysis_labels <- paste(substitute(analysis))
    }
    
    if(class(analysis)=="analysis"){analysis <- list(analysis)}
    
    for(j in 1:length(analysis)){
      data_sim[, analysis_treatment_variable(analysis = analysis[[j]])] <- z_sim
      
      data_sim[, analysis_outcome_variable(analysis = analysis[[j]])] <- 
        observed_outcome(outcome = analysis_outcome_variable(analysis[[j]]),  
                         treatment_assignment = analysis_treatment_variable(analysis[[j]]),
                         data = data_sim)
    }
    
    estimates_list[[i]] <- get_estimates(analysis, data = data_sim, analysis_labels = analysis_labels)
    estimands_list[[i]] <- get_estimands(analysis, data = data_sim, analysis_labels = analysis_labels)
    
  }
  
  return_object <- list(estimates = estimates_list, estimands = estimands_list, label = label)
  class(return_object) <- "experiment_simulations"
  
  return(return_object)
  
}

#' @export
diagnose_parallel <- function(data = NULL, potential_outcomes = NULL, sample = NULL, 
                              blocks = NULL, clusters = NULL, design, analysis, 
                              bootstrap_data = FALSE, N_bootstrap = NULL, sims = 5, label = NULL,
                              analysis_labels = NULL, procs = 2){
  
  require(parallel)
  
  if(is.null(blocks))
    blocks <- design$blocks
  if(is.null(clusters))
    clusters <- design$clusters
  
  if(is.null(sample) & is.null(data))
    stop("Please provide either a sample argument or a data argument.")
  if(!is.null(sample) & is.null(potential_outcomes))
    stop("If you provide a sample argument, please also provide a potential_outcomes argument.")
  if(!is.null(data) & !is.null(sample) & !is.null(potential_outcomes))
    warning("You provided arguments for sample, potential_outcomes, and data. The function will resample the covariates from the sample argument, resample the potential_outcomes from the potential_outcomes argument, and ignore the data argument. To avoid this warning, please do not specify the data argument.")
  
  if(!is.null(data) & is.null(sample) & !is.null(potential_outcomes))
    resample <- "potential_outcomes"
  if(!is.null(sample) & !is.null(potential_outcomes))
    resample <- "both"
  if(is.null(sample) & is.null(potential_outcomes) & !is.null(data))
    resample <- "neither"
  
  if(bootstrap_data == TRUE & is.null(data))
    stop("Please provide a data frame in the data argument to enable data bootstrapping, 
         or set bootstrap_data to FALSE.")
  
  if(!is.null(data))
    data_sim <- data
  
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
  
  simulations_list <- foreach(i = 1:sims, .combine = 'comb', .packages = "experimentr", .multicombine = TRUE, .init = list(list(), list())) %dopar% {
    
    if(resample == "potential_outcomes"){
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample = data, design = design)
      if(bootstrap_data == TRUE){
        ## for now N_resample is preset, but could be set according to a design object
        N_bootstrap <- nrow(data_sim) 
        data_sim <- data_sim[sample(1:nrow(data_sim), N_bootstrap, replace = TRUE)]
      }
    } else if (resample == "both"){
      data_sim <- make_data(potential_outcomes = potential_outcomes, sample = sample, design = design)
    } else if (resample == "neither"){
      stop("Neither is not yet implemented. Please fix make_data.")
    }
    
    data_sim <- make_data(potential_outcomes = potential_outcomes, sample = sample, design = design)
    
    z_sim <- assign_treatment(design = design, data = data_sim)
    
    for(j in 1:length(analysis)){
      data_sim[, analysis_treatment_variable(analysis = analysis[[j]])] <- z_sim
      
      data_sim[, analysis_outcome_variable(analysis = analysis[[j]])] <- 
        observed_outcome(outcome = analysis_outcome_variable(analysis[[j]]),  
                         treatment_assignment = analysis_treatment_variable(analysis[[j]]),
                         data = data_sim)
    }
    
    ##return(list(1, 2))
    return(list(get_estimates(analysis, data = data_sim, analysis_labels = analysis_labels),
                get_estimands(analysis, data = data_sim, analysis_labels = analysis_labels)))
    
  }
  
  
  estimates_list <- simulations_list[[1]]
  estimands_list <- simulations_list[[2]]
  
  return_object <- list(estimates = estimates_list, estimands = estimands_list, label = label)
  class(return_object) <- "experiment_simulations"
  
  return(return_object)
  
}

#' @export
summary.experiment_simulations <- function(object, ...) {
  
  estimates <- object$estimates
  estimands <- object$estimands
  
  summary_array <- array(NA, dim = c(ncol(estimates[[1]]), 5, length(estimates)),
                         dimnames = list(colnames(estimates[[1]]),
                                         c("sate", "sate_hat", "error", "p", "ci_covers_est"), 
                                         1:length(estimates)))
  for(i in 1:length(object$estimates)){
    
    for(q in 1:ncol(estimates[[i]])){
      
      sate <- estimands[[i]]["est", q]
      sate_hat <- estimates[[i]]["est", q]
      error <- sate_hat - sate
      p <- estimates[[i]]["p", q]
      ci_covers_est <- estimands[[i]]["est", q] <= estimates[[i]]["ci_upper", q] & 
        estimands[[i]]["est", q] >= estimates[[i]]["ci_lower", q]
      
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
print.summary.experiment_simulations <- function(x, ...){
  print(summary.experiment_simulations(x, ... = ...))
  return()
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

