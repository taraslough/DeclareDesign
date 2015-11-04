#' Declare Estimand
#' 
#' This function creates an estimand object. 
#'
#' @param estimand A function, possibly of data, that returns a (possibly single-valued) vector of quantities to be estimated.  Users may want to use our built-in estimand functions such as \code{\link{declare_ATE}}
#' @param text_estimand A character string that contains an expression that can be evaluated on the data.  For example, you can provide "mean(Y_Z1 - Y_Z0)" to set the estimand as the average difference between the Y_Z1 potential outcome and the Y_Z0 potential outcome.
#' @param target Either "population" or "sample".  Defaults to "population".
#' @param subset A character string that contains an expression that can be passed to the subset operator.  For example subset = "income > 50".
#' @param weights_variable The name of the weighting variable (optional).
#' @param label A character string for the estimand's label.
#' @param ... 
#'
#' @export
declare_estimand <- function(estimand_function, target = "population",
                             potential_outcomes, condition_names = NULL,
                             subset = NULL, weights_variable = NULL, 
                             label = NULL, ...) {
  
  if(!is.null(weights_variable)){
    stop("Weighted estimands are not yet implemented. Please contact the authors if you are interested in using them.")
  }
  
  if(is.null(potential_outcomes)){
    stop("Please provide a potential_outcomes object. This is used to create the potential outcomes before calculating the estimand.")
  }
  
  estimand_options <- list(...)
  
  ## if a custom estimand is provided
  
  estimand_function <- function(data){
    argument_names <- names(formals(estimand))
    if(!is.null(subset) & "subset" %in% argument_names)
      estimand_options$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable) & "weights" %in% argument_names)
      estimand_options$weights <- data[, weights_variable]
    estimand_options$data <- data
    
    return(do.call(estimand, args = estimand_options))
  }
  
  structure(list(estimand = estimand_function, target = target, potential_outcomes = potential_outcomes, condition_names = condition_names, 
                 label = label, call = match.call()), class = "estimand")
  
}

#' @export
get_estimands <- function(estimand = NULL, estimator = NULL, sample_data = NULL, population_data = NULL){
  
  if(!is.null(estimand) & !is.null(estimator)){
    stop("Please either send estimand(s) or estimator(s) only to get_estimands().")
  }
  
  if(!is.null(estimator) & class(estimator) == "list"){
    estimand <- lapply(1:length(estimator), function(j) estimator[[j]]$estimand)
  } else if(!is.null(estimator) & class(estimator) == "estimator"){
    estimand <- estimator$estimand
  }
  
  if(class(estimand) == "list"){
    estimand_labels <- c(lapply(1:length(estimand), function(j) ifelse(is.null(estimand[[j]]$label), "", estimand[[j]]$label)), recursive = TRUE)
    estimand_labels[which(estimand_labels == "")] <- paste(substitute(estimand)[-1L])[which(estimand_labels == "")]
  } else {
    estimand_labels <- estimand$label
    if(is.null(estimand_labels)){
      estimand_labels <- paste(substitute(estimand))
    }
  }
  
  if(class(estimand) == "estimand"){
    estimand <- list(estimand)
  }
  
  estimands_list <- list()
  if(!is.null(estimand)){
    for(i in 1:length(estimand)){
      if(!is.null(estimand[[i]])){
        ## if there is an estimand defined
        if(estimand[[i]]$target == "population"){
          if(is.null(population_data)){
            stop(paste0("The target of ", estimand_labels[i], " is the population, so please send get_estimands a population data frame, i.e. one created by draw_population()."))
          }
          estimands_list[[i]] <- estimand[[i]]$estimand(data = draw_potential_outcomes(data = population_data, potential_outcomes = estimand[[i]]$potential_outcomes, condition_names = estimand[[i]]$condition_names))
        } else if(estimand[[i]]$target == "sample") {
          if(is.null(sample_data)){
            stop(paste0("The target of ", estimand_labels[i], " is the sample, so please send get_estimands a sample data frame, i.e. one created by draw_sample()."))
          }
          estimands_list[[i]] <- estimand[[i]]$estimand(data = draw_potential_outcomes(data = sample_data, potential_outcomes = estimand[[i]]$potential_outcomes, condition_names = estimand[[i]]$condition_names))
        }
        names(estimands_list[[i]]) <- estimand_labels[i]
      } else {
        ## if there is NOT an estimand defined
        estimands_list[[i]] <- NA
      }
    }
    estimands_vector <- c(estimands_list, recursive = T)
    
  } else {
    ## if estimand is null (i.e. an estimator did not have an estimand)
    
    estimands_vector <- NA
  }

  return(estimands_vector)
  
}

