#' Estimator declaration
#' 
#' @param formula An object of class "formula".
#' @param model 
#' @param model_options 
#' @param estimates 
#' @param estimates_options 
#' @param subset 
#' @param weights_variable_name 
#' @param labels 
#' @param estimand 
#'
#' @export
declare_estimator <- function(formula = NULL, model = NULL, model_options = NULL, estimates, estimates_options = NULL,
                              subset = NULL, weights_variable_name = NULL, labels = NULL, description = NULL, estimand = NULL) {
  
  # Checks -------------------------------------------------
  estimand <- clean_inputs(estimand, "estimand", accepts_list = FALSE)
  
  if(missing(estimates)){
    stop("Please provide an estimates function. If you provided a model function, the estimates function should extract the quantity of interest (for example, the coefficient associated with the treatment variable). If you did not, the estimates function should take the data and return the quantity of interest directly.")
  }
  
  if(is.null(estimates) | (class(estimates) != "function")){
    stop("Please provide a function in the estimates argument.")
  }
  
  if(substitute(estimates) == "difference_in_means" & (length(all.vars(formula)) > 2))
    stop("When using the difference_in_means method, there should only be one covariate listed in the formula on the right-hand side: the treatment variable.")
  
  model_function <- function(data){
    argument_names <- names(formals(model))
    options_internal <- list()
    if(!is.null(formula) & "formula" %in% argument_names)
      options_internal$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      options_internal$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable_name) & "weights" %in% argument_names)
      options_internal$weights <- data[, weights_variable_name]
    if(length(model_options) > 0){
      for(i in 1:length(model_options)){
        if(names(model_options)[[i]] %in% argument_names){
          options_internal[[names(model_options)[[i]]]] <- model_options[[i]]
        }
      }
    }
    options_internal$data <- data
    
    return(do.call(model, args = options_internal))
  }
  
  estimates_function <- function(model = NULL, data = NULL){
    argument_names <- names(formals(estimates))
    options_internal <- list()
    if(!is.null(formula) & "formula" %in% argument_names)
      options_internal$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      options_internal$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable_name) & "weights" %in% argument_names)
      options_internal$weights <- data[, weights_variable_name]
    if(!is.null(labels) & "labels" %in% argument_names)
      options_internal$labels <- labels
    if(!is.null(data) & "data" %in% argument_names)
      options_internal$data <- data
    if(!is.null(model) & "model" %in% argument_names)
      options_internal$model <- model
    if(length(estimates_options) > 0){
      for(i in 1:length(estimates_options)){
        if(names(estimates_options)[[i]] %in% argument_names){
          options_internal[[names(estimates_options)[[i]]]] <- estimates_options[[i]]
        }
      }
    }
    
    return(do.call(estimates, args = options_internal))
  }
  
  if(is.null(estimand$labels) & !is.null(estimand)){
    estimand$labels <- as.character(substitute(estimand))
  }
  
  return_object <- list(model = model_function, estimates = estimates_function, 
                        labels = labels, description = description, estimand = estimand, call = match.call())
  
  if(is.null(model)){
    return_object$model <- NULL
  }
  
  structure(return_object, class = "estimator")
  
}

#' Fit model 
#' 
#' @param estimator 
#' @param data 
#'
#' @export
fit_model <- function(data, estimator){
  
  # Checks -------------------------------------------------
  estimator <- clean_inputs(estimator, "estimator", accepts_list = TRUE)
  
  model_list <- list()
  for(i in 1:length(estimator)){
    if(!is.null(estimator[[i]]$model)){
      model_list[[i]] <- estimator[[i]]$model(data = data)
    } else {
      model_list[[i]] <- NULL
    }
  }
  
  ## just send back the model fit, not a list, if there is a single estimator
  if(length(estimator) == 1){
    model_list <- model_list[[1]]
  }
  
  return(model_list)
}

#' Get estimates
#' 
#' @param estimator 
#' @param data 
#'
#' @export
get_estimates <- function(estimator, data) {
  
  if(class(estimator) == "list"){
    estimator_labels <- lapply(1:length(estimator), function(j) estimator[[j]]$labels)
    if(any(unlist(lapply(estimator_labels, is.null)))){
      estimator_object_labels <- paste(substitute(estimator)[-1L])
      estimator_labels <- lapply(1:length(estimator), function(j) {
        labels <- estimator[[j]]$labels
        if(is.null(labels)){
          labels <- estimator_object_labels[j]
        }
        return(labels)})
    }
  } else {
    if(!is.null(estimator$labels)){
      estimator_labels <- list(estimator$labels)
    } else {
      estimator_labels <- list(paste(substitute(estimator)))
    }
  }
  
  # Checks -------------------------------------------------
  estimator <- clean_inputs(estimator, "estimator", accepts_list = TRUE)
  
  ## if the user sends no qoi function but does send a list of estimator objects,
  ## run this function on each estimator object and cbind the results
  estimates_list <- list()
  for(i in 1:length(estimator)) {
    if(!is.null(estimator[[i]]$model)){
      estimates_list[[i]] <- estimator[[i]]$estimates(model = fit_model(estimator = estimator[[i]], data = data), data = data)
    } else {
      estimates_list[[i]] <- estimator[[i]]$estimates(data = data)
    }
    if(class(estimates_list[[i]]) == "numeric" & !is.null(names(estimates_list[[i]]))){
      estimates_list[[i]] <- as.matrix(estimates_list[[i]])
    }
    if(class(estimates_list[[i]]) != "matrix" & class(estimates_list[[i]]) != "data.frame"){
      stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for estimator named", estimator_labels[i], 
                 "did not produce a matrix or data frame of results."))
    }
    if(ncol(estimates_list[[i]]) != length(estimator_labels[[i]]) & length(estimator_labels[[i]]) == 1)
      estimator_labels[[i]] <- rep(estimator_labels[[i]], ncol(estimates_list[[i]]))
    colnames(estimates_list[[i]]) <- estimator_labels[[i]] ##colnames(estimates_list[[i]]), estimator_labels[i], sep = "_")
  }
  
  ## this merges the summary statistics together such that there can be different statistics for each estimator
  ## and they are merged and named correctly
  estimates_matrix <- estimates_list[[1]]
  if(length(estimator) > 1){
    for(i in 2:length(estimator)){
      estimates_matrix <- merge(estimates_matrix, estimates_list[[i]], by = "row.names", all.x = TRUE, all.y = TRUE, sort = FALSE)
      rownames(estimates_matrix) <- estimates_matrix[,1]
      estimates_matrix <- estimates_matrix[, 2:ncol(estimates_matrix), drop = FALSE]
    }
  }
  return(estimates_matrix)
}
