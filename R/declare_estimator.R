#' Estimator declaration
#' 
#' @param formula An formula used to create estimates.
#' @param model A function, such as \code{lm} or \code{glm}, used to estimate a model, from which estimates are extracted.
#' @param model_options A list of options for the \code{model}, such as \code{family} for \code{glm}.
#' @param estimates A function to calculate estimates or to extract estimates from the output of the \code{model} function.
#' @param ... Include other options that are used for the \code{estimates} function.
#' @param subset A string indicating which subset of the data to calculate estimates on, such as "province == '1'".
#' @param weights_variable_name The variable name of a weights variable used by the \code{estimates} function and/or the \code{model} function.
#' @param labels Labels for each of the estimate(s).
#' @param description A description of the estimator in words.
#' @param estimand An estimand object created by \code{declare_estimand}.
#'
#' @export
declare_estimator <- function(formula = NULL, model = NULL, model_options = NULL, estimates, ...,
                              subset = NULL, weights_variable_name = NULL, labels = NULL, description = NULL, estimand = NULL) {
  
  estimates_options <- list(...)
  
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
  
  return_object <- list(model = model_function, model_name = substitute(model), estimates = estimates_function, estimates_name = substitute(estimates),
                        labels = labels, description = description, estimand = estimand, call = match.call())
  
  if(is.null(model)){
    return_object$model <- NULL
    return_object$model_name <- NULL
  }
  
  structure(return_object, class = "estimator")
  
}

#' Fit model 
#' 
#' @param estimator An estimator object created by \code{declare_estimator}.
#' @param data A data.frame object, used to fit the model.
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
#' @param estimator An estimator object created by \code{declare_estimator}.
#' @param data A data.frame object, used to fit the model.
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
    if(!any(colnames(estimates_list[[i]]) %in% "estimate_label")) {
      if(length(estimator_labels[[i]]) == 1){
        estimates_list[[i]] <- data.frame(estimates_list[[i]], estimate_label = estimator_labels[[i]])    
      } else {
        stop("Please provide a single label or provide a column in the estimates named 'estimate_label'.")
      }
    }
  }
  
  estimates_matrix <- do.call(rbind, estimates_list)
  
  if(!all(c("estimate_label", "statistic_label", "statistic") %in% colnames(estimates_matrix))){
    stop("Please include the columns estimate_label, statistics_label, and statistic in the output of your estimates function.")
  }
  
  return(estimates_matrix)
}
