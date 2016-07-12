#' Estimator declaration
#' 
#' @param formula An formula used to create estimates.
#' @param model A function, such as \code{lm} or \code{glm}, used to estimate a model, from which estimates are extracted.
#' @param model_options A list of options for the \code{model}, such as \code{family} for \code{glm}.
#' @param estimates A function to calculate estimates or to extract estimates from the output of the \code{model} function.
#' @param ... Include other options that are used for the \code{estimates} function.
#' @param subset A string indicating which subset of the data to calculate estimates on, such as "province == '1'".
#' @param weights_variable_name The variable name of a weights variable used by the \code{estimates} function and/or the \code{model} function.
#' @param label the label for the estimator.
#' @param description A description of the estimator in words.
#' @param estimand An estimand object created by \code{declare_estimand}.
#'
#' @export
declare_estimator <- function(formula = NULL, model = NULL, model_options = NULL, estimates, ...,
                              subset = NULL, weights_variable_name = NULL, 
                              label = NULL, description = NULL, estimand = NULL) {
  
  estimates_options <- list(...)
  
  # Checks -------------------------------------------------
  
  
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
    if(!is.null(label) & "label" %in% argument_names)
      options_internal$label <- label
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
  
  if(class(estimand) == "list"){
    estimand_labels <- lapply(1:length(estimand), function(j) estimand[[j]]$label)
    if(any(unlist(lapply(estimand_labels, is.null)))){
      estimand_object_labels <- paste(substitute(estimand)[-1L])
      estimand_labels <- lapply(1:length(estimand), function(j) {
        label <- estimand[[j]]$label
        if(is.null(label)){
          label <- estimand_object_labels[j]
        }
        return(label)})
    }
  } else {
    if(!is.null(estimand$label)){
      estimand_labels <- list(estimand$label)
    } else {
      estimand_labels <- list(paste(substitute(estimand)))
    }
  }
  
  estimand_label <- unlist(estimand_labels)
  
  estimand <- clean_inputs(estimand, "estimand", accepts_list = TRUE)
  
  if(length(estimand) > 0){
    for(i in 1:length(estimand)){
      estimand[[i]]$label <- estimand_labels[[i]]
    }
  }
  
  estimand_level <- sapply(estimand, function(x) x$estimand_level)
  
  return_object <- list(model = model_function, model_name = substitute(model), 
                        estimates = estimates_function, estimates_name = substitute(estimates),
                        label = label, description = description, estimand = estimand, 
                        estimand_label = estimand_label, estimand_level = estimand_level,
                        call = match.call())
  
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
  
  # get the labels
  if(class(estimator) == "list"){
    estimator_labels <- lapply(1:length(estimator), function(j) estimator[[j]]$label)
    estimand_labels <- lapply(1:length(estimator), function(j) estimator[[j]]$estimand_label)
    estimand_levels <- lapply(1:length(estimator), function(j) estimator[[j]]$estimand_level)
    if(any(unlist(lapply(estimator_labels, is.null)))){
      estimator_object_labels <- paste(substitute(estimator)[-1L])
      estimator_labels <- lapply(1:length(estimator), function(j) {
        label <- estimator[[j]]$label
        if(is.null(label)){
          label <- estimator_object_labels[j]
        }
        return(label)})
    }
  } else {
    if(!is.null(estimator$label)){
      estimator_labels <- list(estimator$label)
    } else {
      estimator_labels <- list(paste(substitute(estimator)))
    }
    estimand_labels <- list(estimator$estimand_label)
    estimand_levels <- list(estimator$estimand_level)
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
    if(class(estimates_list[[i]]) != "data.frame"){
      estimates_list[[i]] <- clean_estimates(estimates_list[[i]])
    }
    
    estimates_list[[i]]$estimator_label <- estimator_labels[[i]]
    if(!("estimate_label" %in% names(estimates_list[[i]]))){
      estimates_list[[i]]$estimate_label <- estimator_labels[[i]]
    }
    
    if(length(estimand_labels[[i]]) == nrow(estimates_list[[i]])){
      estimates_list[[i]]$estimand_label <- estimand_labels[[i]]
      estimates_list[[i]]$estimand_level <- c(estimand_levels[[i]], recursive = TRUE)
    } else if (length(estimand_labels[[i]]) == 0){
      estimates_list[[i]]$estimand_label <- "no estimand"
      estimates_list[[i]]$estimand_level <- "no estimand"
    } else if ((length(estimand_labels[[i]]) > 1) & (nrow(estimates_list[[i]]) == 1)){
      estimates_list[[i]] <- estimates_list[[i]][rep(1, length(estimand_labels[[i]])), ]
      estimates_list[[i]]$estimand_label <- c(estimand_labels[[i]], recursive = TRUE)
      estimates_list[[i]]$estimand_level <- c(estimand_levels[[i]], recursive = TRUE)
      rownames(estimates_list[[i]]) <- NULL
    } else if ((length(estimand_labels[[i]]) < nrow(estimates_list[[i]]) & length(estimand_labels[[i]]) != 0)){
      stop("Please provide at least one estimand for each estimate.")
    }
    
  }
  
  estimates_df <- estimates_list[[1]]
  if(length(estimates_list) > 1){
    for(j in 2:length(estimates_list)){
      estimates_df <- safe_rbind(estimates_df, estimates_list[[j]])
    }
  }
  
  ##estimates_df <- do.call(safe_rbind, estimates_list)
  
  
  return(estimates_df)
}

clean_estimates <- function(estimates){
  
  if(is.vector(estimates)){
    if(is.null(names(estimates)) == FALSE){
      estimates <- data.frame(t(estimates), row.names = NULL, stringsAsFactors = FALSE)
    } else {
      if(length(estimates) == 1){
        estimates <- data.frame(est = estimates, row.names = NULL, stringsAsFactors = FALSE)
      }
    }
  } else if(is.matrix(estimates)){
    estimates <- data.frame(estimates, stringsAsFactors = FALSE)
  } 

  return(estimates)
}

safe_rbind <- function(df1, df2){
  
  common_names <- intersect(names(df1), names(df2))
  
  df1_only_names <- names(df1)[!names(df1) %in% common_names]
  df2_only_names <- names(df2)[!names(df2) %in% common_names]
  
  df <- rbind(df1[, sort(common_names)], df2[, sort(common_names)])
  
  if(length(df1_only_names) > 0){
    df <- merge(df, df1[, c("estimator_label", "estimate_label", "estimand_label", df1_only_names)],
                by = c("estimator_label", "estimate_label", "estimand_label"), all = T)
  }
  if(length(df2_only_names) > 0){
    df <- merge(df, df2[, c("estimator_label", "estimate_label", "estimand_label", df2_only_names)], 
                by = c("estimator_label", "estimate_label", "estimand_label"), all = T)
  }
  
  return(df)
  
}

