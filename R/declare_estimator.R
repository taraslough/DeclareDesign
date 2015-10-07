#' @export
declare_estimator <- function(formula = NULL, model = NULL, estimates, estimates_options = NULL,
                              subset = NULL, weights_variable = NULL, label = NULL, estimates_labels = NULL, 
                              estimand = NULL, ...) {
  
  if(missing(estimates)){
    stop("Please provide an estimates function. If you provided a model function, the estimates function should extract the quantity of interest (for example, the coefficient associated with the treatment variable). If you did not, the estimates function should take the data and return the quantity of interest directly.")
  }
  
  estimator_options <- list(...)
  
  ##outcome_variable <- all.vars(formula[[2]])
  
  ##if(length(all.vars(formula)) == 2 & substitute(estimates) == "difference_in_means")
  ##  treatment_variable <- all.vars(formula[[3]])
  
  arguments <- mget(names(formals()), sys.frame(sys.nframe()))
  arguments$... <- NULL
  if(length(estimator_options) > 0) {
    for(k in 1:length(estimator_options))
      arguments[[names(estimator_options)[[k]]]] <- estimator_options[[k]]
  }
  
  if(is.null(estimates) | !(class(estimates) == "function")){
    stop("Please provide a function in the estimates argument.")
  }
  
  if(substitute(estimates) == "difference_in_means" & (length(all.vars(formula)) > 2))
    stop("When using the difference_in_means method, there should only be one covariate listed in the formula on the right-hand side: the treatment variable.")
  
  model_function <- function(data){
    argument_names <- names(formals(model))
    if(!is.null(formula) & "formula" %in% argument_names)
      estimator_options$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      estimator_options$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable) & "weights" %in% argument_names)
      estimator_options$weights <- data[, weights_variable]
    estimator_options$data <- data
    
    return(do.call(model, args = estimator_options))
  }
  
  estimates_function <- function(model = NULL, data = NULL){
    argument_names <- names(formals(estimates))
    if(!is.null(formula) & "formula" %in% argument_names)
      estimates_options$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      estimates_options$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable) & "weights" %in% argument_names)
      estimates_options$weights <- data[, weights_variable]
    if(!is.null(estimates_labels) & "estimates_labels" %in% argument_names)
      estimates_options$estimates_labels <- estimates_labels
    if(!is.null(data) & "data" %in% argument_names)
      estimates_options$data <- data
    if(!is.null(model) & "model" %in% argument_names)
      estimates_options$model <- model
    
    return(do.call(estimates, args = estimates_options))
  }
  
  if(is.null(estimand$label) & !is.null(estimand)){
    estimand$label <- as.character(substitute(estimand))
  }
  
  return_object <- list(model = model_function, estimates = estimates_function, 
                        label = label, estimates_labels = estimates_labels,
                        estimand = estimand, arguments = arguments, call = match.call())
  
  if(is.null(model)){
    return_object$model <- NULL
  }
  
  structure(return_object, class = "estimator")
  
}

#' @export
get_estimates_model <- function(estimator, data){
  if(class(estimator) != "estimator")
    stop("The estimator argument must be an object created by the declare_estimator function")
  if(is.null(estimator$model))
    stop("This analysis function does not have a model associated with it. Try get_estimates to obtain the estimates instead.")
  
  if(!class(estimator) == "list"){ 
    estimator <- list(estimator)
  }
  
  model_list <- list()
  for(i in 1:length(estimator)){
    model_list[[i]] <- estimator[[1]]$model(data = data)
  }
  
  ## just send back the model fit, not a list, if there is a single estimator
  if(length(estimator) == 1){
    model_list <- model_list[[1]]
  }
  
  return(model_list)
}

#' @export
get_estimates <- function(estimator, data) {
  
  if(class(estimator) == "list"){
    estimator_labels <- c(lapply(1:length(estimator), function(j) ifelse(is.null(estimator[[j]]$label), "", estimator[[j]]$label)), recursive = TRUE)
    estimator_labels[which(estimator_labels == "")] <- paste(substitute(estimator)[-1L])[which(estimator_labels == "")]
  } else {
    estimator_labels <- estimator$label
    if(is.null(estimator_labels)){
      estimator_labels <- paste(substitute(estimator))
    }
  }
  
  if(!class(estimator) == "list"){ 
    estimator <- list(estimator)
  }
  
  ## if the user sends no qoi function but does send a list of estimator objects,
  ## run this function on each estimator object and cbind the results
  estimates_list <- list()
  for(i in 1:length(estimator)) {
    if(!is.null(estimator[[i]]$model)){
      estimates_list[[i]] <- estimator[[i]]$estimates(get_estimates_model(estimator = estimator[[i]], data = data))
    } else {
      estimates_list[[i]] <- estimator[[i]]$estimates(data = data)
    }
    if(class(estimates_list[[i]]) != "matrix" & class(estimates_list[[i]]) != "data.frame")
      stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for estimator named", estimator_labels[i], 
                 "did not produce a matrix or data frame of results."))
    colnames(estimates_list[[i]]) <- paste(colnames(estimates_list[[i]]), estimator_labels[i], sep = "_")
  }
  
  ## this merges the summary statistics together such that there can be different statistics for each estimator
  ## and they are merged and named correctly
  estimates_matrix <- estimates_list[[1]]
  if(length(estimator) > 1){
    for(i in 2:length(estimator)){
      estimates_matrix <- merge(estimates_matrix, estimates_list[[i]], by = "row.names", all.x = T, all.y = T)
      rownames(estimates_matrix) <- estimates_matrix[,1]
      estimates_matrix <- estimates_matrix[, 2:ncol(estimates_matrix), drop = F]
    }
  }
  return(estimates_matrix)
}

