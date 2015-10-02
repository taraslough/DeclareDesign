#' @export
declare_estimator <- function(formula, model = NULL, calculate_estimates = difference_in_means, 
                              subset = NULL, weights_variable = NULL, labels = NULL, 
                              estimator = estimand, ...) {
  
  arguments <- mget(names(formals()), sys.frame(sys.nframe()))
  arguments$... <- NULL
  if(length(estimator_options) > 0) {
    for(k in 1:length(estimator_options))
      arguments[[names(estimator_options)[[k]]]] <- estimator_options[[k]]
  }
  
  if(substitute(calculate_estimates) == "difference_in_means" & (length(all.vars(formula)) > 2))
    stop("When using the difference_in_means method, there should only be one covariate listed in the formula on the right-hand side: the treatment variable.")
  
  estimate_function <- function(data){
    argument_names <- names(formals(estimator))
    if(!is.null(formula) & "formula" %in% argument_names)
      estimator_options$formula <- stats::formula(unclass(formula))
    if(!is.null(subset) & "subset" %in% argument_names)
      estimator_options$subset <- with(data, eval(parse(text = subset)))
    if(!is.null(weights_variable) & "weights" %in% argument_names)
      estimator_options$weights <- data[, weights_variable]
    estimator_options$data <- data
    
    return(do.call(estimator, args = estimator_options))
  }
  
  if(!is.null(quantity_of_interest))
    environment(quantity_of_interest) <- environment()
  
  if(!is.null(estimand_quantity_of_interest))
    environment(estimand_quantity_of_interest) <- environment()
  
  return_object <- list(estimate = estimate_function, estimand = estimand_function, 
                        quantity_of_interest = quantity_of_interest, 
                        estimand_formula = estimand_formula, estimand_options = estimand_options,
                        estimand_quantity_of_interest = estimand_quantity_of_interest,
                        treatment_variable = treatment_variable,
                        outcome_variable = outcome_variable, arguments = arguments,
                        call = match.call())
  
  if(is.null(quantity_of_interest))
    return_object$quantity_of_interest <- NULL
  
  if(is.null(estimand_quantity_of_interest))
    return_object$estimand_quantity_of_interest <- NULL  
  
  structure(return_object, class = "estimator")
  
}

#' @param analysis what is it?
#' @param data what is it?
#' @rdname declare_analysis
#' @export
get_estimates_model <- function(analysis, data){
  if(class(analysis) != "analysis")
    stop("The analysis argument must be an object created by the declare_analysis function")
  if(is.null(analysis$estimate))
    stop("This analysis function does not have a model associated with it. Try get_estimates to obtain the quantities of interest.")
  return(analysis$estimate(data = data))
}


#' @param analysis what is it?
#' @param qoi what is it?
#' @param data what is it?
#' @rdname declare_analysis
#' @export
get_estimates <- function(analysis, quantity_of_interest = NULL, data, analysis_labels = NULL) {
  
  ## extract names of arguments analysis objects
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  if(!is.null(quantity_of_interest)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(quantity_of_interest(analysis, data = data))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimates_list <- list()
      for(i in 1:length(analysis)) {
        if(!is.null(analysis[[i]]$quantity_of_interest)){
          estimates_list[[i]] <- analysis[[i]]$quantity_of_interest(get_estimates_model(analysis = analysis[[i]], data = data))
        } else {
          estimates_list[[i]] <- analysis[[i]]$estimate(data = data)
        }
        if(class(estimates_list[[i]]) != "matrix" & class(estimates_list[[i]]) != "data.frame")
          stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for analysis named", analysis_labels[i], "did not produce a matrix or data frame of results."))
        colnames(estimates_list[[i]]) <- paste(colnames(estimates_list[[i]]), analysis_labels[i], sep = "_")
      }
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimates_matrix <- estimates_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimates_matrix <- merge(estimates_matrix, estimates_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimates_matrix) <- estimates_matrix[,1]
          estimates_matrix <- estimates_matrix[, 2:ncol(estimates_matrix), drop = F]
        }
      }
      return(estimates_matrix)
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(!is.null(analysis$quantity_of_interest)){
        estimates_matrix <- analysis$quantity_of_interest(get_estimates_model(analysis = analysis, data = data))
      } else {
        estimates_matrix <- analysis$estimate(data = data)
      }
      if(class(estimates_matrix) != "matrix" & class(estimates_matrix) != "data.frame")
        stop(paste("The quantity_of_interest function you set, or in its absence the estimate function, for analysis named", analysis_labels, "did not produce a matrix or data frame of results."))
      colnames(estimates_matrix) <- paste(colnames(estimates_matrix), analysis_labels[1], sep = "_")
      return(estimates_matrix)
    }
  }
}

