#' @export
declare_estimand <- function(expression, target = "population", subset, weights_variable) {
  
  if(!is.character(eval(expression))){
    expression <- quote(expression)
  } else {
    expression <- parse(text = expression)
  }
  
  estimand <- function(data){
    if(!is.null(subset))
      data <- subset(data, subset = eval(parse(text = subset)))
    ##if(!is.null(weights_variable))
    ##  estimator_options$weights <- data[, weights_variable]
    return(eval(expression, envir = data))
  }
  
  structure(list(estimand = estimand, target = target, call = match.call()), class = "estimand")
  
}



#' @param analysis what is it?
#' @param qoi  what is it?
#' @param data  what is it?
#' @param statistics  what is it?
#' @rdname declare_analysis
#' @export
get_estimands <- function(analysis, qoi = NULL, data, design = design, statistics = "est", analysis_labels = NULL){
  
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  if(!is.null(qoi)) {
    ## if there is a user-defined qoi function, use that to extract qoi from analysis object or list of them
    return(qoi(analysis, data = truth_data_frame(formula = analysis$estimand_formula, data = data, 
                                                 estimand_options = analysis$estimand_options,
                                                 design = design)))
  } else {
    ## otherwise use qoi function defined in the analysis
    if(class(analysis) == "list"){
      ## if the user sends no qoi function but does send a list of analysis objects,
      ## run this function on each analysis object and cbind the results
      estimands_list <- list()
      for(i in 1:length(analysis)){
        if(!is.null(analysis[[i]]$estimand_quantity_of_interest)){
          estimands_list[[i]] <- analysis[[i]]$estimand_quantity_of_interest(get_estimands_model(analysis = analysis[[i]], 
                                                                                                 data = data, design = design), 
                                                                             statistics = statistics)
          ## get_estimands_model does truth_data_frame, so just sending it data
        } else {
          estimands_list[[i]] <- analysis[[i]]$estimand(truth_data_frame(formula = analysis[[i]]$estimand_formula, data = data, 
                                                                         estimand_options = analysis[[i]]$estimand_options,
                                                                         design = design))
        }
        colnames(estimands_list[[i]]) <- paste(colnames(estimands_list[[i]]), analysis_labels[i], sep = "_")
      }
      ## when it is sent back to get_estimands() it will run truth_data_frame, so just sending it data
      
      ## this merges the summary statistics together such that there can be different statistics for each analysis
      ## and they are merged and named correctly
      estimands_matrix <- estimands_list[[1]]
      if(length(analysis) > 1){
        for(i in 2:length(analysis)){
          estimands_matrix <- merge(estimands_matrix, estimands_list[[i]], by = "row.names", all.x = T, all.y = T)
          rownames(estimands_matrix) <- estimands_matrix[,1]
          estimands_matrix <- estimands_matrix[, 2:ncol(estimands_matrix), drop = F]
        }
      }
      return(estimands_matrix)
    } else {
      if(class(analysis) != "analysis")
        stop("The object in the analysis argument must by created by the declare_analysis function.")
      ## otherwise process the one analysis function
      if(!is.null(analysis$estimand_quantity_of_interest)){
        estimands_matrix <- analysis$estimand_quantity_of_interest(get_estimands_model(analysis = analysis, 
                                                                                       data = data, design = design), 
                                                                   statistics = statistics)
        ## get_estimands_model does truth_data_frame, so just sending it data
      } else {
        estimands_matrix <- analysis$estimand(truth_data_frame(formula = analysis$estimand_formula, data = data,
                                                               estimand_options = analysis$estimand_options,
                                                               design = design))
      }
      colnames(estimands_matrix) <- paste(colnames(estimands_matrix), analysis_labels[1], sep = "_")
      return(estimands_matrix)
    }
  }
}

