##estimand = mean(Y_Z1 - Y_Z0)
##estimand = "mean(Y_Z1 - Y_Z0)"
##estimand = declare_ATE()

#' @export
declare_estimand <- function(estimand = NULL, target = "population", subset = NULL, weights_variable = NULL, 
                             custom_estimand_function = NULL, label = NULL, ...) {
  
  if(!is.null(custom_estimand_function) & !is.null(estimand)){
    stop("Please provide either an estimand as a string or an expression, or a custom_estimand_function.")
  }
  
  if(is.null(custom_estimand_function)){
    
    ## if no custom estimand is provided
    
    description <- as.character(estimand)
    
    if(!is.character(eval(estimand))){
      estimand <- quote(estimand)
    } else {
      estimand <- parse(text = estimand)
    }
    
    estimand_function <- function(data){
      if(!is.null(subset))
        data <- subset(data, subset = eval(parse(text = subset)))
      ##if(!is.null(weights_variable))
      ##  estimator_options$weights <- data[, weights_variable]
      return(eval(estimand, envir = data))
    }
  } else {
    
    estimand_options <- list(...)
    
    description <- "custom estimand function"
    
    ## if a custom estimand is provided
    
    estimand_function <- function(data){
      argument_names <- names(formals(custom_estimand_function))
      if(!is.null(subset) & "subset" %in% argument_names)
        estimand_options$subset <- with(data, eval(parse(text = subset)))
      if(!is.null(weights_variable) & "weights" %in% argument_names)
        estimand_options$weights <- data[, weights_variable]
      estimand_options$data <- data
      
      return(do.call(custom_estimand_function, args = estimand_options))
    }
    
  }
  
  structure(list(estimand = estimand_function, description = description, target = target, label = label, call = match.call()), class = "estimand")
  
}

#' @export
declare_ATE <- function(condition_treat = "Z1", condition_control = "Z0", outcome = "Y", sep = "_"){
  return(paste0("mean(", outcome, sep, condition_treat, " - ", outcome, sep, condition_control, ")"))
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
          estimands_list[[i]] <- estimand[[i]]$estimand(data = population_data)
        } else if(estimand[[i]]$target == "sample") {
          if(is.null(sample_data)){
            stop(paste0("The target of ", estimand_labels[i], " is the sample, so please send get_estimands a sample data frame, i.e. one created by draw_sample()."))
          }
          estimands_list[[i]] <- estimand[[i]]$estimand(data = sample_data)
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

