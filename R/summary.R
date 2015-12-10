
#' @export
summary.design <- function(object, ...) {
  
  ## population
  population <- summary(object$population)
  
  ## potential outcomes
  potential_outcomes <- summary.potential_outcomes(object$potential_outcomes)
  
  ## sampling
  if(!is.null(object$sampling)){
    sampling <- summary(object$sampling)
  } else {
    sampling <- "The design is carried out on the population."
  }  
  
  ## assignment
  assignment <- summary.assignment(object$assignment)
  
  ## estimators (with estimands)
  estimators <- summary.estimator(object$estimator)
  
  ## combine
  summary_text <- list(population = population, potential_outcomes = potential_outcomes,
                       sampling = sampling, assignment = assignment, estimators = estimators)
  
  structure(summary_text, class = c("summary.design", "design"))
  
}

#' @export
print.summary.design <- function(x, ...){
  ## prints paragraph describing assignment
  ##cat_bk <- function(x) cat(x, sep = "\n\n")
  do.call(cat, x)
  
}

## population

#' @export
summary.population <- function(object, ...) {
  object <- clean_inputs(object, "population", accepts_list = FALSE)
  size <- get("size", envir = environment(object$population))
  levels <- get_level_names(object$population)
  level_sizes <- paste(size, levels, collapse = " in ")
  summary_text <- paste0("The population is defined as", ifelse(level_sizes == "", "individuals", level_sizes), ".")
  structure(summary_text, class = c("summary.population", "population"))
}

#' @export
print.summary.sampling <- function(x, ...){
  cat(x)
}

get_level_names <- function(population){
  level_ID_variables <- get("level_IDs", envir = environment(population))
  return(tolower(gsub("_", " ", gsub("_ID", "", level_ID_variables))))
}


## potential_outcomes

#' @export
summary.potential_outcomes <- function(object, ...) {
  object <- clean_inputs(object, "potential_outcomes", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    if("formula" %in% ls(environment(object[[i]]$potential_outcomes_function))){
      formula <- formula_as_character(get("formula", envir = environment(object[[i]]$potential_outcomes_function)))
    }
    summary_text[[i]] <- paste0("An outcome ", object[[i]]$outcome_variable_name, " is defined ",
                                ifelse(!exists("formula"), "by a custom function", paste0("the formula ", formula)),
                                ".")
  }
  summary_text <- do.call(paste0, summary_text)
  structure(summary_text, class = c("summary.potential_outcomes", "potential_outcomes"))
}


#' @export
print.summary.potential_outcomes <- function(x, ...){
  cat(x)
}


## sampling

#' @export
summary.sampling <- function(object, ...) {
  object <- clean_inputs(object, "sampling", accepts_list = FALSE)
  summary_text <- paste0("The sampling strategy is ", object$sampling_type, " random assignment.")
  structure(summary_text, class = c("summary.sampling", "sampling"))
}


#' @export
print.summary.sampling <- function(x, ...){
  cat(x)
}


## assignment

#' @export
summary.assignment <- function(object, ...) {
  object <- clean_inputs(object, "assignment", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    summary_text[[i]] <- paste0("The assignment strategy is ", object[[i]]$assignment_type, " random assignment for the treatment labeled ", object[[i]]$assignment_variable_name,
                                ", and the possible treatment conditions are ", paste(object[[i]]$condition_names, collapse = " and "), ".")
  }
  summary_text <- do.call(paste0, summary_text)
  structure(summary_text, class = c("summary.assignment", "assignment"))
}

#' @export
print.summary.assignment <- function(x, ...){
  ## prints paragraph describing assignment
  cat(x)
}


## estimator

summary.estimator <- function(object, ...) {
  object <- clean_inputs(object, "estimator", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    if(!is.null(object[[i]]$model)){
      formula <- formula_as_character(get("formula", envir = environment(object[[i]]$model)))
      if(identical(object[[i]]$model, lm)){
        model <- "linear regression"
      } else if(identical(object[[i]]$model, glm)){
        model <- "generalized linear model"
      } else {
        model <- object$model_name
      }
      if(identical(object[[i]]$estimates, "get_regression_coefficient")){
        summary_text[[i]] <- paste0("the coefficient for ", get("coefficient_name", envir = environment(object[[i]]$estimates)), " from a ", model)
      }
    } else {
      if(identical(object[[i]]$estimates, DeclareDesign::difference_in_means)){
        estimator <- "difference-in-means"
      } else if(identical(object[[i]]$estimates, DeclareDesign::difference_in_means_blocked)) {
        estimator <- "block-adjusted difference-in-means"
      } else {
        estimator <- object$estimates_name
      }
      summary_text[[i]] <- paste0(estimator)
    }
    summary_text[[i]] <-  paste0("An estimator", ifelse(is.null(object$label), "", paste(", labeled", object$label, ", ")), 
                                 "is calculated using", summary_text[[i]])
  }
  summary_text <- do.call(paste0, summary_text)
  structure(summary_text, class = c("summary.estimator", "estimator"))
}

#' @export
print.summary.estimator <- function(x, ...){
  ## prints paragraph describing assignment
  cat(x)
}
