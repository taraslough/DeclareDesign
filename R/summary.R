
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

#' Summary of design in code
#'
#' @param design A design object created by \code{\link{declare_design}}.
#'
#' @return character string representing the design in R code
#' 
#' @export
summary_code <- function(design){
  
  stitch_call <- function(call, formals){
    
    function_name <- call[[1]]
    
    call <- as.list(call)
    call[[1]] <- NULL
    
    as.call(c(function_name, call, formals[!(names(formals) %in% c("...", names(call))) & sapply(formals, function(i) is.null(i)) == FALSE]))
    
  }
  
  design <- clean_inputs(design, "design", accepts_list = FALSE)
  
  paste_skip_lines <- function(x) {
    paste(x, collapse = "\n\n")
  }
  
  steps <- c("population", "potential_outcomes", "sampling", "assignment", "estimator")
  code <- list()
  for(i in 1:length(steps)){
    step_object <- get(steps[i], design)
    if(class(step_object) == "list"){
      code_list <- list()
      for(j in 1:length(step_object)){
        code_list[[j]] <- trim_spaces(paste(deparse(stitch_call(step_object[[j]]$call, formals(get(paste0("declare_", steps[i]))))), collapse = " "))
        if(steps[i] == "estimator" & !is.null(step_object[[j]]$estimand)){
          code_list[[j]] <- paste_skip_lines(list(code_list[[j]], trim_spaces(paste(deparse(stitch_call(step_object[[j]]$estimand$call, formals(get(paste0("declare_estimand"))))), collapse = " "))))
        }
      }
      code[[i]] <- paste_skip_lines(code_list)
    } else {
      code[[i]] <- trim_spaces(paste(deparse(stitch_call(step_object$call, formals(get(paste0("declare_", steps[i]))))), collapse = " "))
    }
  }
  cat(paste_skip_lines(code))
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
  level_sizes <- ifelse(is.null(levels), "individuals", paste(size, levels, collapse = " in "))
  
  summary_text <- paste0("The population is defined as ", 
                         ifelse(length(size) > 1, level_sizes, paste(size, "units")), ".")
  structure(summary_text, class = c("summary.population", "population"))
}

#' @export
print.summary.sampling <- function(x, ...){
  cat(x)
}

get_level_names <- function(population){
  level_ID_variables <- get_level_IDs(expression_list = get("expressions", envir = environment(population)), 
                                      level_IDs = get("level_IDs", envir = environment(population)), 
                                      N_levels = get("N_levels", envir = environment(population)))
  return(gsub("[[:space:]]", "", tolower(gsub("_", " ", gsub("_ID", "", level_ID_variables)))))
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
      if(identical(get("model", envir = environment(object[[i]]$model)), lm)){
        model <- "linear regression"
      } else if(identical(get("model", envir = environment(object[[i]]$model)), glm)){
        model <- "generalized linear model"
      } else {
        model <- object[[i]]$model_name
      }
      if(identical(get("estimates", envir = environment(object[[i]]$estimates)), DeclareDesign::get_regression_coefficient)){
        summary_text[[i]] <- paste0("the coefficient for ", get("estimates_options", envir = environment(object[[i]]$estimates))$coefficient_name, " from a ", model)
      }
    } else {
      if(identical(object[[i]]$estimates, DeclareDesign::difference_in_means)){
        estimator <- "difference-in-means"
      } else if(identical(get("estimates", envir = environment(object[[i]]$estimates)), DeclareDesign::difference_in_means_blocked)) {
        estimator <- "block-adjusted difference-in-means"
      } else {
        estimator <- object[[i]]$estimates_name
      }
      summary_text[[i]] <- paste0(estimator)
    }
    summary_text[[i]] <-  paste0(ifelse(length(object) > 1, paste0("(", i, ifelse(is.null(object$label), "", paste(", labeled", object$label)), ") "), ""), 
                                 "calculated using ", summary_text[[i]])
  }
  paste_semi <- function(...) paste(... = ..., sep = "; ")
  summary_text <- paste0(ifelse(length(object) == 1, "There is one estimator: ", paste0("There are ", length(object), " estimators: ")), do.call(paste_semi, summary_text), ".")
  structure(summary_text, class = c("summary.estimator", "estimator"))
}

#' @export
print.summary.estimator <- function(x, ...){
  ## prints paragraph describing assignment
  cat(x)
}
