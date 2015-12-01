#' Declare the design
#' 
#' This function gathers together the declarations for population, sampling, assignment, estimator(s), and potential outcomes. Design objects can be summarized with summary(my_design), diagnosed with diagnose_design(my_design), and modified with modify_design(my_design).
#' 
#' @param population A population object, as created by \code{\link{declare_population}}.
#' @param sampling A sampling object, as created by \code{\link{declare_sampling}}.
#' @param assignment An assignment object, as created by \code{\link{declare_assignment}}.
#' @param estimator An estimator object, as created by \code{\link{declare_estimator}}. Optionally a list of estimator objects.
#' @param potential_outcomes A potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. Optionally a list of potential_outcomes objects.
#' @param label An optional design label.
#' @return A design object
#' @export
declare_design <- function(population, sampling = NULL, assignment, estimator = NULL, 
                           potential_outcomes, inputs = NULL, label = NULL, description = NULL) {
  
  ## do checks
  
  ## construct estimator labels
  
  if(class(estimator) == "list"){
    estimator_object_names <- paste(substitute(estimator)[-1L])
    for(i in 1:length(estimator)){
      if(is.null(estimator[[i]]$labels)){
        estimator[[i]]$labels <- estimator_object_names[i]
      }
    }
  } else if(class(estimator) == "estimator"){
    if(is.null(estimator$labels)){
      estimator$labels <- paste(substitute(estimator))
    }
  }
  
  # Checks -------------------------------------------------
  population <- clean_inputs(population, "population", accepts_list = FALSE)
  sampling <- clean_inputs(sampling, "sampling", accepts_list = FALSE)
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  estimator <- clean_inputs(estimator, "estimator", accepts_list = TRUE)
  potential_outcomes <- clean_inputs(potential_outcomes, "potential_outcomes", accepts_list = TRUE)
  inputs <- clean_inputs(inputs, "inputs", accepts_list = FALSE)
  
  ## return object
  
  structure(list(population = population, sampling = sampling, assignment = assignment, estimator = estimator,
                 potential_outcomes = potential_outcomes, inputs = inputs, label = label, description = description,
                 call = match.call()), class = "design")
  
}

#' Modify the design
#' 
#' This function takes a design object and allows the user to modify one or more of the declarations that make up a design.
#' 
#' @param design A design object, created by \code{\link{declare_design}}.
#' @param population A population object, as created by \code{\link{declare_population}}.
#' @param sampling A sampling object, as created by \code{\link{declare_sampling}}.
#' @param assignment An assignment object, as created by \code{\link{declare_assignment}}.
#' @param estimator An estimator object, as created by \code{\link{declare_estimator}}. Optionally a list of estimator objects.
#' @param potential_outcomes A potential_outcomes object, as created by \code{\link{declare_potential_outcomes}}. Optionally a list of potential_outcomes objects.
#' @param label An optional design label.
#' @return A design object
#' @export
modify_design <- function(design, population = NULL, sampling = NULL, assignment = NULL, estimator = NULL, 
                          potential_outcomes = NULL, inputs = NULL, label = NULL, description = NULL) {
  
  if(all(is.null(population), is.null(sampling), is.null(assignment), is.null(estimator), is.null(potential_outcomes),
         is.null(label))){
    warning("No part of the design was modified, so the original design is being returned.")
    return(design)
  }
  
  if(is.null(population) & !is.null(design$population)){
    population <- design$population
  }
  if(is.null(sampling) & !is.null(design$sampling)){
    sampling <- design$sampling
  }
  if(is.null(assignment) & !is.null(design$assignment)){
    assignment <- design$assignment
  }
  if(is.null(estimator) & !is.null(design$estimator)){
    estimator <- design$estimator
  }
  if(is.null(potential_outcomes) & !is.null(design$potential_outcomes)){
    potential_outcomes <- design$potential_outcomes
  }
  if(is.null(inputs) & !is.null(design$inputs)){
    inputs <- design$inputs
  }
  if(is.null(label) & !is.null(design$label)){
    label <- design$label
  }
  if(is.null(description) & !is.null(design$description)){
    description <- design$description
  }
  
  declare_design(population = population, sampling = sampling, assignment = assignment, estimator = estimator, 
                 potential_outcomes = potential_outcomes, inputs = inputs, label = label, description = description)
  
}


#' Summary of design in code
#'
#' @param design 
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


