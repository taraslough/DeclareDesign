
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
  
  
  ## combine
  
  summary_text <- list(population = population, potential_outcomes = potential_outcomes,
                       sampling = sampling, assignment = assignment)
  
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
  summary_text <- paste0("The population is defined containing ", size[1], " units.")
  structure(summary_text, class = c("summary.population", "population"))
}


#' @export
print.summary.sampling <- function(x, ...){
  cat(x)
}


## potential_outcomes

#' @export
summary.potential_outcomes <- function(object, ...) {
  object <- clean_inputs(object, "potential_outcomes", accepts_list = TRUE)
  summary_text <- list()
  for(i in 1:length(object)){
    if("formula" %in% ls(environment(object[[i]]$potential_outcomes_function))){
      formula <- Reduce(paste, deparse(get("formula", envir = environment(object[[i]]$potential_outcomes_function))))
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
