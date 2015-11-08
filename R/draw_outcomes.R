
#' Draw potential outcomes
#' 
#' @param data 
#' @param potential_outcomes 
#' @param condition_names
#'
#' @export
draw_potential_outcomes <- function(data, potential_outcomes, condition_names = NULL) {
  
  if(is.null(potential_outcomes)){
    stop("You must provide a potential_outcomes object to draw potential_outcomes. This error may indicate you did not provide potential_outcomes to declare_estimand() if you did not directly call draw_potential_outcomes.")
  }
  
  if(class(potential_outcomes) == "list" & class(condition_names) == "list" &
     length(potential_outcomes) != length(condition_names)){
    stop("If you provide a list of potential_outcomes, you must provide a list of condition_names of the same length.")
  }
  
  if(class(potential_outcomes) != "list"){
    potential_outcomes <- list(potential_outcomes)
  }
  
  if(is.null(condition_names)){
    condition_names <- lapply(potential_outcomes, function(x) x$condition_names)
  }
  
  if(class(potential_outcomes) == "list" & class(condition_names) != "list"){
    condition_names <- replicate(length(potential_outcomes), condition_names, simplify = FALSE)
  }
  
  has_condition_names <- all(sapply(condition_names, function(x) is.null(x))) == FALSE
  has_assignment_variable_names <- all(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name))) == TRUE
  create_columns <- has_condition_names & has_assignment_variable_names
  
  if(has_condition_names & !has_assignment_variable_names){
    stop("Please provide the name of the treatment variable to the assignment_variable_name argument in declare_potential_outcomes if you provide condition_names.")
  }
  
  if(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name)) != length(potential_outcomes)){
    stop("If you provide a assignment_variable_name for any of the potential_outcomes, you must provide it for all of them.")
  }
  
  if(create_columns == TRUE) {
    for(i in 1:length(potential_outcomes)){
      for(j in 1:length(condition_names[[i]])){
        data_internal <- data
        data_internal[potential_outcomes[[i]]$assignment_variable_name] <- condition_names[[i]][j]
        data[,paste0(potential_outcomes[[i]]$outcome_variable_name, potential_outcomes[[i]]$sep, 
                     condition_names[[i]][j])] <- draw_outcome_vector(data = data_internal, potential_outcomes = potential_outcomes[[i]])
      }
    }
  }
  
  return(data)
  
}

#' Draw observed outcome
#' 
#' @param data 
#' @param potential_outcomes 
#'
#' @export
draw_outcome <- function(data, potential_outcomes){
  
  if(class(potential_outcomes) == "potential_outcomes"){
    potential_outcomes <- list(potential_outcomes)
  }
  
  for(i in 1:length(potential_outcomes)){
      data[, potential_outcomes[[i]]$outcome_variable_name] <- 
        draw_outcome_vector(data = data, potential_outcomes = potential_outcomes[[i]])
  }
  
  return(data)
  
}

#' Draw observed outcome (vector)
#' @param data 
#' @param potential_outcomes 
#'
#' @export
draw_outcome_vector <- function(data, potential_outcomes){
  
  outcome_draw <- potential_outcomes$potential_outcomes_function(data = data)
  
  if( is.atomic(outcome_draw) || is.list(outcome_draw)) {
    outcome_draw <- as.numeric(outcome_draw)
    if(length(outcome_draw) != nrow(data)){
      stop("The potential_outcomes function returned an outcome with a different number of rows than the population data. They must be the same.")
    }
  } else {
    stop("The potential_outcomes function you provided returned something other than a vector, like a matrix. Please edit your potential_outcomes function.")
  }
  
  return(outcome_draw)
  
}


