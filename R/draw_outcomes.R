
#' Draw potential outcomes
#' 
#' @param data 
#' @param potential_outcomes 
#' @param condition_names
#'
#' @export
draw_potential_outcomes <- function(data, potential_outcomes, condition_names = NULL, attrition = NULL) {
  
  if(is.null(potential_outcomes)){
    stop("You must provide a potential_outcomes object to draw potential_outcomes. This error may indicate you did not provide potential_outcomes to declare_estimand() if you did not directly call draw_potential_outcomes.")
  }
  
  if(class(potential_outcomes) == "list" & class(condition_names) == "list" &
     length(potential_outcomes) != length(condition_names)){
    stop("If you provide a list of potential_outcomes, you must provide a list of condition_names of the same length.")
  }
  
  potential_outcomes <- clean_inputs(potential_outcomes, object_class = "potential_outcomes")
  
  if(is.null(condition_names)){
    condition_names <- lapply(potential_outcomes, function(x) x$condition_names)
  }else{
    condition_names <- replicate(length(potential_outcomes), condition_names, simplify = FALSE)
  }
  # You must provide a condition_names argument that makes sense for all po objects.
  
  has_condition_names <- all(sapply(condition_names, function(x) is.null(x))) == FALSE
  has_assignment_variable_names <- all(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name))) == TRUE
  
  if(has_condition_names & !has_assignment_variable_names){
    stop("Please provide the name of the treatment variable to the assignment_variable_name argument in declare_potential_outcomes if you provide condition_names.")
  }
  
  if(sapply(potential_outcomes, function(x) !is.null(x$assignment_variable_name)) != length(potential_outcomes)){
    stop("If you provide a assignment_variable_name for any of the potential_outcomes, you must provide it for all of them.")
  }
  
  if(has_condition_names & has_assignment_variable_names) {
    for(i in 1:length(potential_outcomes)){
      
      # make the combinations
      
      sep = potential_outcomes[[i]]$sep
      
      condition_combinations <- expand.grid(condition_names[[i]])
      if(is.null(names(condition_names[[i]]))){
        colnames(condition_combinations) <- potential_outcomes[[i]]$assignment_variable_name
      }
      
      for(j in 1:nrow(condition_combinations)){
        
        if(ncol(condition_combinations) > 1){
          condition_combination <- lapply(1:ncol(condition_combinations[j, ]), function(x){ condition_combinations[j, x] })
        } else {
          condition_combination <- list(condition_combinations[j, ])
        }
        names(condition_combination) <- colnames(condition_combinations)
        
        outcome_name_internal <- 
          paste(potential_outcomes[[i]]$outcome_variable_name, 
                paste(names(condition_combination), condition_combinations[j,], sep = sep, collapse = sep),
                sep = sep)
        
        
        
        data[,outcome_name_internal] <- 
          draw_potential_outcome_vector(data = data, 
                                        potential_outcomes = potential_outcomes[[i]],
                                        condition_name = condition_combination)
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

#' Draw potential outcome vector
#'
#' @param data 
#' @param potential_outcomes 
#' @param condition_name 
#' @param assignment_variable_name 
#'
#' @export
draw_potential_outcome_vector <- function(data, potential_outcomes, condition_name){
  
  for(i in 1:length(condition_name)){
    data[,names(condition_name)[i]] <- condition_name[[i]]
  }
  
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
