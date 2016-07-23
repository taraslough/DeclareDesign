

#' Compare designs
#'
#' Create a diagnosis of two or more designs
#'
#' @param design A design object created by \code{\link{declare_design}}.
#' @param population A population object created by \code{\link{declare_population}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}.
#' @param assignment An assignment object created by \code{\link{declare_assignment}}.
#' @param estimator A estimator object created by \code{\link{declare_population}}.
#' @param potential_outcomes A potential_outcomes object created by \code{\link{declare_population}}.
#' @param inputs An inputs object created by \code{\link{declare_population}}.
#' @param size A vector representing the size of the population.
#' @param design_labels A vector of labels for each design that will be compared.
#' @param ... other options for diagnose_design.
#'
#' @export
compare_designs <-
  function(design,
           population = NULL,
           sampling = NULL,
           assignment = NULL,
           estimator = NULL,
           potential_outcomes = NULL,
           inputs = NULL,
           size = NULL,
           design_labels = NULL,
           variable_labels = NULL,
           ...) {
    
    # If created with quick_design
    if(class(design) == "quick_design_list"){
      design_list <- design
      design <- design$design_list
      if(is.null(variable_labels))
        variable_labels <- design_list$variable_labels
    }
    
    design <- clean_inputs(design, "design", accepts_list = TRUE)
    
    population <-
      clean_inputs(population, "population", accepts_list = TRUE)
    sampling <-
      clean_inputs(sampling, "sampling", accepts_list = TRUE)
    assignment <-
      clean_inputs(assignment, "assignment", accepts_list = TRUE)
    estimator <-
      clean_inputs(estimator, "estimator", accepts_list = TRUE)
    potential_outcomes <-
      clean_inputs(potential_outcomes,
                   c("potential_outcomes", "interference"),
                   accepts_list = TRUE)
    inputs <- clean_inputs(inputs, "inputs", accepts_list = TRUE)
    
    comparison_counts <-
      c(
        length(size),
        length(population),
        length(sampling),
        length(assignment),
        length(estimator),
        length(potential_outcomes),
        length(inputs)
      )
    
    if (any(comparison_counts > 0) & length(design) > 1) {
      stop(
        "If you provide more than one design, please do not provide other components of a design, including population, sampling, assignment, estimator, potential_outcomes, or inputs."
      )
    }
    
    if (length(design) == 1 & all(comparison_counts == 0)) {
      stop(
        "Please provide either more than one design or more than one components of a design for comparison."
      )
    }
    
    diagnoses <- list()
    
    if (length(design) < 2) {
      if (any(comparison_counts != max(comparison_counts) &
              comparison_counts > 1))
        stop(
          "Please provide either no inputs for a given option (for example, not providing any size), one input (for example a single value of size), or more than one input (for example size = c(500, 1000)). The set of varying inputs must be the same length (you cannot provide three values of size and two values of assignment)."
        )
      
      ## loop over different configurations of experiments
      for (e in 1:max(comparison_counts)) {
        if (!is.null(size)) {
          population_compare[[min(length(population_compare), e)]] <-
            substitute_input(population_compare[[min(length(population_compare), e)]], "size", size[e])
        }
        
        diagnoses[[e]] <-
          diagnose_design(
            design = modify_design(
              design = design[[1]],
              population = population[[min(length(population), e)]],
              sampling = sampling[[min(length(sampling), e)]],
              assignment = assignment[[min(length(assignment), e)]],
              estimator = estimator[[min(length(estimator), e)]],
              potential_outcomes = potential_outcomes[[min(length(potential_outcomes), e)]],
              inputs = inputs[[min(length(inputs), e)]],
              label = design_labels[[e]]
            ),
            ... = ...
          )
        
      }
    } else {
      ## if 2 or more designs are presented, compare them
      
      for (e in 1:length(design)) {
        if (!is.null(design_labels[[e]])) {
          diagnoses[[e]] <-
            diagnose_design(design = modify_design(design[[e]], label = design_labels[[e]]),
                            ... = ...)
        } else {
          diagnoses[[e]] <- diagnose_design(design = design[[e]], ... = ...)
        }
      }
    }
    
    if(is.null(variable_labels)){
      variable_labels <- data.frame(
          diagnosis = 1:length(diagnoses)
        )
    }
    
    diagnoses <- append_variable_labels(
      variable_labels = variable_labels,
      diagnoses = diagnoses)
    
    diagnosands <- do.call(rbind,lapply(diagnoses,function(x)x$diagnosands))
    simulations <- do.call(rbind,lapply(diagnoses,function(x)x$simulations))
    
    diagnoses <- list(diagnosands = diagnosands, simulations = simulations)
    
    structure(diagnoses, class = "diagnosis_list")
    
  }

#' @export
summary.diagnosis_list <- function(object, ...) {
  diagnosis_matrix <- object$diagnosands
  structure(diagnosis_matrix, class = c("summary.diagnosis_list", "data.frame"))
}

#' @export
print.diagnosis_list <- function(x, ...){
  print(summary(x))
  invisible(summary(x))
}




exists_input <- function(object, input_name) {
  call <- object$call
  
  return(is.null(call[[input_name]]))
}

substitute_input <- function(object, input_name, input_value) {
  call <- object$call
  
  if (!(is.null(input_value) & is.null(call[[input_name]])))
    call[[input_name]] <- input_value
  
  eval(call)
}



append_variable_labels <- function(variable_labels,diagnoses){
  
  if(nrow(variable_labels) != length(diagnoses)){
    stop("There must be as many rows in the variable_labels matrix or vector as there are designs to diagnose.") 
  }
  
  variable_names <- colnames(variable_labels)
  
  if(is.null(variable_names)){
    stop("You must provide a named data.frame or matrix of variable names.")
  }
  
  
  if(!"diagnosis" %in% variable_names){
    variable_labels <- cbind(as.data.frame(variable_labels),diagnosis = 1:length(diagnoses))
    variable_names <- colnames(variable_labels)
  }

  for(i in 1:length(diagnoses)){
    for(j in 1:length(variable_names)){
      diagnoses[[i]]$diagnosands[,variable_names[j]] <- variable_labels[i,j]
      diagnoses[[i]]$simulations[,variable_names[j]] <- variable_labels[i,j]
    }
  }
  
  return(diagnoses)
  
}
  













