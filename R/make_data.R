#' Make the full dataset or just a sample
#'
#' @param sample A sample object made with \code{\link{declare_sample}}. Contains sample size, structure, custom-DGP functions, and other information related to baseline.
#' @param potential_outcomes An optional argument for a potential_outcomes object made with \code{\link{declare_potential_outcomes}}, or a list of potential_outcomes objects. Contains all of the necessary information to construct the potential outcomes revealed by the experiment. Potential outcomes can be a function of previous outcomes in a list of potential outcomes. 
#' @param assign_treatment An optional argument indicating whether treatment should be assigned as part of make_data(). If TRUE, then the user must also provide arguments to design, outcome_variable, and reveal_outcome (see below). 
#' @param design An optional argument for a design object, made with \code{\link{declare_design}}, or list of design objects. Contains the randomization procedure and other important information, such as the name of the treatment variable. Must be provided when assign_treatment == TRUE.
#' @param outcome_variable An optional string with the name of the outcome variable. Must be provided when assign_treatment == TRUE.
#' @param reveal_outcome An optional argument that reveals the observed outcome under treatment when TRUE. 
#' @param sep a character string used in the naming of potential outcomes. Defaults to "_".
#' @param noncompliance A noncompliance object, made with \code{\link{declare_noncompliance}}.
#' @export
make_data <- function(sample, potential_outcomes = NULL,assign_treatment = FALSE,design = NULL, outcome_variable = NULL, reveal_outcome = FALSE,noncompliance = NULL,sep = "_") {
  
  # Do checks ---------------------------------------------------------------
  
  # Check whether sample provided
  if(missing(sample)){
    stop("You must provide an argument to sample, created with declare_sample(). For example, all sample size arguments should be provided to sample through declare_sample().")
  }
  
  # Default to treatment assignment when reveal_outcome = TRUE
  if(!assign_treatment&reveal_outcome){
    assign_treatment <- TRUE
  }
  
  # Check that correct arguments provided when assign_treatment == TRUE
  if(assign_treatment == TRUE){
    if(is.null(potential_outcomes)){
      stop("If you want to assign treatment, you must provide a potential_outcomes object (see declare_potential_outcomes()).")
    }
    if(is.null(design) | is.null(treatment_variable)){
      stop("If assign_treatment = TRUE, you must supply both the name of the treatment variable (i.e. 'Z', or 'treatment_status') and the design object, declared using declare_design().")
    }
  }
  
  # Get the covariates ------------------------------------------------------
  
  covariates <- get_covariates(sample = sample)
  
  # Make data if POs absent -------------------------------------------------
  
  if(is.null(potential_outcomes)){
    data <- covariates
  }
  
  # Make potential outcomes -------------------------------------------------
  
  if(!is.null(potential_outcomes)){
    
    outcomes <- loop_potential_outcomes(
      potential_outcomes = potential_outcomes,
      covariates = covariates)
    
    data <- data.frame(outcomes, covariates)
    
    # ... and reveal treatment
    
    if(assign_treatment){
      treatment <- loop_treatment(data = data,
                                  design = design,
                                  assign_treatment = assign_treatment,
                                  reveal_outcome = reveal_outcome,
                                  outcome_variable = outcome_variable,
                                  sep = sep)
      
      data <- data.frame(treatment,data)
      
    }
    
  }
  
  # Make clusters and blocks ------------------------------------------------  
  
  data <- make_clusters_blocks(design = design,data = data)
  
  # Return data -------------------------------------------------------------
  
  return(data)
  
}
























#' @export
get_covariates <- function(sample){
  if ( class(sample) != "sample" )
    stop("Please send the sample argument an object created using declare_sample. You can send just a data frame to declare_sample to use your own fixed data.")
  if (!is.null(sample$make_sample)) {
    covariates <- sample$make_sample()
  } else {
    covariates <- sample$data
  }
  return(covariates)
}

#' @export
make_potential_outcomes <- function(potential_outcomes,covariates,sep = "_"){
  
  N <- dim(covariates)[1]
  
  covariate_colnames <- NULL
  
  # Grab names
  outcome_name <- potential_outcomes$outcome_name
  condition_names  <- potential_outcomes$condition_names
  
  # Check if it is proportions 
  proportion_check <- "population_proportions"%in%names(potential_outcomes)
  
  # Proportional case
  if(proportion_check){
    if(is.null(N)){
      stop("You must provide N if you supply potential outcomes objects defined with population_proportions.")}
    
    outcomes <- make_proportion_outcomes(
      potential_outcomes = potential_outcomes,
      sep = sep,
      N = N)
    
  }
  
  # Non-Proportional case
  if(!proportion_check){
    
    # Grab outcome formula properties 
    outcome_formula <- potential_outcomes$outcome_formula
    covariate_names  <-
      all.vars(outcome_formula)[!all.vars(outcome_formula) %in% condition_names][-1]
    covariate_colnames <- colnames(covariates)
    PO_names <- paste0(outcome_name,sep,condition_names)
    
    # Check that all variables in formula are in the data
    variable_names_match <- all(all.vars(outcome_formula)[-1] %in% c(covariate_colnames,condition_names))
    
    if (!variable_names_match)
      stop(
        "All of the variables in the formula should either be in the names of the covariates or in the condition_names of the potential_outcomes.")
    
    
    
    # Make function for generating potential outcomess
    gen_outcome  <- eval(parse(
      text = paste0(
        "function(slice){y <- with(slice,{",outcome_formula[3],"});return(y)}"
      )
    ))
    
    # Loop the outcome function through the data and different values of the 
    # treatment variable
    
    outcomes <- matrix(
      data = NA,
      nrow = N,
      ncol = length(condition_names),
      dimnames = list(NULL,condition_names)
    )
    
    for (l in condition_names) {
      treat_mat <- matrix(
        data = 0,
        nrow = N,
        ncol = length(condition_names),
        dimnames = list(NULL,condition_names)
      )
      treat_mat[,l] <- 1
      
      data <- data.frame(treat_mat,covariates)
      
      outcomes[,l] <- gen_outcome(data) 
      
    }
    
    colnames(outcomes) <- PO_names
    
  }
  return(outcomes)
}

#' @export
loop_potential_outcomes <- function(potential_outcomes,covariates,sep = "_"){
  
  is_list <- class(potential_outcomes)=="list" 
  is_PO <- class(potential_outcomes)=="potential_outcomes"
  
  if(is_list){
    is_PO <- all(sapply(potential_outcomes,class)=="potential_outcomes")
  }
  
  if(!is_PO&!is_list){
    stop("You must give potential_outcomes a potential_outcomes object or a list of potential_outcomes objects.")
  }
  
  if(is_PO&!is_list){
    potential_outcomes <- list(potential_outcomes)
  }
  
  outcome_list <- lapply(potential_outcomes,
                         FUN = make_potential_outcomes,
                         covariates = covariates,
                         sep = sep)
  
  
  
  outcomes <- do.call(cbind,outcome_list)
  
  return(outcomes)
  
}

#' @export
make_proportion_outcomes <- function(potential_outcomes,N,sep = "_"){
  return_frame <- make_proportions(
    population_proportions = potential_outcomes$population_proportions,
    N = N)
  
  names(return_frame) <- paste0(potential_outcomes$outcome_name,sep,
                                potential_outcomes$condition_names)
  return(return_frame)
}

#' @export
make_proportions <- function(population_proportions,N){
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  con_names <- rownames(population_proportions)
  outcomes <- apply(counts,2,function(reps){
    sample(
      rep(con_names,reps)
    )
  })
  colnames(outcomes) <- colnames(population_proportions)
  outcomes <- integerize(as.data.frame(outcomes))
  return(outcomes)
}

#' @export
integerize <- function(data_frame){
  for(i in 1:ncol(data_frame)){
    numeric_check <- FALSE
    numeric_check <- class(data_frame[,i])%in%c("numeric","integer")
    
    if(!numeric_check){
      suppressWarnings(numeric_check <- identical(data_frame[,i],as.factor(as.integer(as.character(data_frame[,i])))))
      if(!numeric_check){
        suppressWarnings(numeric_check <- identical(data_frame[,i],as.factor(as.numeric(as.character(data_frame[,i])))))
        if(!numeric_check){
          suppressWarnings(numeric_check <- identical(data_frame[,i],as.numeric(as.character(data_frame[,i]))))
          if(!numeric_check){
            suppressWarnings(numeric_check <- identical(data_frame[,i],as.numeric(as.character(data_frame[,i]))))
          }
        }
      }
      if(numeric_check){
        data_frame[,i] <- as.integer(as.character(data_frame[,i]))
      }
    }
  }
  return(data_frame)
}

#' @export
with_treatment <- function(data,design,assign_treatment,reveal_outcome,outcome_variable,sep){
  # Get treatment variable name
  treatment_variable <- design$treatment_variable
  
  # Assign treatment 
  data[,treatment_variable] <- assign_treatment(design = design, data = data)
  
  # Reveal outcome
  if(reveal_outcome){
    data$outcome <- observed_outcome(outcome = outcome_variable, 
                                     treatment_assignment = treatment_variable, 
                                     data = data, sep = sep)
    treat_obs_out <- data.frame(data[,treatment_variable],data$outcome)
    colnames(treat_obs_out) <- c(treatment_variable,outcome_variable)
    return(treat_obs_out)
  }
  treatment <- matrix(data = data[,treatment_variable],
                      nrow = nrow(data),
                      ncol = 1,
                      dimnames = list(NULL,
                                      treatment_variable))
  return(data.frame(treatment))
}

#' @export
loop_treatment <- function(data,design,assign_treatment,reveal_outcome,outcome_variable,sep){
  
  is_list <- class(design)=="list" 
  is_design <- class(design)=="design"
  
  if(is_list){
    is_design <- all(sapply(design,class)=="design")
  }
  
  if(!is_design&!is_list){
    stop("You must give potential_outcomes a potential_outcomes object or a list of potential_outcomes objects.")
  }
  
  if(is_design&!is_list){
    design <- list(design)
  }
  
  treatment_list <- lapply(design,
                           FUN = with_treatment,
                           data = data,
                           assign_treatment = assign_treatment,
                           reveal_outcome = reveal_outcome,
                           outcome_variable = outcome_variable,
                           sep = sep
  )
  
  
  
  treatments <- do.call(cbind,treatment_list)
  
  return(treatments)
}

#' @export
make_proportions <- function(population_proportions,N){
  
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  
  con_names <- rownames(population_proportions)
  
  outcomes <- apply(counts,2,function(times){
    sample(
      rep(con_names,times = times)
    )
  })
  
  colnames(outcomes) <- colnames(population_proportions)
  
  outcomes <- integerize(as.data.frame(outcomes))
  
  return(outcomes)
}

#' @export
make_clusters_blocks <- function(design,data){
  if (!is.null(design$clusters)) {
    data <-
      cbind(data, design$clusters$cluster_function(sample = data))
  }
  
  if (!is.null(design$blocks)) {
    if (is.null(design$blocks$call$clusters)) {
      data <-
        cbind(data, design$blocks$blocks_function(sample = data))
    } else {
      cluster_frame <-
        unique(data[, c(design$clusters$cluster_name, design$blocks$call$blocks)])
      if (nrow(cluster_frame) != length(unique(data[,design$clusters$cluster_name]))) {
        stop(
          "There is more than one level of a cluster-level covariate in at least one cluster, so you cannot block on it. Please construct cluster-level variables that have a single value within clusters."
        )
      }
      cluster_frame[, design$blocks$block_name] <-
        design$blocks$blocks_function(sample = cluster_frame)
      
      data <-
        merge(
          data, 
          cluster_frame[, c(design$blocks$block_name, design$clusters$cluster_name)], 
          by = design$clusters$cluster_name, all.x = TRUE, all.y = FALSE
        )
      
    }
  }
  
  return(data)
  
}




