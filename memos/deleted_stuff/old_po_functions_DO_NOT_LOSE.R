

#' @export
draw_potential_outcomes <- function(potential_outcomes, covariates, sep = "_"){
  
  N <- dim(covariates)[1]
  
  covariate_colnames <- NULL
  
  # Grab names
  outcome_name <- potential_outcomes$outcome_name
  condition_names  <- potential_outcomes$condition_names
  
  # Check if it is proportions 
  proportion_check <- "population_proportions" %in% names(potential_outcomes)
  
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
    formula <- potential_outcomes$formula
    covariate_names  <-
      all.vars(formula)[!all.vars(formula) %in% condition_names][-1]
    covariate_colnames <- colnames(covariates)
    PO_names <- paste0(outcome_name,sep,condition_names)
    
    # Check that all variables in formula are in the data
    variable_names_match <- all(all.vars(formula)[-1] %in% c(covariate_colnames,condition_names))
    
    if (!variable_names_match)
      stop(
        "All of the variables in the formula should either be in the names of the covariates or in the condition_names of the potential_outcomes.")
    
    
    
    # Make function for generating potential outcomess
    gen_outcome  <- eval(parse(
      text = paste0(
        "function(slice){y <- with(slice,{",formula[3],"});return(y)}"
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
    outcomes <- draw_potential_outcomes(potential_outcomes = potential_outcomes,
                                        covariates = covariates,
                                        sep = sep)
  }
  
  if(is_PO&is_list){
    outcomes <- draw_potential_outcomes(potential_outcomes = potential_outcomes[[1]],
                                        covariates = covariates,
                                        sep = sep)
    
    for(i in 2:length(potential_outcomes)){
      grab_outcome_names <- paste0(potential_outcomes[[i]]$outcome_name,
                                   sep,
                                   potential_outcomes[[i]]$condition_names)
      
      full_outcomes <- draw_potential_outcomes(potential_outcomes = potential_outcomes[[i]],
                                               covariates = data.frame(covariates,outcomes))
      
      merge_outcomes <- subset(full_outcomes,select = grab_outcome_names)
      
      outcomes <- data.frame(outcomes,merge_outcomes)
      
    }
    
    
  }
  
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
proportional_potential_outcomes_function <- function(data, population_proportions = NULL, outcome_name = NULL, sep = "_"){
  
  warning("You have defined the potential outcomes in terms of population-level changes in proportions. Consequently, potential outcomes cannot be a function covariates. The following arguments will be ignored: \n formula")
  
  if(any(class(population_proportions)%in%c("numeric", "double"))){
    
    warning("Assuming that population_proportions[i] = Pr(outcome[i]==1).")
    
    if(any(population_proportions>1|population_proportions<0)){
      stop("Population proportions must be in the interval [0,1].")
    }
    
    
    failure_prob <- rep(1,length(population_proportions))-population_proportions
    
    prop_names <- names(population_proportions)
    
    if(is.null(condition_names)&!is.null(prop_names)){
      condition_names <- prop_names
    }
    
    population_proportions <- matrix(
      data = c(failure_prob,
               population_proportions),
      nrow = 2,
      ncol = length(population_proportions),
      byrow = TRUE,
      dimnames = list(c(0,1),
                      c(condition_names))
    )
  }
  
  
  if(!any(class(population_proportions)%in%c("data.frame","matrix"))){
    stop("You must supply either a vector of proportions or a matrix of proportions with condition names in the columns and outcome names in the rows.")}
  
  if(!all(apply(population_proportions,2,sum,na.rm = T)==1)){
    stop("All columns of the population_proportions matrix should sum to 1. Check that you have conditions as the column names and outcomes as the row names.")
  }
  
  if(is.null(colnames(population_proportions))&is.null(condition_names)){
    stop("You must supply at least the condition_names or give condition names in the colnames of population_proportions.")
  }
  
  if(is.null(colnames(population_proportions))&!is.null(condition_names)){
    colnames(population_proportions) <- condition_names
  }else{
    if(is.null(condition_names)&!is.null(colnames(population_proportions))){
      condition_names <- colnames(population_proportions)
    }else{
      if(!all(condition_names==colnames(population_proportions))){
        stop("The order and spelling of condition_names and the column names in population_proportions should be identical.")
      }
    }
  }
  
  if(is.null(rownames(population_proportions))){
    if(dim(population_proportions)[1]==2){
      warning("No rownames supplied in population_proportions, assuming row 1 = 0 and row 2 = 1.")
      rownames(population_proportions) <- c(0,1)
    }
    else{stop("You must specify the names of the outcomes in the rows of population_proportions.")}
  }
  
  if(is.null(outcome_name)){
    warning("Outcome name not supplied, defaulting to 'Y'.")
    outcome_name <- "Y"
  }
  
  outcomes_object <- list(
    condition_names  = condition_names,
    population_proportions = population_proportions,
    outcome_name = outcome_name,
    sep = sep,
    call = match.call()
  )
  
  class(outcomes_object) <- "potential_outcomes"
  
  return(outcomes_object)
  
}

#' @export
complex_potential_outcomes_function <- function(data, formula = NULL, outcome_name = NULL, sep = "_"){
  
  
  if(is.null(condition_names)){
    stop("Please use condition_names to specify the names of the conditions to which units are assigned.")
  }
  
  if(!all(condition_names %in% all.vars(formula)))stop(
    "All conditions must be included as variables in the outcome formula."
  )
  
  if(is.null(formula)){
    stop("Please specify the data generating process as a formula that generates the outcomes as a function of the treatment and/or covariates, for example: \n Y ~ 1 + 0*control + .5*treatment + .2*income")
  }
  
  outcomes_object <- list(
    condition_names  = condition_names,
    formula  = formula,
    outcome_name     = all.vars(formula)[1],
    sep = sep,
    call = match.call()
  )
  class(outcomes_object) <- "potential_outcomes"
  
  return(outcomes_object)
}
