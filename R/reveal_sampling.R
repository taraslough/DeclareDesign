#' draw sample from population
#'
#' Description
#' @param sampling A sampling object created by \code{\link{declare_sampling}}; or a function that samples
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @return A vector of 0's and 1's indicating which population units are sampled.
#' @export
draw_sample_indicator <- function(sampling, population_data, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  ## should be expanded to take either a sampling object or a function
  
  N <- nrow(population_data)

  strata_var <- population_data[,sampling$strata_variable_name]
  clust_var <- population_data[,sampling$cluster_variable_name]
  
  m <- sampling$m
  prob <- sampling$prob
  strata_m <- sampling$strata_m
  strata_prob <- sampling$strata_prob
  sampling_type <- sampling$sampling_type
  
  # For custom random assignment functions
  if(is.null(sampling_type)){
    sampling_type <- "custom"
  }
  
  if(!is.null(sampling$custom_sampling_function)){
    if("data" %in% names(formals(sampling$custom_sampling_function)))
      Z <- sampling$custom_sampling_function(data = population_data)
    else
      Z <- sampling$custom_sampling_function()
  } 
  
  # For "simple" random sampling
  if(sampling_type=="complete"){
    Z <- complete_sample(N = N, m = m, prob = prob)
  }
  
  # For stratified random sampling
  if(sampling_type=="stratified"){
    Z <- stratified_sample(strata_var = strata_var, strata_m = strata_m, strata_prob = strata_prob, prob = prob)
  }
  
  # For clustered random sampling
  if(sampling_type=="clustered"){
    Z <- cluster_sample(clust_var = clust_var, m = m, prob = prob)
  }
  
  # For stratified and clustered sampling
  if(sampling_type=="stratified and clustered"){
    Z <- stratified_and_clustered_sample(clust_var = clust_var, strata_var = strata_var, strata_m = strata_m, prob = prob, strata_prob = strata_prob)
  }
  
  inclusion_probs <- get_inclusion_probs(sampling = sampling, population_data = population_data)
  
  return(data.frame(sampled = Z, inclusion_probs = inclusion_probs, sampling_weights = 1/inclusion_probs))
  
}




#' @export
get_covariates <- function(population){
  if (class(population) != "population" )
    stop("Please send the population argument an object created using declare_population. You can send just a data frame to declare_population to use your own fixed data.")
  if(class(population$population) == "function")
    return(population$population())
  else if(class(population$population) == "data.frame")
    return(population$population)
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
    outcomes <- make_potential_outcomes(potential_outcomes = potential_outcomes,
                                        covariates = covariates,
                                        sep = sep)
  }
  
  if(is_PO&is_list){
    outcomes <- make_potential_outcomes(potential_outcomes = potential_outcomes[[1]],
                                        covariates = covariates,
                                        sep = sep)
    
    for(i in 2:length(potential_outcomes)){
      grab_outcome_names <- paste0(potential_outcomes[[i]]$outcome_name,
                                   sep,
                                   potential_outcomes[[i]]$condition_names)
      
      full_outcomes <- make_potential_outcomes(potential_outcomes = potential_outcomes[[i]],
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

