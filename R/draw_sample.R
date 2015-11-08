#' Draw a sample from the population
#' 
#' This function takes a data frame representing the population and returns a data frame of the data from a sample drawn from the population.
#' 
#' @param data A data.frame object representing the population to sample from, typically created by \code{draw_population}.
#' @param sampling A sampling object describing the sampling strategy created by \code{declare_sampling}.
#' @param random_seed A random seed to fix the sampling across draws.
#'
#' @export
draw_sample <- function(data, sampling = NULL, random_seed = NULL) {
  
  # Construct strata and clusters if custom functions --------------------------------------------------
  
  if(!is.null(sampling$custom_clustering_function)){
    data[, sampling$cluster_variable_name] <- sampling$custom_clustering_function(data = data)
  }
  
  if(!is.null(sampling$custom_stratification_function)) { 
    data[, sampling$strata_variable_name] <- sampling$custom_stratification_function(data = data)
  }
  
  # Draw the sample ------------------------------------------------------
  
  Z <- draw_sample_indicator(data = data, sampling = sampling, random_seed = random_seed)
  
  if(!(sampling$sampling_type == "custom")){
    inclusion_probabilities <- get_sampling_probabilities(data = data, sampling = sampling)
    sampling_data <- data.frame(sampled = Z, inclusion_probabilities = inclusion_probabilities, sampling_weights = 1/inclusion_probabilities)
  } else {
    sampling_data <- data.frame(sampled = Z)
  }
  
  data <- data.frame(data, sampling_data)
  
  sample_data <- data[data$sampled == 1, ]
  sample_data$sampled <- NULL
  
  # Return data -------------------------------------------------------------
  
  return(sample_data)
  
}

#' Draw a sample indicator from population
#' 
#' This function takes a data.frame object representing the population and returns a vector of sampling indicators, 1 for sampled and 0 for not sampled.
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}; or a function that samples
#' @param random_seed A random seed to fix the sampling across draws.
#' 
#' @return A vector of 0's and 1's indicating which population units are sampled.
#' 
#' @export
draw_sample_indicator <- function(data, sampling, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  ## should be expanded to take either a sampling object or a function
  
  N <- nrow(data)

  strata_variable <- data[,sampling$strata_variable_name]
  cluster_variable <- data[,sampling$cluster_variable_name]
  
  n <- sampling$n
  probability <- sampling$probability
  strata_n <- sampling$strata_n
  strata_probabilities <- sampling$strata_probabilities
  sampling_type <- sampling$sampling_type
  
  # For custom random assignment functions
  if(is.null(sampling_type)){
    sampling_type <- "custom"
  }
  
  if(sampling_type == "custom"){
    if("data" %in% names(formals(sampling$custom_sampling_function)))
      Z <- sampling$custom_sampling_function(data = data)
    else
      Z <- sampling$custom_sampling_function()
  } 
  
  # For "simple" random sampling
  if(sampling_type=="simple"){
    Z <- simple_sampling(N = N, n = n, probability = probability)
  }
  
  # For stratified random sampling
  if(sampling_type=="stratified"){
    Z <- stratified_sampling(strata_variable = strata_variable, strata_n = strata_n, strata_probabilities = strata_probabilities, probability = probability)
  }
  
  # For clustered random sampling
  if(sampling_type=="clustered"){
    Z <- cluster_sampling(cluster_variable = cluster_variable, n = n, probability = probability)
  }
  
  # For stratified and clustered sampling
  if(sampling_type=="stratified and clustered"){
    Z <- stratified_and_clustered_sampling(cluster_variable = cluster_variable, strata_variable = strata_variable, strata_n = strata_n, probability = probability, strata_probabilities = strata_probabilities)
  }
  
  return(Z)
  
}

