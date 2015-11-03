#' @export
draw_sample <- function(data, sampling = NULL, noncompliance = NULL, random_seed = NULL) {
  
  # Construct strata and clusters if custom functions --------------------------------------------------
  
  if(!is.null(sampling$custom_cluster_function)){
    data[, sampling$cluster_variable_name] <- sampling$custom_cluster_function(data = data)
  }
  
  if(!is.null(sampling$custom_strata_function)) { 
    data[, sampling$strata_variable_name] <- sampling$custom_strata_function(data = data)
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
  
  sample_data <- subset(data, sampled == 1, select = -c(sampled))
  
  # Return data -------------------------------------------------------------
  
  return(sample_data)
  
}

#' draw sample from population
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}; or a function that samples
#' @return A vector of 0's and 1's indicating which population units are sampled.
#' @export
draw_sample_indicator <- function(data, sampling, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  ## should be expanded to take either a sampling object or a function
  
  N <- nrow(data)

  strata_var <- data[,sampling$strata_variable_name]
  clust_var <- data[,sampling$cluster_variable_name]
  
  n <- sampling$n
  prob <- sampling$prob
  strata_n <- sampling$strata_n
  strata_prob <- sampling$strata_prob
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
    Z <- simple_sampling(N = N, n = n, prob = prob)
  }
  
  # For stratified random sampling
  if(sampling_type=="stratified"){
    Z <- stratified_sampling(strata_var = strata_var, strata_n = strata_n, strata_prob = strata_prob, prob = prob)
  }
  
  # For clustered random sampling
  if(sampling_type=="clustered"){
    Z <- cluster_sampling(clust_var = clust_var, n = n, prob = prob)
  }
  
  # For stratified and clustered sampling
  if(sampling_type=="stratified and clustered"){
    Z <- stratified_and_clustered_sampling(clust_var = clust_var, strata_var = strata_var, strata_n = strata_n, prob = prob, strata_prob = strata_prob)
  }
  
  return(Z)
  
}

