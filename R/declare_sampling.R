#' Declare the sampling strategy
#' @param prob The probability of inclusion in the sample. Must be a scalar that takes values between 0 and 1.  
#' @param strata_variable_name The name of the variable according to which stratified random sampling should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random sampling should be conducted.
#' @param n The number of units (or clusters) to be sampled.
#' @param strata_n A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm.
#' @param strata_probabilities 
#' @param custom_sampling_function 
#' @param custom_stratification_function 
#' @param custom_clustering_function 
#'
#' @export
declare_sampling <- function(prob = NULL,
                             strata_variable_name = NULL, 
                             cluster_variable_name = NULL,
                             n = NULL, 
                             strata_n = NULL, 
                             strata_probabilities = NULL,
                             custom_sampling_function = NULL,
                             custom_stratification_function = NULL,
                             custom_clustering_function = NULL) {
  
  # Determine assignment type
  sampling_type <- "simple"  ## what should this be called. Simple? 
  if(!is.null(strata_variable_name)) {sampling_type <- "stratified"}
  if(!is.null(cluster_variable_name)) {sampling_type <- "clustered"}
  if(!is.null(cluster_variable_name) & !is.null(strata_variable_name)) {
    sampling_type <- "stratified and clustered"
  }
  
  # Checks ------------------------------------------------------------------
  if(sampling_type == "stratified" & !is.null(n)){
    stop("Please do not specify n in a stratified sampling design.  Use strata_n or strata_probabilities instead.")
  }
  
  if(!is.null(custom_stratification_function) & !is.character(strata_variable_name)){
    stop("If you supply a custom strata function, you must supply the name of the strata variable.")
  }
  
  if(!is.null(custom_clustering_function) & !is.character(cluster_variable_name)){
    stop("If you supply a custom cluster function, you must supply the name of the cluster variable.")
  }

  if(is.null(custom_sampling_function)){
    return.object <- list(prob = prob,
                          strata_variable_name = strata_variable_name,
                          cluster_variable_name = cluster_variable_name,
                          n = n,
                          strata_n = strata_n,
                          strata_probabilities = strata_probabilities,
                          sampling_type = sampling_type,
                          custom_stratification_function = custom_stratification_function,
                          custom_clustering_function = custom_clustering_function,
                          call = match.call())
  } else {
    return.object <- list(
      custom_sampling_function = custom_sampling_function,
      sampling_type = "custom",
      call = match.call())
  }
  class(return.object) <- "sampling"
  return(return.object)
}


#' @export
simple_sampling <- function(N, n = NULL, prob = NULL){
  probability_each <- NULL
  
  if(!is.null(prob)){
    probability_each <- c(1 - prob, prob)
  }
  complete_assignment(N = N, m = n, probability_each = probability_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_sampling <- function(strata_variable, prob = NULL, strata_n = NULL, strata_probabilities = NULL){
  probability_each <- NULL
  
  if(!is.null(prob)){
    probability_each <- c(1 - prob, prob)
  }
  
  block_probabilities <- NULL
  if(!is.null(strata_probabilities)){
    block_probabilities <- cbind(1-strata_probabilities, strata_probabilities)  
  }
  
  if(!is.null(strata_n)){
    strata_totals <- table(strata_variable)
    strata_n_matrix <- cbind(strata_totals - strata_n, strata_n)
  }else{
    strata_n_matrix <-NULL
  }
  
  
  blocked_assignment(block_variable = strata_variable, block_m = strata_n_matrix, block_probabilities = block_probabilities, probability_each = probability_each, condition_names = c(0,1), baseline_condition = 0)
}


#' @export
cluster_sampling <- function(cluster_variable, n = NULL, prob = NULL){
  probability_each <- NULL
  
  if(!is.null(prob)){
    probability_each <- c(1-prob, prob)
  }
  clustered_assignment(cluster_variable = cluster_variable, m = n, probability_each = probability_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_and_clustered_sampling <- function(cluster_variable, strata_variable, strata_n = NULL, prob = NULL, strata_probabilities = NULL){
  
  probability_each <- NULL
  if(!is.null(prob)){
    probability_each <- c(1-prob, prob)
  }
  
  block_probabilities <- NULL
  if(!is.null(strata_probabilities)){
    block_probabilities <- cbind(1-strata_probabilities, strata_probabilities)  
  }
  
  if(!is.null(strata_n)){
    strata_totals <- table(strata_variable)
    strata_n_matrix <- cbind(strata_totals - strata_n, strata_n)
  }else{
    strata_n_matrix <-NULL
  }
  
  blocked_and_clustered_assignment(cluster_variable = cluster_variable, block_variable = strata_variable, 
                           block_m = strata_n_matrix, probability_each = probability_each, 
                           block_probabilities = block_probabilities, condition_names = c(0,1), baseline_condition = 0)
}
