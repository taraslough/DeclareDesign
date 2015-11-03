#' Declare the sampling strategy
#' @param prob The probability of inclusion in the sample. Must be a scalar that takes values between 0 and 1.  
#' @param strata_variable_name The name of the variable according to which stratified random sampling should be conducted.
#' @param cluster_variable_name The name of the variable according to which clustered random sampling should be conducted.
#' @param n The number of units (or clusters) to be sampled.
#' @param strata_n A matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm.
#' @param strata_prob 
#' @param custom_sampling_function 
#' @param custom_strata_function 
#' @param custom_cluster_function 
#'
#' @export
declare_sampling <- function(prob = NULL,
                             strata_variable_name = NULL, 
                             cluster_variable_name = NULL,
                             n = NULL, 
                             strata_n = NULL, 
                             strata_prob = NULL,
                             custom_sampling_function = NULL,
                             custom_strata_function = NULL,
                             custom_cluster_function = NULL) {
  
  # Determine assignment type
  sampling_type <- "simple"  ## what should this be called. Simple? 
  if(!is.null(strata_variable_name)) {sampling_type <- "stratified"}
  if(!is.null(cluster_variable_name)) {sampling_type <- "clustered"}
  if(!is.null(cluster_variable_name) & !is.null(strata_variable_name)) {
    sampling_type <- "stratified and clustered"
  }
  
  # Checks ------------------------------------------------------------------
  if(sampling_type == "stratified" & !is.null(n)){
    stop("Please do not specify n in a stratified sampling design.  Use strata_n or strata_prob instead.")
  }
  
  if(!is.null(custom_strata_function) & !is.character(strata_variable_name)){
    stop("If you supply a custom strata function, you must supply the name of the strata variable.")
  }
  
  if(!is.null(custom_cluster_function) & !is.character(cluster_variable_name)){
    stop("If you supply a custom cluster function, you must supply the name of the cluster variable.")
  }

  if(is.null(custom_sampling_function)){
    return.object <- list(prob = prob,
                          strata_variable_name = strata_variable_name,
                          cluster_variable_name = cluster_variable_name,
                          n = n,
                          strata_n = strata_n,
                          strata_prob = strata_prob,
                          sampling_type = sampling_type,
                          custom_strata_function = custom_strata_function,
                          custom_cluster_function = custom_cluster_function,
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
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  complete_assignment(N = N, m = n, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_sampling <- function(strata_var, prob = NULL, strata_n = NULL, strata_prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  if(!is.null(strata_n)){
    strata_totals <- table(strata_var)
    strata_n_matrix <- cbind(strata_totals - strata_n, strata_n)
  }else{
    strata_n_matrix <-NULL
  }
  
  
  blocked_assignment(block_var = strata_var, block_m = strata_n_matrix, block_prob = block_prob, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}


#' @export
cluster_sampling <- function(clust_var, n = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  cluster_ra(clust_var = clust_var, m = n, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_and_clustered_sampling <- function(clust_var, strata_var, strata_n = NULL, prob = NULL, strata_prob = NULL){
  
  prob_each <- NULL
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  if(!is.null(strata_n)){
    strata_totals <- table(strata_var)
    strata_n_matrix <- cbind(strata_totals - strata_n, strata_n)
  }else{
    strata_n_matrix <-NULL
  }
  
  blocked_and_clustered_ra(clust_var = clust_var, block_var = strata_var, 
                           block_m = strata_n_matrix, prob_each = prob_each, 
                           block_prob = block_prob, condition_names = c(0,1), baseline_condition = 0)
}
