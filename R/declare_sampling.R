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
  sampling_type <- "complete"  ## what should this be called. Simple? 
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
      call = match.call())
  }
  class(return.object) <- "sampling"
  return(return.object)
}






#' @export
complete_sample <- function(N, n = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  complete_ra(N = N, m = n, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_sample <- function(strata_var, prob = NULL, strata_n = NULL, strata_prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  block_ra(block_var = strata_var, block_m = strata_n, block_prob = block_prob, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}


#' @export
cluster_sample <- function(clust_var, n = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  cluster_ra(clust_var = clust_var, m = n, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_and_clustered_sample <- function(clust_var, strata_var, strata_n = NULL, prob = NULL, strata_prob = NULL){
  
  prob_each <- NULL
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  # Must do someday
  # block_m, strata_n
  
  blocked_and_clustered_ra(clust_var = clust_var, block_var = strata_var, 
                           block_m = strata_n, prob_each = prob_each, 
                           block_prob = block_prob, condition_names = c(0,1), baseline_condition = 0)
}
