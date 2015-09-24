#' @export
declare_sampling <- function(prob = NULL,
                             strata = NULL, 
                             clusters = NULL,
                             m = NULL, 
                             strata_m = NULL, 
                             strata_prob = NULL,
                             custom_sampling_function = NULL) {
  
  # Determine design type
  sampling_type <- "complete"  ## what should this be called. Simple? 
  if(!is.null(strata)) {sampling_type <- "stratified"}
  if(!is.null(clusters)) {sampling_type <- "clustered"}
  if(!is.null(clusters) & !is.null(strata)) {
    sampling_type <- "stratified and clustered"
  }
  
  # Figure out strata
  if(!is.null(strata) & is.character(strata)){
    strata <- declare_strata(strata = strata, recode = FALSE)
  }
  
  if(!is.null(strata) & !is.character(strata)){
    strata_name <- strata$strata_name
  }
  
  if(is.null(strata)){strata_name=NULL}
  
  # Figure out cluster name
  if(!is.null(clusters) & is.character(clusters)){
    clusters_internal <- clusters
    clusters <- declare_clusters(clusters = clusters_internal, cluster_name = "sampling_cluster_variable")
  }  
  
  if(!is.null(clusters) & !is.character(clusters)){
    cluster_name <- clusters$cluster_name
  }
  
  if(is.null(clusters)){cluster_name=NULL}
  
  if(is.null(custom_sampling_function)){
    return.object <- list(prob = prob,
                          strata_name = strata_name,
                          cluster_name = cluster_name,
                          m = m,
                          strata_m = strata_m,
                          strata_prob = strata_prob,
                          sampling_type = sampling_type,
                          strata = strata,
                          clusters = clusters,
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
complete_sample <- function(N, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  complete_ra(N = N, m = m, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_sample <- function(strata_var, strata_m = NULL, strata_prob = NULL){
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  block_ra(block_var = strata_var, block_m = strata_m, block_prob = block_prob, condition_names = c(0,1), baseline_condition = 0)
}


#' @export
cluster_sample <- function(clust_var, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  cluster_ra(clust_var = clust_var, m = m, prob_each = prob_each, condition_names = c(0,1), baseline_condition = 0)
}

#' @export
stratified_and_clustered_ra <- function(clust_var, strata_var, strata_m = NULL, prob = NULL, strata_prob = NULL){
  
  prob_each <- NULL
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  block_prob <- NULL
  if(!is.null(strata_prob)){
    block_prob <- cbind(1-strata_prob, strata_prob)  
  }
  
  # Must do someday
  # block_m, strata_m
  
  blocked_and_clustered_ra(clust_var = clust_var, block_var = strata_var, 
                           block_m = strata_m, prob_each = prob_each, 
                           block_prob = block_prob, condition_names = c(0,1), baseline_condition = 0)
}
