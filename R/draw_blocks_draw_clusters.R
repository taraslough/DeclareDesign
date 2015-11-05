#' Draw blocks for custom block functions used in an assignment function
#'
#' @param data 
#' @param assignment 
#' @param random_seed 
#'
#' @return
#'
#' @export
draw_blocks <- function(data, assignment, random_seed = NULL){
  if(!is.null(assignment$custom_blocking_function)) { 
    data[, assignment$block_variable_name] <- assignment$custom_blocking_function(data = data)
  }
  return(data)
}

#' Draw clusters for custom cluster functions used in an assignment function
#'
#' @param data 
#' @param assignment 
#' @param random_seed 
#'
#' @return
#'
#' @export
draw_clusters <- function(data, assignment, random_seed = NULL){
  if(!is.null(assignment$custom_clustering_function)){
    data[, assignment$cluster_variable_name] <- assignment$custom_clustering_function(data = data)
  }
  return(data)
}
