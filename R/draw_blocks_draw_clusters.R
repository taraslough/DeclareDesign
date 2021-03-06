#' Draw blocks for custom block functions used in an assignment function
#'
#' @param data A data.frame used to draw blocks from.
#' @param assignment An \code{assignment} object created by \code{\link{declare_assignment}}.
#' 
#' @return a data.frame including blocks.
#'
#' @export
draw_blocks <- function(data, assignment){
  
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  
  for(i in 1:length(assignment)){
    if(!is.null(assignment[[i]]$custom_blocking_function)) { 
      data[, assignment[[i]]$block_variable_name] <- assignment[[i]]$custom_blocking_function(data = data)
    }
  }
  return(data)
}

#' Draw clusters for custom cluster functions used in an assignment function
#'
#' @param data A data.frame used to draw clusters from.
#' @param assignment An \code{assignment} object created by \code{\link{declare_assignment}}.
#' 
#' @return a data.frame including a cluster indicator.
#'
#' @export
draw_clusters <- function(data, assignment){
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = TRUE)
  
  for(i in 1:length(assignment)){
    if(!is.null(assignment[[i]]$custom_clustering_function)){
      data[, assignment[[i]]$cluster_variable_name] <- assignment[[i]]$custom_clustering_function(data = data)
    }
  }
  return(data)
}
