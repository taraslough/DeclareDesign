
cluster_function_generic <- function(clusters, cluster_name, covariates){
  x <- covariates[,clusters]
  n_digits <- nchar(as.character(length(unique(x))))
  x <- paste0("Cluster_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
  cluster_df <- data.frame(x)
  colnames(cluster_df) <- cluster_name
  return(cluster_df)
} 


#' @export
#' @param clusters A character vector describing the variable in the covariates dataframe that will be used for clustering
#' @param cluster_name A character string that gives the name of the cluster variable.
#' @param custom_cluster_function A function that takes only a dataframe of covariates and returns a vector of length n whose entries describe which cluster each unit belongs to.
declare_clusters <- function(clusters, cluster_name="cluster_variable", custom_cluster_function=NULL){
  
  if(is.null(custom_cluster_function)){
  cluster_function <- function(covariates){
    cluster_function_generic(clusters = clusters, cluster_name = cluster_name, covariates = covariates)
  }
  }
  if(!is.null(custom_cluster_function)){
    cluster_funtion <- function(covariates){
      x <- custom_cluster_function(covariates)
      n_digits <- nchar(as.character(length(unique(x))))
      x <- paste0("Cluster_",sprintf(paste0("%0",n_digits,"d"),(as.numeric(as.factor(x)))))
      cluster_df <- data.frame(x)
      colnames(cluster_df) <- cluster_name
      return(cluster_df)
    }
  }
  return_object <- list(cluster_function = cluster_function, cluster_name = cluster_name, call = match.call())  
  class(return_object) <- "clusters_object"
  
  return(return_object)
}
