#' Estimate the ICC of a given variable
#'
#' @param variable A vector whose intra-cluster correlation coefficient should be estimated
#' @param cluster A vector of cluster IDs indicating the cluster corresponding to each element of variable 
#' @export
estimate_ICC <- function(variable,cluster){
  # Code inspired by ICC package, Matthew Wolak <matthewwolak@gmail.com>
  # 2015-06-17 licensed under GPL (>= 2)
  anova_out <- anova(aov(variable ~ as.factor(cluster)))
  J <- length(unique(cluster))
  mean_sq_j <- anova_out$"Mean Sq"[1]
  mean_sq_i <- var_i <- anova_out$"Mean Sq"[2]
  n_j <- tapply(variable,as.factor(cluster), FUN = length)
  k <- (1/(J - 1)) * (sum(n_j) - (sum(n_j^2)/sum(n_j)))
  var_j <- (mean_sq_j - mean_sq_i)/k
  rho <- var_j/(var_i + var_j)
  return(rho)
}



#' Built-in Estimand Functions: declare_ATE()
#' 
#' This function is designed to be passed to the estimand argument of the \code{\link{declare_estimand}} function, and can be used to specify a common estimand, the average difference between two potential outcomes.
#' 
#' @param condition_treat The condition name of the "treatment" condition. By default "Z1".
#' @param condition_control The condition name of the "control" condition. By default "Z0".
#' @param outcome The name of the outcome variable.  By default "Y".
#' @param sep The separator used to create potential outcomes columns. By default "_", so that potential outcomes are constructed as, for example,  "Y_Z1" and "Y_Z0".
#'
#' @export
declare_ATE <- function(condition_treat = "Z1", condition_control = "Z0", outcome = "Y", sep = "_"){
  return(paste0("mean(", outcome, sep, condition_treat, " - ", outcome, sep, condition_control, ")"))
}


# Propose we nix this?
#' @export
make_permutation_matrix <- function(data, assignment, sims = 100){
  permutation_matrix <- replicate(n = sims, expr = assign_treatment_indicator(assignment = assignment, data = data))
  return(permutation_matrix)
}

