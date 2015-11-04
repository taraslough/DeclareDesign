
#' Calculate probabilties of assignment
#'
#' Description
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A matrix of probabilities of assignment to treatment.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # assignment <- declare_assignment(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # mock$Z        <- assign_treatment_indicator(assignment, data = mock)
#' # assignment_probabilities <- get_assignment_probabilities(assignment, mock)
#' 
#' head(assignment_probabilities)
#' @export
get_sampling_probabilities <- function(data, sampling){
  
  N <- nrow(data)  
  strata_variable_name <- sampling$strata_variable_name
  cluster_variable_name <- sampling$cluster_variable_name
  
  if(!is.null(strata_variable_name)){
    strata_variable <- data[,strata_variable_name]  
  }else{
    strata_variable <- NULL
  }
  
  if(!is.null(cluster_variable_name)){
    cluster_variable <- data[,cluster_variable_name]
  }else{
    cluster_variable <- NULL
  }
  
  
  m <- sampling$m
  prob <- sampling$prob
  strata_m <- sampling$strata_m
  strata_prob <- sampling$strata_prob
  sampling_type <- sampling$sampling_type
  
  if(sampling_type=="simple"){
    probs <- simple_sampling_probabilities(N = N, m = m, prob = prob)
  }
  
  if(sampling_type=="stratified"){
    probs <- stratified_sampling_probabilities(strata_variable = strata_variable, prob = prob, strata_m = strata_m, strata_prob = strata_prob)
  }
  
  if(sampling_type=="clustered"){
    probs <- clustered_sampling_probabilities(cluster_variable = cluster_variable, m = m, prob = prob)
  }
  
  if(sampling_type=="stratified and clustered"){
    probs <- stratified_and_clustered_sampling_probabilities(cluster_variable = cluster_variable, strata_variable = strata_variable, strata_m = strata_m, prob = prob, strata_prob = strata_prob)
  }
  
  return(probs)
}

#' @export
simple_sampling_probabilities <- function(N, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  prob_mat <- complete_assignment_probabilities(N = N, m = m, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

#' @export
stratified_sampling_probabilities <- function(strata_variable, prob = NULL, strata_m = NULL, strata_prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1 - prob, prob)
  }
  
  block_probabilities <- NULL
  if(!is.null(strata_prob)){
    block_probabilities <- cbind(1-strata_prob, strata_prob)  
  }
  
  prob_mat <- blocked_assignment_probabilities(block_variable = strata_variable, block_m = strata_m, block_probabilities = block_probabilities, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}


#' @export
clustered_sampling_probabilities <- function(cluster_variable, m = NULL, prob = NULL){
  prob_each <- NULL
  
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  prob_mat <- clustered_assignment_probabilities(cluster_variable = cluster_variable, m = m, prob_each = prob_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

#' @export
stratified_and_clustered_sampling_probabilities <- function(cluster_variable, strata_variable, strata_m = NULL, prob = NULL, strata_prob = NULL){
  
  prob_each <- NULL
  if(!is.null(prob)){
    prob_each <- c(1-prob, prob)
  }
  
  block_probabilities <- NULL
  if(!is.null(strata_prob)){
    block_probabilities <- cbind(1-strata_prob, strata_prob)  
  }
  
  # Must do someday
  # block_m, strata_m
  prob_mat <- blocked_and_clustered_assignment_probabilities(cluster_variable = cluster_variable, block_variable = strata_variable, 
                           block_m = strata_m, prob_each = prob_each, 
                           block_probabilities = block_probabilities, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}
