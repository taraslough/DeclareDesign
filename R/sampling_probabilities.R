
#' Calculate probabilties of assignment
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param sampling A sampling object created by \code{\link{declare_sampling}}; or a function that assigns treatment
#' @return A matrix of probabilities of selection.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # assignment <- declare_assignment(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # mock$Z        <- assign_treatment_indicator(assignment, data = mock)
#' # assignment_probabilities <- get_assignment_probabilities(assignment, mock)
#' # head(assignment_probabilities)
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
  probability <- sampling$probability
  strata_n <- sampling$strata_n
  strata_probabilities <- sampling$strata_probabilities
  sampling_type <- sampling$sampling_type
  
  if(sampling_type=="simple"){
    probs <- simple_sampling_probabilities(N = N, m = m, probability = probability)
  }
  
  if(sampling_type=="stratified"){
    probs <- stratified_sampling_probabilities(strata_variable = strata_variable, probability = probability, strata_n = strata_n, strata_probabilities = strata_probabilities)
  }
  
  if(sampling_type=="clustered"){
    probs <- clustered_sampling_probabilities(cluster_variable = cluster_variable, m = m, probability = probability)
  }
  
  if(sampling_type=="stratified and clustered"){
    probs <- stratified_and_clustered_sampling_probabilities(cluster_variable = cluster_variable, strata_variable = strata_variable, strata_n = strata_n, probability = probability, strata_probabilities = strata_probabilities)
  }
  
  return(probs)
}

simple_sampling_probabilities <- function(N, m = NULL, probability = NULL){
  probability_each <- NULL
  
  if(!is.null(probability)){
    probability_each <- c(1 - probability, probability)
  }
  prob_mat <- complete_assignment_probabilities(N = N, m = m, probability_each = probability_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

stratified_sampling_probabilities <- function(strata_variable, probability = NULL, strata_n = NULL, strata_probabilities = NULL){
  probability_each <- NULL
  
  if(!is.null(probability)){
    probability_each <- c(1 - probability, probability)
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
  
  
  prob_mat <- blocked_assignment_probabilities(block_variable = strata_variable, block_m = strata_n_matrix, block_probabilities = block_probabilities, probability_each = probability_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

clustered_sampling_probabilities <- function(cluster_variable, m = NULL, probability = NULL){
  probability_each <- NULL
  
  if(!is.null(probability)){
    probability_each <- c(1-probability, probability)
  }
  prob_mat <- clustered_assignment_probabilities(cluster_variable = cluster_variable, m = m, probability_each = probability_each, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}

stratified_and_clustered_sampling_probabilities <- function(cluster_variable, strata_variable, strata_n = NULL, probability = NULL, strata_probabilities = NULL){
  
  probability_each <- NULL
  if(!is.null(probability)){
    probability_each <- c(1-probability, probability)
  }
  
  block_probabilities <- NULL
  if(!is.null(strata_probabilities)){
    block_probabilities <- cbind(1-strata_probabilities, strata_probabilities)  
  }
  
  # Must do someday
  # block_m, strata_n
  prob_mat <- blocked_and_clustered_assignment_probabilities(cluster_variable = cluster_variable, block_variable = strata_variable, 
                           block_m = strata_n, probability_each = probability_each, 
                           block_probabilities = block_probabilities, condition_names = c(0,1))
  
  return(prob_mat[,"prob_1"])
}
