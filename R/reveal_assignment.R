#' @export
reveal_assignment <- function(data, assignment, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  # Checks -------------------------------------------------
  if(!class(assignment)=="assignment"){
    stop("You must give reveal_assignment a assignment object.")
  }
  
  # Make clusters and blocks ------------------------------------------------  
  
  if(!is.null(assignment$custom_cluster_function)){
    data[, assignment$cluster_variable_name] <- assignment$custom_cluster_function
  }
  
  if(!is.null(assignment$custom_block_function)) { 
    data[, assignment$block_variable_name] <- assignment$custom_block_function
  }
  
  # Assign treatment and reveal outcomes ------------------------------------------------  
  
  data[, assignment$treatment_variable] <- assign_treatment(assignment = assignment, data = data)
  
  if(assignment$assignment_type != "custom") {
    
    data[, "assignment_probs"] <- observed_probs(treatment_assignment = assignment$treatment_variable,
                                                 assignment = assignment, data = data)
    
    data[, "assignment_weights"] <- 1/data[, "assignment_probs"]
    
    ## only reveal assignment_sampling_weights if there are sampling probabilities
    if("inclusion_probs" %in% colnames(data)){
      
      data[, "assignment_inclusion_probs"] <- data[, "assignment_probs"] * data[, "inclusion_probs"]
      
      data[, "assignment_sampling_weights"] <- 1/data[, "assignment_inclusion_probs"]
    }
  }
  
  if(class(assignment$potential_outcomes) == "potential_outcomes") { assignment$potential_outcomes <- list(assignment$potential_outcomes) }
  
  for(k in 1:length(assignment$potential_outcomes)){
    data[, assignment$potential_outcomes[[k]]$outcome_name] <- observed_outcome(outcome = assignment$potential_outcomes[[k]]$outcome_name, 
                                                                       treatment_assignment = assignment$treatment_variable, 
                                                                       data = data, sep = assignment$potential_outcomes[[k]]$sep)
  }
  
  return(data)
  
}

#' Assign treatment status
#'
#' Description
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A random assignment vector of length N.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # assignment <- declare_assignment(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # Z        <- assign_treatment(assignment, data = mock)
#' # table(Z)
#' @export
assign_treatment <- function(assignment, data) {
  
  ## should be expanded to take either a assignment object or a function
  
  N <- nrow(data)

  block_var <- data[,assignment$block_variable_name]
  clust_var <- data[,assignment$cluster_variable_name]
  
  condition_names <- assignment$condition_names
  baseline_condition <- assignment$baseline_condition
  m <- assignment$m
  m_each <- assignment$m_each
  prob_each <- assignment$prob_each
  block_m <- assignment$block_m
  block_prob <- assignment$block_prob
  assignment_type <- assignment$assignment_type

  # For custom random assignment functions
  if(is.null(assignment_type)){
    assignment_type <- "custom"
  }
  
  if(!is.null(assignment$custom_assignment_function)){
    if("data" %in% names(formals(assignment$custom_assignment_function)))
      Z <- assignment$custom_assignment_function(data = data)
    else
      Z <- assignment$custom_assignment_function()
  } 
  
  # For complete random assignment designs
  if(assignment_type=="complete"){
    Z <- complete_ra(N = N,
                     m = m,
                     m_each = m_each,
                     prob_each = prob_each, 
                     condition_names = condition_names,
                     baseline_condition=baseline_condition)
  }
  
  # For block random assignment designs
  if(assignment_type=="blocked"){
    Z <- block_ra(block_var=block_var, 
                  block_m = block_m,
                  prob_each = prob_each,
                  block_prob=block_prob,
                  condition_names = condition_names,
                  baseline_condition=baseline_condition)
  }
  
  # For cluster random assignment designs
  if(assignment_type=="clustered"){
    Z <- cluster_ra(clust_var=clust_var, 
                    m = m, 
                    m_each = m_each,
                    prob_each = prob_each,
                    condition_names = condition_names,
                    baseline_condition=baseline_condition)
  }
  
  # For blocked and clustered random assignment designs
  if(assignment_type=="blocked and clustered"){
    Z <- blocked_and_clustered_ra(clust_var=clust_var,
                                  block_var=block_var, 
                                  block_m=block_m,
                                  block_prob=block_prob,
                                  prob_each = prob_each,
                                  condition_names = condition_names,
                                  baseline_condition=baseline_condition)
  }
  return(Z)
}

#' Reveal observed outcomes based on a given treatment assignment
#'
#' Description
#' @param outcome A character string
#' @param treatment_assignment A string indicating the name of the realized treatment assignment vector 
#' @param data A data frame including the outcome and realized treatment assignment vectors indicating by outcome and treatment_assignment
#' @param sep The character separating outcomes from condition names in the potential outcomes columns of data
#' @return an outcome vector of observed y
#' @examples
#' smp <- declare_population(N = 850)
#' po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#'                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' assignment <- declare_assignment(potential_outcomes = po, m=200)
#' mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' mock$Z        <- assign_treatment(assignment, data = mock)
#' mock$Y  <- observed_outcome("Y", "Z", mock)
#' summary(lm(Y~Z, data=mock))
#' @export
observed_outcome <- function(outcome = "Y", treatment_assignment, data, sep = "_"){
  
  # Checks
  if(any(is.na(data[,treatment_assignment]))>0){
    warning("There are NA's in the treatment assignment vector.") 
  }
   
  # Setup
  observed_y <- rep(NA, nrow(data))
  condition_names <- unique(data[,treatment_assignment])
  all_pos <- paste(outcome, condition_names, sep = sep)
  
  # Check to confirm that all relevant potential outcomes have been made
  if(!all(all_pos %in% colnames(data))){
    stop(paste0("The following potential outcome(s) are implied by the treatment variable, but have not defined in declare_potential_outcomes(): ", 
                all_pos[!all_pos %in% colnames(data)], ". Please either exclude a treatment arm in declare_assignment() or specify the missing potential outcomes in declare_potential_outcomes()."))
  }
  
  # Loop through the conditions, select the appropriate outomes
  for(v in condition_names){
    treat_cond <- data[,treatment_assignment] == v
    observed_y[treat_cond] <- data[treat_cond, paste0(outcome, sep, v)]
  }
  
  return(observed_y)
  
}

#' Reveal probabilties of assignment to realized treatment conditions
#'
#' Description
#' @param treatment_assignment The name of the treatment assignment variable in data.
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A vector probabilities of assignment to treatment.
#' @examples
#' smp <- declare_population(N = 850)
#' po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#'                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' assignment <- declare_assignment(potential_outcomes = po, m=200)
#' mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' mock$Z        <- assign_treatment(assignment, data = mock)
#' mock$prob_obs        <- observed_probs("Z", assignment, mock)
#' 
#' table(mock$prob_obs)
#' 
#' 
#' @export
observed_probs <- function(treatment_assignment, assignment, data){
  prob_mat <- get_assignment_probs(assignment = assignment, data = data)
  prob_obs <- rep(NA, nrow(data))
  condition_names <- unique(data[,treatment_assignment])
  for(i in 1:length(condition_names)){
    prob_obs[data[,treatment_assignment]==condition_names[i]] <- 
      prob_mat[data[,treatment_assignment]==condition_names[i], paste0("prob_", condition_names[i])]
  }
  return(prob_obs)  
}


# Propose we nix this?
#' @export
make_permutation_matrix <- function(assignment, data, sims=100){
  permutation_matrix <- replicate(n = sims, expr = assign_treatment(assignment = assignment, data = data))
  return(permutation_matrix)
}



