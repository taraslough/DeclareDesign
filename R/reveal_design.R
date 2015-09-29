#' @export
reveal_design <- function(data, design) {
  
  # Checks -------------------------------------------------
  if(!class(design)=="design"){
    stop("You must give reveal_design a design object.")
  }

  # Make clusters and blocks ------------------------------------------------  
  
  if(!is.null(design$custom_cluster_function)){
    data[, design$cluster_variable_name] <- design$custom_cluster_function
  }
  
  if(!is.null(design$custom_block_function)) { 
    data[, design$block_variable_name] <- design$custom_block_function
  }
  
  # Assign treatment and reveal outcomes ------------------------------------------------  

  data[, design$treatment_variable] <- assign_treatment(design = design, data = data)
  
  data[, "assignment_probs"] <- observed_probs(treatment_assignment = design$treatment_variable, design = design, data = data)
  
  data[, "assignment_weights"] <- 1/data[, "assignment_probs"]
  
  data[, "assignment_inclusion_probs"] <- data[, "assignment_probs"] * data[, "inclusion_probs"]
  
  data[, "assignment_sampling_weights"] <- 1/data[, "assignment_inclusion_probs"]
  
  if(class(design$potential_outcomes) == "potential_outcomes") { design$potential_outcomes <- list(design$potential_outcomes) }
  
  for(k in 1:length(design$potential_outcomes)){
    data[, design$potential_outcomes[[k]]$outcome_name] <- observed_outcome(outcome = design$potential_outcomes[[k]]$outcome_name, 
                                                                       treatment_assignment = design$treatment_variable, 
                                                                       data = data, sep = design$potential_outcomes[[k]]$sep)
  }
  
  return(data)
  
}

#' Assign treatment status
#'
#' Description
#' @param design A design object created by \code{\link{declare_design}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A random assignment vector of length N.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # design <- declare_design(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # Z        <- assign_treatment(design, data = mock)
#' # table(Z)
#' @export
assign_treatment <- function(design, data) {
  
  ## should be expanded to take either a design object or a function
  
  N <- nrow(data)

  block_var <- data[,design$block_variable_name]
  clust_var <- data[,design$cluster_variable_name]
  
  condition_names <- design$condition_names
  baseline_condition <- design$baseline_condition
  m <- design$m
  m_each <- design$m_each
  prob_each <- design$prob_each
  block_m <- design$block_m
  block_prob <- design$block_prob
  design_type <- design$design_type

  # For custom random assignment functions
  if(is.null(design_type)){
    design_type <- "custom"
  }
  
  if(!is.null(design$custom_assignment_function)){
    if("data" %in% names(formals(design$custom_assignment_function)))
      Z <- design$custom_assignment_function(data = data)
    else
      Z <- design$custom_assignment_function()
  } 
  
  # For complete random assignment designs
  if(design_type=="complete"){
    Z <- complete_ra(N = N,
                     m = m,
                     m_each = m_each,
                     prob_each = prob_each, 
                     condition_names = condition_names,
                     baseline_condition=baseline_condition)
  }
  
  # For block random assignment designs
  if(design_type=="blocked"){
    Z <- block_ra(block_var=block_var, 
                  block_m = block_m,
                  prob_each = prob_each,
                  block_prob=block_prob,
                  condition_names = condition_names,
                  baseline_condition=baseline_condition)
  }
  
  # For cluster random assignment designs
  if(design_type=="clustered"){
    Z <- cluster_ra(clust_var=clust_var, 
                    m = m, 
                    m_each = m_each,
                    prob_each = prob_each,
                    condition_names = condition_names,
                    baseline_condition=baseline_condition)
  }
  
  # For blocked and clustered random assignment designs
  if(design_type=="blocked and clustered"){
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
#' design <- declare_design(potential_outcomes = po, m=200)
#' mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' mock$Z        <- assign_treatment(design, data = mock)
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
                all_pos[!all_pos %in% colnames(data)], ". Please either exclude a treatment arm in declare_design() or specify the missing potential outcomes in declare_potential_outcomes()."))
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
#' @param design A design object created by \code{\link{declare_design}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A vector probabilities of assignment to treatment.
#' @examples
#' smp <- declare_population(N = 850)
#' po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#'                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' design <- declare_design(potential_outcomes = po, m=200)
#' mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' mock$Z        <- assign_treatment(design, data = mock)
#' mock$prob_obs        <- observed_probs("Z", design, mock)
#' 
#' table(mock$prob_obs)
#' 
#' 
#' @export
observed_probs <- function(treatment_assignment, design, data){
  prob_mat <- get_design_probs(design = design, data = data)
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
make_permutation_matrix <- function(design, data, sims=100){
  permutation_matrix <- replicate(n = sims, expr = assign_treatment(design = design, data = data))
  return(permutation_matrix)
}



