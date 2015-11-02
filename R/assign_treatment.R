#' Assign treatment
#' 
#' This function takes a data.frame and an assignment object and returns a data.frame with a treatment assignment, probabilities of assignment, and inverse probability weights.
#'
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}. 
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param random_seed An optional random seed, in order to ensure the replicability of a particular random assignment.
#'
#' @return A data.frame with new columns added for a treatment assignment, probabilities of assignment, and inverse probability weights.
#' 
#' @examples 
#' population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  treatment_variable = "Z")
#' assignment <- declare_assignment(condition_names = c(0,1))
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
#' 
#' head(smp_draw)
#' 
#' @export
assign_treatment <- function(data, assignment, random_seed = NULL) {
  
  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
  # Checks -------------------------------------------------
  if(!class(assignment)=="assignment"){
    stop("You must give assign_treatment a assignment object.")
  }
  
  # Make clusters and blocks ------------------------------------------------  
  
  if(!is.null(assignment$custom_cluster_function)){
    data[, assignment$cluster_variable_name] <- assignment$custom_cluster_function(data = data)
  }
  
  if(!is.null(assignment$custom_block_function)) { 
    data[, assignment$block_variable_name] <- assignment$custom_block_function(data = data)
  }
  
  # Assign treatment and reveal outcomes ------------------------------------------------  
  
  if(assignment$assignment_type == "existing assignment"){
    data[, assignment$treatment_variable] <- data[, assignment$existing_assignment_variable_name]
  } else {
    
    ## if the treatment is created using assign_treatment_indicator, rather than using an existing assignment variable
    
    data[, assignment$treatment_variable] <- assign_treatment_indicator(assignment = assignment, data = data)
    
    if(assignment$assignment_type != "custom" & assignment$assignment_type != "existing assignment") {
      
      data[, "assignment_probs"] <- observed_probs(treatment_assignment = assignment$treatment_variable,
                                                   assignment = assignment, data = data)
      
      data[, "assignment_weights"] <- 1/data[, "assignment_probs"]
      
      ## only reveal assignment_sampling_weights if there are sampling probabilities
      if("inclusion_probs" %in% colnames(data)){
        
        data[, "assignment_inclusion_probs"] <- data[, "assignment_probs"] * data[, "inclusion_probs"]
        
        data[, "assignment_sampling_weights"] <- 1/data[, "assignment_inclusion_probs"]
      }
    }
  }
  
  return(data)
  
}

#' Assign treatment status
#' 
#' This function takes a data.frame and an assignment object and returns an assignment vector.  Users will often prefer to use \code{\link{assign_treatment}}.
#'
#' Description
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A random assignment vector of length N.
#' @examples 
#' population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  treatment_variable = "Z")
#' assignment <- declare_assignment(condition_names = c(0,1))
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' Z <- assign_treatment_indicator(data = smp_draw, assignment=assignment)
#' table(Z)
#' @export
assign_treatment_indicator <- function(data, assignment, random_seed = NULL) {

  if(!is.null(random_seed)){
    set.seed(random_seed)
  }
  
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

#' Reveal probabilties of assignment to realized treatment conditions
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param treatment_assignment The name of the treatment assignment variable in data.
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @return A vector probabilities of assignment to treatment.
#' @examples 
#' population <- declare_population(noise = declare_variable(), N = 1000)
#' sampling <- declare_sampling(n = 500)
#' potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
#'                                                  condition_names = c(0, 1),
#'                                                  treatment_variable = "Z")
#' assignment <- declare_assignment(condition_names = c(0,1))
#' pop_draw <- draw_population(population = population)
#' smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
#' smp_draw$Z <- assign_treatment_indicator(data = smp_draw, assignment=assignment)
#' 
#' probs <- observed_probs(data = smp_draw, 
#'                         treatment_assignment= "Z",
#'                         assignment=assignment)
#'                         
#' table(probs)                         
#' 
#' @export
observed_probs <- function(data, treatment_assignment, assignment){
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
make_permutation_matrix <- function(data, assignment, sims = 100){
  permutation_matrix <- replicate(n = sims, expr = assign_treatment_indicator(assignment = assignment, data = data))
  return(permutation_matrix)
}


