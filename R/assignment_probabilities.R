
#' Calculate probabilities of assignment
#'
#' Description
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @param assignment A assignment object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @return A matrix of probabilities of assignment to treatment.
#' @examples 
#' population <- declare_population(size = 850)
#' sampling <- declare_sampling(n=500)
#' potential_outcomes <- declare_potential_outcomes(condition_names = c(0, 1),
#'                                                  formula = Y ~ 1 + 1*Z)
#' assignment <- declare_assignment(potential_outcomes = potential_outcomes, m = 200)
#' 
#' design <- declare_design(population = population, 
#'                          sampling = sampling, 
#'                          assignment = assignment, 
#'                          potential_outcomes = potential_outcomes)
#' 
#' smp_draw <- draw_data(design)
#' 
#' assignment_probabilities <- get_assignment_probabilities(data = smp_draw, assignment = assignment)
#' head(assignment_probabilities)
#' @export
get_assignment_probabilities <- function(data, assignment){
  
  # Checks -------------------------------------------------
  assignment <- clean_inputs(assignment, "assignment", accepts_list = FALSE)
  
  N <- nrow(data)  
  block_variable_name <- assignment$block_variable_name
  cluster_variable_name <- assignment$cluster_variable_name
  
  if(!is.null(block_variable_name)){
    block_variable <- data[,block_variable_name]  
  }else{
    block_variable <- NULL
  }
  
  if(!is.null(cluster_variable_name)){
    cluster_variable <- data[,cluster_variable_name]
  }else{
    cluster_variable <- NULL
  }
  
  condition_names <- assignment$condition_names
  m <- assignment$m
  m_each <- assignment$m_each
  probability_each <- assignment$probability_each
  block_m <- assignment$block_m
  block_m_each <- assignment$block_m_each
  block_probabilities <- assignment$block_probabilities
  assignment_type <- assignment$assignment_type
  
  if(assignment_type=="complete"){
    prob_mat <- complete_assignment_probabilities(N = N, m = m, m_each = m_each, probability_each = probability_each, condition_names = condition_names)
  }
  
  if(assignment_type=="blocked"){
    prob_mat <- blocked_assignment_probabilities(block_variable = block_variable, block_m = block_m, block_m_each = block_m_each, block_probabilities = block_probabilities, probability_each = probability_each, condition_names = condition_names)
  }
  
  if(assignment_type=="clustered"){
    prob_mat <- clustered_assignment_probabilities(cluster_variable = cluster_variable, m = m, m_each = m_each, probability_each = probability_each, condition_names = condition_names)
  }
  
  if(assignment_type=="blocked and clustered"){
    prob_mat <- blocked_and_clustered_assignment_probabilities(cluster_variable = cluster_variable, block_variable = block_variable, block_m_each = block_m_each, probability_each = probability_each, block_probabilities = block_probabilities, condition_names = condition_names)
  }
  
  return(prob_mat)
}

complete_assignment_probabilities <- function(N, m = NULL, m_each = NULL, probability_each = NULL, condition_names = NULL){
  
  # Setup: obtain number of arms
  num_arms <- length(condition_names)
  
  # Case 0: Two Arms and N = 1
  if(is.null(m_each) & is.null(probability_each) & num_arms ==2 & N ==1) {
    prob <- 0.5
    prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
  }
  
  # Case 1: Two Arms and N > 1
  if(is.null(m_each) & is.null(probability_each) & num_arms==2 & N > 1){
    m_floor <- m
    m_ceiling <- m
    
    if(is.null(m)){
      m_floor <- floor(N/2)
      m_ceiling <- ceiling(N/2)
    }
    
    prob <- 0.5*(m_floor/N) + 0.5*(m_ceiling/N)
    prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
  }
  
  # Case 2: We need to obtain "condition_probabilities" then make a matrix.
  
  # 2a: If m_each is specified
  if(!is.null(m_each) & is.null(probability_each)){
    remainder <-  N%%num_arms
    condition_probabilities <- (m_each/N)
  }
  
  # 2b: if neither m_each nor probability_each is specified
  if(is.null(m_each) & is.null(probability_each)){
    m_each <- rep(N%/%num_arms, num_arms)
    remainder <-  N%%num_arms
    condition_probabilities <- 
      (1-(remainder/num_arms))*(m_each/N) +
      (remainder/num_arms)*((m_each +1)/N)
  }
  
  # 2c: if probability_each is specified
  if(!is.null(probability_each)){
    m_each <- floor(N*probability_each)
    remainder <- N - sum(m_each)
    condition_probabilities <- 
      (1-(remainder/length(probability_each)))* (m_each/N) +
      (remainder/length(probability_each))* ((m_each +1)/N)
  } 
  
  # 2d: if N is smaller than number of arms, we just flip coins
  if(N < num_arms){
    condition_probabilities <- rep(N/num_arms, num_arms)
  }
  
  # Build prob_mat
  prob_mat <- matrix(rep(condition_probabilities, N), 
                     byrow=TRUE, ncol=length(condition_probabilities), 
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  return(prob_mat)
  
}

blocked_assignment_probabilities <- function(block_variable, block_m = NULL, block_m_each=NULL, block_probabilities = NULL, probability_each = NULL, condition_names = NULL){
  
  blocks <- sort(unique(block_variable))
  prob_mat <- matrix(NA, 
                     nrow = length(block_variable), 
                     ncol = length(condition_names),
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  
  # Case 1: Assume (approximately) equal probabilities for all blocks and conditions.
  if(is.null(block_m_each) & is.null(probability_each) & is.null(block_probabilities) & is.null(block_m)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_variable==blocks[i])
      prob_mat[block_variable==blocks[i],] <- complete_assignment_probabilities(N = N_block, condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 2: block_m_each is specified
  if(!is.null(block_m_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_variable==blocks[i])
      prob_mat[block_variable==blocks[i],] <- complete_assignment_probabilities(N = N_block, 
                                                           m_each = block_m_each[i,], 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 3: probability_each is specified
  if(!is.null(probability_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_variable==blocks[i])
      prob_mat[block_variable==blocks[i],] <- complete_assignment_probabilities(N = N_block, 
                                                           probability_each = probability_each, 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 4: block_probabilities is specified
  if(!is.null(block_probabilities)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_variable==blocks[i])
      prob_mat[block_variable==blocks[i],] <- complete_assignment_probabilities(N = N_block, 
                                                           probability_each = block_probabilities[i,], 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 5: block_m is specified
  if(!is.null(block_m)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_variable==blocks[i])
      prob_mat[block_variable==blocks[i],] <- complete_assignment_probabilities(N = N_block, 
                                                                                m = block_m[i], 
                                                                                condition_names=condition_names)
    }
    return(prob_mat)
  }
  
}

clustered_assignment_probabilities <- function(cluster_variable, m=NULL, m_each = NULL, probability_each = NULL, condition_names = NULL){
  unique_clus <- unique(cluster_variable)
  n_clus <- length(unique_clus)
  probs_clus <- complete_assignment_probabilities(N = n_clus, m = m, m_each = m_each, probability_each = probability_each, condition_names = condition_names)
  merged <- merge(x = data.frame(cluster_variable, init_order = 1:length(cluster_variable)), 
                  data.frame(cluster_variable=unique_clus, probs_clus), by="cluster_variable")
  merged <- merged[order(merged$init_order),]
  probs_mat <- as.matrix(merged[,colnames(probs_clus)])
  return(probs_mat)
}

blocked_and_clustered_assignment_probabilities <- 
  function(cluster_variable, block_variable, block_m_each=NULL, probability_each=NULL, block_probabilities=NULL,condition_names = NULL){
    unique_clus <- unique(cluster_variable)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clus))
    for(i in 1:length(unique_clus)){
      clust_blocks[i] <- unique(block_variable[cluster_variable==unique_clus[i]])  
    }
    
    probs_clus <- blocked_assignment_probabilities(block_variable = clust_blocks, 
                                 block_m_each = block_m_each,
                                 probability_each = probability_each,
                                 block_probabilities=block_probabilities,
                                 condition_names = condition_names)
    
    merged <- merge(x = data.frame(cluster_variable, init_order = 1:length(cluster_variable)), 
                    data.frame(cluster_variable=unique_clus, probs_clus), by="cluster_variable")
    merged <- merged[order(merged$init_order),]
    probs_mat <- as.matrix(merged[,colnames(probs_clus)])
    return(probs_mat)
  }
