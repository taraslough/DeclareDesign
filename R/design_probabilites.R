
#' Calculate probabilties of assignment
#'
#' Description
#' @param design A design object created by \code{\link{declare_assignment}}; or a function that assigns treatment
#' @param data A dataframe, often created by \code{\link{draw_population}} or \code{\link{draw_sample}}.
#' @return A matrix of probabilities of assignment to treatment.
#' @examples
#' # these examples don't work yet
#' # smp <- declare_population(N = 850)
#' # po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
#' #                                    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
#' # design <- declare_assignment(potential_outcomes = po, m=200)
#' # mock          <- draw_population(potential_outcomes = po, sample =  smp)
#' # mock$Z        <- assign_treatment(design, data = mock)
#' # design_probs <- get_design_probs(design, mock)
#' 
#' head(design_probs)
#' @export
get_design_probs <- function(design, data){
  
  N <- nrow(data)  
  block_variable_name <- design$block_variable_name
  cluster_variable_name <- design$cluster_variable_name
  
  if(!is.null(block_variable_name)){
    block_var <- data[,block_variable_name]  
  }else{
    block_var <- NULL
  }
  
  if(!is.null(cluster_variable_name)){
    clust_var <- data[,cluster_variable_name]
  }else{
    clust_var <- NULL
  }
  
  condition_names <- design$condition_names
  m <- design$m
  m_each <- design$m_each
  prob_each <- design$prob_each
  block_m <- design$block_m
  block_prob <- design$block_prob
  design_type <- design$design_type
  
  if(design_type=="complete"){
    prob_mat <- complete_ra_probs(N = N, m = m, m_each = m_each, prob_each = prob_each, condition_names = condition_names)
  }
  
  if(design_type=="blocked"){
    prob_mat <- block_ra_probs(block_var = block_var, block_m = block_m, block_prob = block_prob, prob_each = prob_each, condition_names = condition_names)
  }
  
  if(design_type=="clustered"){
    prob_mat <- cluster_ra_probs(clust_var = clust_var, m = m, m_each = m_each, prob_each = prob_each, condition_names = condition_names)
  }
  
  if(design_type=="blocked and clustered"){
    prob_mat <- blocked_and_clustered_ra_probs(clust_var = clust_var, block_var = block_var, block_m = block_m, prob_each = prob_each, block_prob = block_prob, condition_names = condition_names)
  }
  
  return(prob_mat)
}

#' @export
complete_ra_probs <- function(N, m = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  
  # Setup: obtain number of arms
  num_arms <- length(condition_names)
  
  # Case 0: Two Arms and N = 1
  if(is.null(m_each) & is.null(prob_each) & num_arms ==2 & N ==1) {
    prob <- 0.5
    prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
  }
  
  # Case 1: Two Arms and N > 1
  if(is.null(m_each) & is.null(prob_each) & num_arms==2 & N > 1){
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
  
  # Case 2: We need to obtain "condition_probs" then make a matrix.
  
  # 2a: If m_each is specified
  if(!is.null(m_each) & is.null(prob_each)){
    remainder <-  N%%num_arms
    condition_probs <- (m_each/N)
  }
  
  # 2b: if neither m_each nor prob_each is specified
  if(is.null(m_each) & is.null(prob_each)){
    m_each <- rep(N%/%num_arms, num_arms)
    remainder <-  N%%num_arms
    condition_probs <- 
      (1-(remainder/num_arms))*(m_each/N) +
      (remainder/num_arms)*((m_each +1)/N)
  }
  
  # 2c: if prob_each is specified
  if(!is.null(prob_each)){
    m_each <- floor(N*prob_each)
    remainder <- N - sum(m_each)
    condition_probs <- 
      (1-(remainder/length(prob_each)))* (m_each/N) +
      (remainder/length(prob_each))* ((m_each +1)/N)
  } 
  
  # 2d: if N is smaller than number of arms, we just flip coins
  if(N < num_arms){
    condition_probs <- rep(N/num_arms, num_arms)
  }
  
  # Build prob_mat
  prob_mat <- matrix(rep(condition_probs, N), 
                     byrow=TRUE, ncol=length(condition_probs), 
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  return(prob_mat)
  
}

#' @export
block_ra_probs <- function(block_var, block_m=NULL, block_prob = NULL, prob_each = NULL, condition_names = NULL){
  
  blocks <- sort(unique(block_var))
  prob_mat <- matrix(NA, 
                     nrow = length(block_var), 
                     ncol = length(condition_names),
                     dimnames = list(NULL,  paste0("prob_",condition_names)))
  
  # Case 1: Assume (approximately) equal probabilities for all blocks and conditions.
  if(is.null(block_m) & is.null(prob_each) & is.null(block_prob)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probs(N = N_block, condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 2: block_m is specified
  if(!is.null(block_m)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probs(N = N_block, 
                                                           m_each = block_m[i,], 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 3: prob_each is specified
  if(!is.null(prob_each)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probs(N = N_block, 
                                                           prob_each = prob_each, 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
  
  # Case 4: block_prob is specified
  if(!is.null(block_prob)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      prob_mat[block_var==blocks[i],] <- complete_ra_probs(N = N_block, 
                                                           prob_each = block_prob[i,], 
                                                           condition_names=condition_names)
    }
    return(prob_mat)
  }
}

#' @export
cluster_ra_probs <- function(clust_var, m=NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  probs_clus <- complete_ra_probs(N = n_clus, m = m, m_each = m_each, prob_each = prob_each, condition_names = condition_names)
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
  merged <- merged[order(merged$init_order),]
  probs_mat <- as.matrix(merged[,colnames(probs_clus)])
  return(probs_mat)
}

#' @export
blocked_and_clustered_ra_probs <- 
  function(clust_var, block_var, block_m=NULL, prob_each=NULL, block_prob=NULL,condition_names = NULL){
    unique_clus <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clus))
    for(i in 1:length(unique_clus)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clus[i]])  
    }
    
    probs_clus <- block_ra_probs(block_var = clust_blocks, 
                                 block_m = block_m,
                                 prob_each = prob_each,
                                 block_prob=block_prob,
                                 condition_names = condition_names)
    
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
    merged <- merged[order(merged$init_order),]
    probs_mat <- as.matrix(merged[,colnames(probs_clus)])
    return(probs_mat)
  }
