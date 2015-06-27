#' Assign treatment status
#'
#' Description
#' @param design A design object created by declare_design(); or a function that assigns treatment
#' @param data A dataframe, often created by make_data()
#' @return a random assignment vector of length N.
#' @examples
#' # some examples go here
#' @export
assign_treatment <- function(design, data) {
  
  ## should be expanded to take either a design object or a function
  ## may be need to have this respond to the characteristics of the data frame, i.e. N and m?
  
  N <- nrow(data)
  # It occurs to me that condition_names will be inherited from potential_outcomes (and exclude arms).  
  # Should we just simplify to prob_each or block_prob?
  block_name <- design$block_name
  cluster_name <- design$cluster_name
  block_var <- data[,block_name]
  clust_var <- data[,cluster_name]
  
  condition_names <- design$condition_names
  m <- design$m
  m_each <- design$m_each
  prob_each <- design$prob_each
  block_m <- design$block_m
  block_prob <- design$block_prob
  design_type <- design$design_type
  
  if(design_type=="complete"){
    Z <- complete_ra(N = N,
                     m_each = m_each,
                     prob_each = prob_each, 
                     condition_names = condition_names)
  }
  
  if(design_type=="blocked"){
    Z <- block_ra(block_var=block_var, 
                  block_m = block_m,
                  block_prob = block_prob, 
                  condition_names = condition_names)
  }
  
  
  if(design_type=="clustered"){
    Z <- cluster_ra(clust_var=clust_var, 
                    m = m, 
                    m_each = m_each,
                    condition_names = condition_names)
  }
  
  if(design_type=="blocked and clustered"){
    Z <- blocked_and_clustered_ra(clust_var=clust_var,
                                  block_var=block_var, 
                                  block_m=block_m,
                                  block_prob = block_prob, 
                                  condition_names = condition_names)
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
#' # some examples go here
#' @export
observed_outcome <- function(outcome = "Y", treatment_assignment, data, sep = "_"){
  
  if(any(is.na(data[,treatment_assignment]))>0)
    warning("There are NA's in the treatment assignment vector.")

  observed_y <- rep(NA, nrow(data))
  condition_names <- unique(data[,treatment_assignment])
  for(v in condition_names){
    treat_cond <- data[,treatment_assignment] == v
    observed_y[treat_cond] <- data[treat_cond, paste0(outcome, sep, v)]
  }
  
  return(observed_y)
  
}


#' Calculate probabilties of assignment
#'
#' Description
#' @param design A design object created by declare_design(); or a function that assigns treatment
#' @param data A dataframe, often created by make_data()
#' @return a matrix of probabilities of assignment to treatment
#' @examples
#' # some examples go here
#' @export
get_design_probs <- function(design, data){
  
  N <- nrow(data)  
  block_name <- design$block_name
  cluster_name <- design$cluster_name
  
  if(!is.null(block_name)){
    block_var <- data[,block_name]  
  }else{
    block_var <- NULL
  }
  
  if(!is.null(cluster_name)){
    clust_var <- data[,cluster_name]
  }else{
    clust_var <- NULL
  }
  
  condition_names <- design$condition_names
  m <- design$m
  m_each <- design$m_each
  prob_each <- m <- design$prob_each
  block_m <- design$block_m
  block_prob <- design$block_prob
  design_type <- design$design_type
  num_arms <- length(unique(condition_names))
  
  prob_mat <- design_probs(N = N, 
                           prob_each = prob_each, 
                           m = m, 
                           m_each = m_each,
                           block_var = block_var, 
                           block_m = block_m, 
                           block_prob = block_prob,
                           clust_var = clust_var, 
                           num_arms=num_arms, 
                           condition_names = condition_names, 
                           design_type=design_type)
  
  return(prob_mat)
}

#' @export
design_probs <- function(N= NULL, 
                         prob_each=NULL, 
                         m = NULL, 
                         m_each = NULL,
                         block_var = NULL, 
                         block_m = NULL, 
                         block_prob = NULL,
                         clust_var = NULL, 
                         num_arms=NULL, 
                         condition_names = NULL, 
                         design_type){
  
  if(design_type=="complete"){
    
    if(is.null(m_each) & is.null(condition_names) & is.null(num_arms) & is.null(prob_each)){
      m_floor <- m
      m_ceiling <- m
      
      if(is.null(m)){
        m_floor <- floor(N/2)
        m_ceiling <- ceiling(N/2)
      }
      
      prob <- 0.5*(m_floor/N) + 0.5*(m_ceiling/N)
      condition_names <- c(0, 1)
      prob_mat <- matrix(rep(c(1-prob, prob), N), byrow=TRUE, ncol=2, dimnames = list(NULL,  paste0("prob_",condition_names)))
      return(prob_mat)
    }
    
    if(!is.null(m_each) & is.null(prob_each)){
      if(is.null(num_arms)){
        num_arms <- length(m_each)
      }
      remainder <-  N%%num_arms
      condition_probs <- (m_each/N)
    }
    
    if(is.null(m_each) & is.null(prob_each)){
      if(is.null(num_arms)){
        num_arms <- length(condition_names)
      }
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <-  N%%num_arms
      condition_probs <- 
        (1-(remainder/num_arms))* (m_each/N) +
        (remainder/num_arms)* ((m_each +1)/N)
    }
    
    
    
    if(!is.null(prob_each)){
      m_each <- floor(N*prob_each)
      remainder <- N - sum(m_each)
      condition_probs <- 
        (1-(remainder/length(prob_each)))* (m_each/N) +
        (remainder/length(prob_each))* ((m_each +1)/N)
    } 
    
    
    if(is.null(num_arms)){
      num_arms <- length(m_each)
    }
    
    if(is.null(condition_names)){
      condition_names <- paste0("T", 1:num_arms)
    }
    
    if(N < num_arms){
      condition_probs <- rep(N/num_arms, num_arms)
    }
    
    prob_mat <- matrix(rep(condition_probs, N), byrow=TRUE, ncol=length(condition_probs), dimnames = list(NULL,  paste0("prob_",condition_names)))
    return(prob_mat)
    
  }
  
  if(design=="blocked"){
    
    blocks <- sort(unique(block_var))
    prob_mat <- matrix(NA, nrow = length(block_var), ncol = length(unique(Z)))
    
    if(is.null(block_m) & is.null(block_prob) & is.null(num_arms)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        prob_mat[block_var==blocks[i],] <- design_probs(N = N_block, condition_names=condition_names, design_type="complete")
      }
      colnames(prob_mat) <- colnames(design_probs(N = N_block, condition_names=condition_names, design_type="complete"))
      return(prob_mat)
    }
    
    if(is.null(block_m) & is.null(block_prob) & !is.null(num_arms)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        prob_mat[block_var==blocks[i],] <- design_probs(N = N_block, num_arms=num_arms, condition_names=condition_names, design_type="complete")
      }
      colnames(prob_mat) <- colnames(design_probs(N = N_block, num_arms=num_arms, condition_names=condition_names, design_type="complete"))
      return(prob_mat)
    }
    
    if(!is.null(block_m)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        prob_mat[block_var==blocks[i],] <- design_probs(N = N_block, m_each = block_m[i,], condition_names=condition_names, design_type="complete")
      }
      colnames(prob_mat) <- colnames(design_probs(N = N_block, m_each = block_m[i,], condition_names=condition_names, design_type="complete"))
      return(prob_mat)
    }
    
    if(!is.null(block_prob)){
      
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        prob_mat[block_var==blocks[i],] <- design_probs(N = N_block, prob_each = block_prob, condition_names=condition_names, design_type="complete")
      }
      colnames(prob_mat) <- colnames(design_probs(N = N_block, prob_each = block_prob, condition_names=condition_names, design_type="complete"))
      return(prob_mat)
    }
    
  }
  
  if(design=="clustered"){`
    unique_clus <- unique(clust_var)
    n_clus <- length(unique_clus)
    probs_clus <- design_probs(N = n_clus, m = m, num_arms = num_arms, m_each = m_each, 
                               condition_names = condition_names, design_type = "complete")
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
    merged <- merged[order(merged$init_order),]
    probs_mat <- as.matrix(merged[,colnames(probs_clus)])
    return(probs_mat)
  }
  
  if(design=="blocked and clustered"){`
    unique_clus <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clust))
    for(i in 1:length(unique_clust)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clust[i]])  
    }
    probs_clus <- design_probs(num_arms = num_arms, m_each = m_each, 
                               prob_each = prob_each, block_var = clust_blocks,
                               condition_names = condition_names, design_type = "blocked")
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    data.frame(clust_var=unique_clus, probs_clus), by="clust_var")
    merged <- merged[order(merged$init_order),]
    probs_mat <- as.matrix(merged[,colnames(probs_clus)])
    return(probs_mat)
  }
}

#' @export
observed_probs <- function(treatment_assignment, design, data){
  prob_mat <- get_design_probs(design = design, data = data)
  prob_obs <- rep(NA, nrow(data))
  condition_names <- unique(treatment_assignment)
  for(v in condition_names){
    prob_obs[treatment_assignment==v] <- prob_mat[treatment_assignment==v, v]
  }
  return(prob_obs)  
}
  
#' @export
make_permutation_matrix <- function(design, data, sims=100){
  permutation_matrix <- replicate(n = sims, expr = assign_treatment(design = design, data = data))
  return(permutation_matrix)
}



