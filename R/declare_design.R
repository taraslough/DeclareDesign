
# random assignment functions

#' @export
complete_ra <- 
  function(N, m = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL, baseline_condition=NULL) {
    
    # Checks
    
    if(!is.null(prob_each) & !is.null(m_each)) {
      stop("Do not specify prob_each and m_each together. Use one or the other.")
    }
    if(!is.null(prob_each) & !is.null(m)) {
      stop("Do not specify prob_each and m together. Use one or the other.")
    }
    if(!is.null(m_each) & !is.null(m)) {
      stop("Do not specify m_each and m together. Use one or the other.")
    }
    if(!is.null(m) & length(condition_names) > 2) {
      stop("Do not specify m when there are more than 2 conditions. Use m_each instead.")
    }
    if(!is.null(m_each) & length(m_each) != length(condition_names)){
      stop("The length of m_each must match the length of condition names. Either exclude some conditions with the excluded_arms argument or add some conditions in declare_potential_outcomes.")
    }
    if(!is.null(prob_each) & length(prob_each) != length(condition_names)){
      stop("The length of prob_each must match the length of condition names. Either exclude some conditions with the excluded_arms argument or add some conditions in declare_potential_outcomes.")
    }
    if(all(!is.null(m_each), sum(m_each) != N)) {
      stop("The sum of number assigned to each condition (m_each) must equal the total number of units (N)")
    }
    
    # Setup: obtain number of arms
    num_arms <- length(condition_names)
    
    # Case 0: If there's only one unit and two arms, flip a coin
    if(is.null(m_each) & is.null(prob_each) & num_arms ==2 & N ==1) {
      assign <- sample(condition_names, N, replace = FALSE)
      return(assign)
    }
    if (N < num_arms) {
      assign <- sample(condition_names, N, replace = FALSE)
      return(assign)
    }
    
    # Case 1: If there are two arms, and nothing else is specified, get as close to 50/50 as possible
    if(is.null(m_each) & is.null(prob_each) & num_arms ==2 & N >1) {
      if(is.null(m)) {
        coin_flip <- rbinom(1, 1, 0.5)
        if (coin_flip == 0) 
          m <- floor(N/2)
        if (coin_flip == 1) 
          m <- ceiling(N/2)
      }
      if (m >= N) {
        stop("The number of units assigned to treatment (m) must be less than the total number of units (N)")
      }
      assign <- ifelse(1:N %in% sample(1:N, m),  condition_names[condition_names!=baseline_condition], baseline_condition)
      return(assign)
    }

    # Case 2: using m_each (or determining m_each from prob_each)
        
    # Figure out m_each
    if (!is.null(prob_each)) {
      if (sum(prob_each) != 1) {
        stop("If specified, the sum of prob_each must equal 1")
      }
      m_each <- floor(N * prob_each)
      remainder <- N - sum(m_each)
      m_each <- m_each + ifelse(1:length(prob_each) %in% sample(1:length(prob_each), remainder), 1, 0)
    }
    
    # Correct m_each if there is a remainder
    if (is.null(m_each)) {
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <- N%%num_arms
      m_each <- m_each + ifelse(1:num_arms %in% sample(1:num_arms, remainder), 1, 0)
    }
    
    # Conduct complete_ra with multiple arms
    rand_order <- sample(1:N, replace = FALSE)
    assign <- rep(NA, N)
    for (i in 1:num_arms) {
      assign[rand_order[(sum(m_each[0:(i - 1)]) + 1):sum(m_each[0:i])]] <- condition_names[i]
    }
    return(assign)
  }
#' @export
block_ra <- 
  function(block_var, block_m=NULL, block_prob = NULL, prob_each = NULL, condition_names = NULL, baseline_condition=NULL){
    
    # Checks
    
    if(!is.null(block_m) & !is.null(prob_each)){
      stop("Do not specify both block_m and prob_each at the same time.")      
    }
    
    if(!is.null(block_m) & !is.null(block_prob)){
      stop("Do not specify both block_m and block_prob at the same time.")      
    }
    
    if(!is.null(prob_each) & !is.null(block_prob)){
      stop("Do not specify both prob_each and block_prob at the same time.")      
    }
    
    # Setup (obtain unique blocks and create assignment vector)
    
    blocks <- sort(unique(block_var))
    assign <- rep(NA, length(block_var))
    
    # Case 1: Assumes equal probabilties for each condition in all block
    # Does complete_ra() by block
    
    if(is.null(block_m) & is.null(prob_each) & is.null(block_prob)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- 
          complete_ra(N = N_block, 
                      condition_names=condition_names, 
                      baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 2: User specifies exactly how many units will be assigned to each condition, by block
    
    if(!is.null(block_m)){
      for(i in 1:length(blocks)){
        if(nrow(block_m)!=length(unique(blocks))){
          stop("block_m should have the same number of rows as there are unique blocks in block_var")
        }
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, 
                                                    m_each = block_m[i,], 
                                                    condition_names=condition_names, 
                                                    baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 3: User specifies the probability of assignment to each condition, but it doesn't vary by block
    
    if(!is.null(prob_each)){
      for(i in 1:length(blocks)){
        if(sum(prob_each)!=1){
          stop("prob_each must sum to 1.")
        }
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, 
                                                    prob_each = prob_each, 
                                                    condition_names=condition_names, 
                                                    baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
    # Case 4: User specifies the probability of assignment to each condition, but it doesn't vary by block
    
    if(!is.null(block_prob)){
      for(i in 1:length(blocks)){
        prob_each_local <- block_prob[i,]
        if(sum(prob_each_local)!=1){
          stop("Each row of block_prob must sum to 1.")
        }
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, 
                                                    prob_each = prob_each_local, 
                                                    condition_names=condition_names, 
                                                    baseline_condition = baseline_condition)
      }
      return(assign)
    }
    
  }

#' @export
cluster_ra <- function(clust_var, m=NULL, m_each = NULL, prob_each = NULL, condition_names = NULL, baseline_condition=NULL){
  
  # Setup: get unique clusters and the number of clusters
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  
  # Conduct assignment at the cluster level
  z_clus <- complete_ra(N = n_clus, 
                        m = m,
                        m_each = m_each, 
                        prob_each = prob_each,
                        condition_names = condition_names,
                        baseline_condition = baseline_condition)
  
  # Merge back up to the individual level, maintaining original ordering
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  y = data.frame(clust_var=unique_clus, z_clus, stringsAsFactors=FALSE), by="clust_var")
  merged <- merged[order(merged$init_order),]
  return(merged$z_clus)
}

#' @export
blocked_and_clustered_ra <- 
  function(clust_var, block_var, block_m=NULL, prob_each=NULL, block_prob=NULL,condition_names = NULL, baseline_condition=NULL) {
    
    # confirm that all units within clusters are in the same block
    # is there a computationally faster way to confirm this (possible c++ loop?)
    
    if(!all(rowSums(table(clust_var, block_var) != 0)==1)){
      stop("All units within a cluster must be in the same block.")
    }
    
    # Setup: obtain unique clusters
    unique_clust <- unique(clust_var)
    
    # get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clust))
    for(i in 1:length(unique_clust)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clust[i]])  
    }
    
    # Conduct random assignment at cluster level
    z_clust <- block_ra(block_var = clust_blocks, 
                        block_m = block_m, 
                        prob_each = prob_each,
                        block_prob = block_prob,
                        condition_names = condition_names, 
                        baseline_condition = baseline_condition)
    
    # Merge back up to the individual level, maintaining original ordering
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    y = data.frame(clust_var=unique_clust, z_clust), by="clust_var")
    merged <- merged[order(merged$init_order),]
    return(as.character(merged$z_clust))
  }

#' Declare the experimental design
#'
#' @param potential_outcomes potential_outcomes object, as created by \code{\link{declare_potential_outcomes}} (required).
#' @param blocks blocks object, as created by \code{\link{declare_blocks}} (optional).
#' @param clusters clusters object, as created by \code{\link{declare_clusters}} (optional).
#' @param m the number of units (or clusters) to be assigned to treatment in a two-arm trial.
#' @param m_each a vector describing the number of units (or clusters) to be assigned to each treatment arm in a multi-arm trial.  Must sum to N (for individually randomized experments) or N_clusters (for cluster randomized experiments).
#' @param prob_each a vector describing the probability of units (or clusters) being assigned to each treatment arm. Must sum to 1.
#' @param block_m a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the number of units (or clusters) to be assigned to each treatment arm.
#' @param block_prob a matrix with the same number of rows as blocks and the same number of columns as treatment arms. Cell entries are the probabilities of assignment to each treatment arm.
#' @param excluded_arms a character vector excluding some potential outcomes from the randomization.  Used primarily when comparing designs that feature different numbers of treatment arms.
#' @return design object
#' @export
declare_design <- 
  function(potential_outcomes, 
           blocks = NULL, 
           clusters = NULL,
           m = NULL, 
           m_each = NULL, 
           prob_each = NULL, 
           block_m = NULL, 
           block_prob = NULL,
           excluded_arms = NULL,
           baseline_condition = NULL,
           treatment_variable = "Z",
           custom_assignment_function = NULL) {
    
    # Determine design type
    design_type <- "complete"   
    if(!is.null(blocks)) {design_type <- "blocked"}
    if(!is.null(clusters)) {design_type <- "clustered"}
    if(!is.null(clusters) & !is.null(blocks)) {
      design_type <- "blocked and clustered"
    }
    
    # Obtain Condition Names
    if(all(sapply(potential_outcomes, class)=="potential_outcomes")){
      condition_names <- 
        unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$condition_names})))
    }else{
      condition_names <- potential_outcomes$condition_names
    }
    
    # If necessary, exlude some arms
    if(!is.null(excluded_arms)){
      condition_names <- condition_names[!condition_names %in% excluded_arms]  
    }
    
    # Figure out baseline condition
    if(!is.null(baseline_condition)){
      if(!(baseline_condition %in% condition_names))
        stop("The baseline condition must match one of the conditions specified in declare_potential_outcomes().")
    }
    if(is.null(baseline_condition)){
      baseline_condition <- condition_names[1]
    }
    
    # Figure out blocks
    if(!is.null(blocks) & is.character(blocks)){
      blocks <- declare_blocks(blocks = blocks, recode = FALSE)
    }
    
    if(!is.null(blocks) & !is.character(blocks)){
      block_name <- blocks$block_name
    }

    if(is.null(blocks)){block_name=NULL}
    
    # Figure out cluster name
    if(!is.null(clusters) & is.character(clusters)){
      clusters_internal <- clusters
      clusters <- declare_clusters(clusters = clusters_internal)
    }  
    
    if(!is.null(clusters) & !is.character(clusters)){
      cluster_name <- clusters$cluster_name
    }

    if(is.null(clusters)){cluster_name=NULL}
    
    if(is.null(custom_assignment_function)){
      return.object <- list(block_name = block_name,
                            cluster_name = cluster_name,
                            condition_names = condition_names,
                            m = m,
                            m_each = m_each,
                            prob_each = prob_each,
                            block_m = block_m,
                            block_prob = block_prob,
                            design_type = design_type,
                            blocks = blocks,
                            clusters = clusters,
                            baseline_condition = baseline_condition,
                            treatment_variable = treatment_variable,
                            call = match.call())
    } else {
      return.object <- list(
        custom_assignment_function = custom_assignment_function,
        condition_names = condition_names,
        baseline_condition = baseline_condition,
        treatment_variable = treatment_variable,
        call = match.call())
    }
    class(return.object) <- "design"
    return(return.object)
  }


#' @export
summary.design <- function(object, ...) {
  ## this function itself does nothing, it's just an R package technicality
  ## so that print.summary.design() works
  structure(object, class = c("summary.design", class(object)))
}

#' @export
print.summary.design <- function(x, ...){
  ## prints paragraph describing design
  cat(ifelse(x$design_type == "blocked", paste("This experiment employs a block-randomized design."), ""),
      ifelse(x$design_type == "clustered", paste("This experiment employs a cluster-randomized design."), ""),
      ifelse(x$design_type == "blocked and clustered", paste("This experiment employs a block-and-cluster-randomized design."), ""),
      ifelse(x$design_type == "complete", "This experiment employs a completely-randomized design.", "")
  )
  cat(" The possible treatment categories are ", paste(x$condition_names, collapse = " and "), ".", sep = "")
}

#' @export
treatment_indicator_name <- function(x) {
  return("NOT SURE YET")
}
