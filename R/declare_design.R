
# random assignment functions

#' @export
complete_ra <- 
  function(N, m = NULL, m_each = NULL, prob_each = NULL, condition_names = NULL) {
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
    
    
    num_arms <- length(condition_names)
    
    if(is.null(m_each) & is.null(prob_each) & num_arms ==2) {
      if(is.null(m)) {
        coin_flip <- rbinom(1, 1, 0.5)
        if (coin_flip == 0) 
          m <- floor(N/2)
        if (coin_flip == 1) 
          m <- ceiling(N/2)
      }
      if (m >= N) {
        stop("The number of units assigned to treatment (m) must be smaller than the total number of units (N)")
      }
      assign <- ifelse(1:N %in% sample(1:N, m), condition_names[2], condition_names[1])
      warning("Assuming that the second condition in condition_names is the treatment condition.")
      return(assign)
    }
    if(all(!is.null(m_each), sum(m_each) != N)) {
      stop("The sum of number assigned to each condition (m_each) must equal the total number of units (N)")
    }
    if (!is.null(prob_each)) {
      if (sum(prob_each) != 1) {
        stop("If specified, the sum of prob_each must equal 1")
      }
      m_each <- floor(N * prob_each)
      remainder <- N - sum(m_each)
      m_each <- m_each + ifelse(1:length(prob_each) %in% sample(1:length(prob_each), remainder), 1, 0)
    }
    if (is.null(m_each)) {
      m_each <- rep(N%/%num_arms, num_arms)
      remainder <- N%%num_arms
      m_each <- m_each + ifelse(1:num_arms %in% sample(1:num_arms, remainder), 1, 0)
    }
    if (N < num_arms) {
      assign <- sample(condition_names, N, replace = FALSE)
      return(assign)
    }
    rand_order <- sample(1:N, replace = FALSE)
    assign <- rep(NA, N)
    for (i in 1:num_arms) {
      assign[rand_order[(sum(m_each[0:(i - 1)]) + 1):sum(m_each[0:i])]] <- condition_names[i]
    }
    return(assign)
  }
#' @export
block_ra <- 
  function(block_var, block_m=NULL, prob_each = NULL,condition_names = NULL){
    
    if(!is.null(block_m) & !is.null(prob_each)){
      stop("Do not specify both block_m and prob_each at the same time.")      
    }
    
    blocks <- sort(unique(block_var))
    assign <- rep(NA, length(block_var))
    
    if(is.null(block_m) & is.null(prob_each)){
      for(i in 1:length(blocks)){
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, condition_names=condition_names)
      }
      return(assign)
    }
    
    if(!is.null(block_m)){
      for(i in 1:length(blocks)){
        if(nrow(block_m)!=length(unique(blocks))){
          stop("block_m should have the same number of rows as there are unique blocks in block_var")
        }
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, m_each = block_m[i,], condition_names=condition_names)
      }
      return(assign)
    }
    
    if(!is.null(prob_each)){
      
      for(i in 1:length(blocks)){
        if(sum(prob_each)!=1){
          stop("prob_each must sum to 1.")
        }
        N_block <- sum(block_var==blocks[i])
        assign[block_var==blocks[i]] <- complete_ra(N = N_block, prob_each = prob_each, condition_names=condition_names)
      }
      return(assign)
    }
  }

#' @export
cluster_ra <- function(clust_var, m=NULL, m_each = NULL, prob_each = NULL, condition_names = NULL){
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  z_clus <- complete_ra(N = n_clus, 
                        m = m,
                        m_each = m_each, 
                        prob_each = prob_each,
                        condition_names = condition_names)
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  y = data.frame(clust_var=unique_clus, z_clus, stringsAsFactors=FALSE), by="clust_var")
  merged <- merged[order(merged$init_order),]
  return(merged$z_clus)
}

#' @export
blocked_and_clustered_ra <- 
  function(clust_var, block_var, block_m=NULL, prob_each=NULL, condition_names = NULL) {
    
    # confirm that all units within clusters are in the same block
    if(!all(rowSums(table(clust_var, block_var) != 0)==1)){
      stop("All units within a cluster must be in the same block.")
    }
    
    unique_clust <- unique(clust_var)
    
    ## get the block for each cluster
    clust_blocks <- rep(NA, length(unique_clust))
    for(i in 1:length(unique_clust)){
      clust_blocks[i] <- unique(block_var[clust_var==unique_clust[i]])  
    }
    
    z_clust <- block_ra(block_var = clust_blocks, 
                        block_m = block_m, 
                        prob_each = prob_each,
                        condition_names = condition_names)
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    y = data.frame(clust_var=unique_clust, z_clust), by="clust_var")
    merged <- merged[order(merged$init_order),]
    return(as.character(merged$z_clust))
  }

#' @export
declare_design <- 
  function(potential_outcomes, 
           blocks = NULL, 
           clusters = NULL,
           m = NULL, 
           m_each = NULL, 
           prob_each = NULL, 
           block_m = NULL, 
           excluded_arms = NULL) {
    
    design_type <- "complete"   
    if(!is.null(blocks)) {design_type <- "blocked"}
    if(!is.null(clusters)) {design_type <- "clustered"}
    if(!is.null(clusters) & !is.null(blocks)) {
      design_type <- "blocked and clustered"
    }
    
    if(all(sapply(potential_outcomes, class)=="outcomes_object")){
      condition_names <- 
        unique(unlist(lapply(X = potential_outcomes, FUN = function(po){po$condition_names})))
    }else{
      condition_names <- potential_outcomes$condition_names
    }
    
    if(!is.null(excluded_arms)){
      condition_names <- condition_names[!condition_names %in% excluded_arms]  
    }
    
    if(is.null(blocks)){block_name=NULL}
    if(is.null(clusters)){cluster_name=NULL}
    
    block_name <- blocks$block_name
    cluster_name <- clusters$cluster_name
    
    return.object <- list(block_name = block_name,
                          cluster_name = cluster_name,
                          condition_names = condition_names,
                          m = m,
                          m_each = m_each,
                          prob_each = prob_each,
                          block_m = block_m,
                          design_type = design_type,
                          call = match.call())
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
