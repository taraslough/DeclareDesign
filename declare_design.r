rm(list=ls())


# random assignment functions

complete_ra <- 
  function(N, m = NULL, num_arms = NULL, m_each = NULL, prob_each = NULL, 
                        condition_names = NULL) {
                          if (!is.null(m) & !is.null(condition_names)) {
                            stop("Do not specify m and condition_names together. Use m_each and condition_names instead.")
                          }
                          if (!is.null(prob_each) & !is.null(m_each)) {
                            stop("Do not specify prob_each and m_each together. Use one or the other.")
                          }
                          if (is.null(m_each) & is.null(condition_names) & is.null(num_arms) & 
                              is.null(prob_each)) {
                                if (is.null(m)) {
                                  coin_flip <- rbinom(1, 1, 0.5)
                                  if (coin_flip == 0) 
                                    m <- floor(N/2)
                                  if (coin_flip == 1) 
                                    m <- ceiling(N/2)
                                }
                                if (m >= N) {
                                  stop("The number of units assigned to treatment (m) must be smaller than the total number of units (N)")
                                }
                                assign <- ifelse(1:N %in% sample(1:N, m), 1, 0)
                                return(assign)
                              }
                          if (all(!is.null(m_each), sum(m_each) != N)) {
                            stop("The sum of number assigned to each condition (m_each) must equal the total number of units (N)")
                          }
                          if (all(!is.null(condition_names), !is.null(m_each), length(m_each) != 
                                  length(condition_names))) {
                                    stop("The length of conditions_names must equal the length of m_each")
                                  }
                          if (all(!is.null(condition_names), !is.null(prob_each), length(prob_each) != 
                                  length(condition_names))) {
                                    stop("The length of conditions_names must equal the length of prob_each")
                                  }
                          if (all(!is.null(m_each), !is.null(num_arms), length(m_each) != 
                                  num_arms)) {
                                    stop("The number of arms (n_arms) must equal the length of m_each")
                                  }
                          if (all(!is.null(prob_each), !is.null(num_arms), length(prob_each) != 
                                  num_arms)) {
                                    stop("The number of arms (n_arms) must equal the length of prob_each")
                                  }
                          if (all(!is.null(condition_names), !is.null(num_arms), length(condition_names) != 
                                  num_arms)) {
                                    stop("The length of conditions_names must equal the number of arms (n_arms)")
                                  }
                          if (!is.null(prob_each)) {
                            if (sum(prob_each) != 1) {
                              stop("If specified, the sum of prob_each must equal 1")
                            }
                            m_each <- floor(N * prob_each)
                            remainder <- N - sum(m_each)
                            m_each <- m_each + complete_ra(N = length(prob_each), 
                                                           m = remainder)
                          }
                          if (is.null(m_each)) {
                            if (is.null(num_arms)) {
                              num_arms <- length(condition_names)
                            }
                            m_each <- rep(N%/%num_arms, num_arms)
                            remainder <- N%%num_arms
                            m_each <- m_each + ifelse(1:num_arms %in% sample(1:num_arms, 
                                                                             remainder), 1, 0)
                          }
                          if (is.null(num_arms)) {
                            num_arms <- length(m_each)
                          }
                          if (is.null(condition_names)) {
                            condition_names <- paste0("T", 1:num_arms)
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
block_ra <- 
  function(block_var, num_arms= NULL, block_m=NULL, block_prob=NULL, condition_names = NULL){
  
  if(!is.null(block_m) & !is.null(block_prob)){
    stop("Do not specify both block_m and block_prob at the same time.")      
  }
  
  blocks <- sort(unique(block_var))
  assign <- rep(NA, length(block_var))
  
  if(is.null(block_m) & is.null(block_prob) & is.null(num_arms)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, condition_names=condition_names)
    }
    return(assign)
  }
  
  if(is.null(block_m) & is.null(block_prob) & !is.null(num_arms)){
    for(i in 1:length(blocks)){
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, num_arms=num_arms, condition_names=condition_names)
    }
    return(assign)
  }
  
  if(all(!is.null(num_arms), !is.null(block_prob), num_arms != length(block_prob))){
    stop("If both num_arms and block_prob are specified, num_arms must be equal to the length of block_prob")
  }
  
  if(all(!is.null(num_arms), !is.null(block_m), num_arms != ncol(block_m))){
    stop("If both num_arms and block_m are specified, num_arms must be equal to the number of columns of block_m")
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
  
  if(!is.null(block_prob)){
    
    for(i in 1:length(blocks)){
      if(sum(block_prob)!=1){
        stop("block_prob must sum to 1.")
      }
      N_block <- sum(block_var==blocks[i])
      assign[block_var==blocks[i]] <- complete_ra(N = N_block, prob_each = block_prob, condition_names=condition_names)
    }
    return(assign)
  }
}

cluster_ra <- function(clust_var, m=NULL, num_arms=NULL, m_each = NULL, condition_names = NULL){
  unique_clus <- unique(clust_var)
  n_clus <- length(unique_clus)
  z_clus <- complete_ra(N = n_clus, m = m, num_arms = num_arms, m_each = m_each, 
                        condition_names = condition_names)
  merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                  y = data.frame(clust_var=unique_clus, z_clus, stringsAsFactors=FALSE), by="clust_var")
  merged <- merged[order(merged$init_order),]
  return(merged$z_clus)
}

block_var <- rep(1:10, each=10)
clust_var <- rep(letters[1:20], each=5)

blocked_and_clustered_ra <- 
  function(clust_var, block_var, num_arms= NULL, block_m=NULL, block_prob=NULL, condition_names = NULL) {
    
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
    
    z_clust <- block_ra(block_var = clust_blocks, block_m = block_m, block_prob = block_prob, num_arms = num_arms, condition_names = condition_names)
    merged <- merge(x = data.frame(clust_var, init_order = 1:length(clust_var)), 
                    y = data.frame(clust_var=unique_clust, z_clust), by="clust_var")
    merged <- merged[order(merged$init_order),]
    return(as.character(merged$z_clust))
  }

declare_design <- 
  function(N = NULL, m = NULL, 
           m_each = NULL, prob_each = NULL, 
           block_var = NULL, block_m = NULL, 
           block_prob = NULL,
           clust_var = NULL, 
           num_arms = NULL, condition_names = NULL) {
          
          design <- "complete"   
          if(!is.null(block_var)) {design <- "blocked"}
          if(!is.null(clust_var)) {design <- "clustered"}
          if(!is.null(clust_var) & !is.null(block_var)) {design <- "blocked and clustered"}
          
          if(design=="complete"){
            ra_fun <- function() {
              complete_ra(N=N, m=m, num_arms = num_arms, m_each = m_each,
                            prob_each = prob_each, condition_names = condition_names)
            }
            Z <- ra_fun()
            N_2 <- length(Z)
            N_blocks <- "Not a blocked design"
            N_clus <- "Not a clustered design"
            
            condition_names_2 <- unique(Z)
            num_arms_2 <- length(unique(Z))
            probs_mat <- "to be added"
          }
          
          if(design=="blocked"){
            ra_fun <- function() {
              block_ra(block_var=block_var, num_arms = num_arms, block_m = block_m,
                                  block_prob = block_prob, condition_names = condition_names)
            }
            Z <- ra_fun()
            N_2 <- length(Z)
            N_blocks <- length(unique(block_var))
            N_clus <- "Not a clustered design"
            condition_names_2 <- unique(Z)
            num_arms_2 <- length(unique(Z))
            probs_mat <- "to be added"
          }
          
          if(design=="clustered"){
            ra_fun <- function() {
              cluster_ra(clust_var=clust_var, m = m, num_arms = num_arms, m_each = m_each,
                               condition_names = condition_names)
            }
            Z <- ra_fun()
            N_2 <- length(Z)
            N_blocks <- "Not a blocked design"
            N_clus <- length(unique(clust_var))
            condition_names_2 <- unique(Z)
            num_arms_2 <- length(unique(Z))
            probs_mat <- "to be added"
          }
          
          if(design=="blocked and clustered"){
            ra_fun <- function() {
                blocked_and_clustered_ra(clust_var=clust_var,block_var=block_var, block_m=block_m, num_arms=num_arms,
                       block_prob = block_prob, condition_names = condition_names)
            }
            Z <- ra_fun()
            N_2 <- length(Z)
            N_blocks <- length(unique(block_var))
            N_clus <- length(unique(clust_var))
            condition_names_2 <- unique(Z)
            num_arms_2 <- length(unique(Z))
            probs_mat <- "to be added"
          }
          return(list(ra_fun=ra_fun, N=N_2, N_blocks = N_blocks, N_clus = N_clus, 
                      condition_names=sort(condition_names_2), 
                      num_arms=num_arms_2, probs_mat=probs_mat,
                      call = match.call()))
 }


# Examples
design_1 <- declare_design(N = 100, m=50)
design_1
design_1$ra_fun()

design_2 <- declare_design(N = 100, m_each = c(30, 40, 30), 
                           condition_names=c("control", "placebo", "treatment"))
design_2
design_2$ra_fun()

block_var <- rep(c("A", "B","C"), times=c(50, 100, 200))
design_3 <- declare_design(block_var = block_var)
design_3
design_3$ra_fun()

block_m <- rbind(c(10, 20, 20),
                 c(30, 50, 20),
                 c(50, 75, 75))

design_4 <- declare_design(block_var = block_var, block_m = block_m)
design_4
design_4$ra_fun()


clust_var <- rep(letters, times=1:26)
design_5 <- declare_design(clust_var = clust_var)
design_5
design_5$ra_fun()

design_6 <- declare_design(clust_var=clust_var, m_each=c(7, 7, 12),
                           condition_names=c("control", "placebo", "treatment"))
design_6
design_6$ra_fun()


cluster_ra(clust_var=clust_var, m_each=c(7, 7, 12),
           condition_names=c("control", "placebo", "treatment"))

clust_var <- rep(letters, times=1:26)
block_var <- rep(rep(1:13, each=2), times=1:26)
design_7 <- declare_design(clust_var=clust_var, block_var = block_var, num_arms=4)
design_7
design_7$ra_fun()
