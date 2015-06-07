#' Create the potential outcomes of the experiment
#'
#' @param design_object 
#' @return Filename and location where .Rmd or .Rnw and PDF file are saved.
#' @importFrom rmarkdown render
#' @export

make_potential_outcomes <- 
  function(
    covariate_object,
    design_object,
    outcome_formula,
    outcome_DGP = declare_DGP(),
    cluster_variable = NULL,
    N_per_cluster = NULL,
    ICC = .01,
    unit_variance = 1
  ){
      
      
      
      # Check whether covariate_object is covarite_object or a user-supplied matrix
      if(class(covariate_object)=="covariate_object"){
        X <- covariate_object$make_X_matrix()
      }
      if(class(covariate_object)%in%c("matrix","data.frame")){
        X <- covariate_object
      }
      
      # Check that all of the variables in the formula are in the X matrix or in the treatment names
      # Check that the baseline is nested in the treatment formula
      if(
        FALSE %in% (all.vars(outcome_formula)[-1] %in% c(names(X),design_object$condition_names))
                     )stop("All of the variables in the formula should either be in the covariate matrix or in the condition_names of the design_object.")

      treat_mat    <- diag(length(design_object$condition_names))
      colnames(treat_mat) <- design_object$condition_names
      
      
      # Make a function that generates potential outcomes as a function of 
      # all of the variables (treatment assignment, covariates) and some normal noise
      gen_outcome  <- eval(parse(text = paste0(
        "function(slice){y <- with(slice,{",outcome_formula[3],"}) + rnorm(1,0,residual.variance);return(y)}"
      )))
      
      
      
      
      # Make another function that applies the gen.outcome function across 
      # the treatment condition indicators, generating a vector of outcomes for 
      # each observation, conditional on assignment
      each_treat   <- function(cov_slice){apply(
        X      = treat_mat,
        MARGIN = 1,
        FUN    = function(treat_mat_slice){
          gen_outcome(slice = as.data.frame(t(unlist(c(
            treat_mat_slice,cov_slice)
          ))))
        }
      )}
      
      # Apply that function through the covariate matrix to get the potential outcomes
      outcomes     <- data.frame(t(apply(
        X      = X,
        MARGIN = 1,
        FUN    = each_treat
      )))
      
      # Give outcomes user-defined names 
      names(outcomes) <- paste0(as.character(treatment.formula)[2],".",treat.names)
      
      
      # Put in all of the logical checks here, i.e. did they supply a cluster 
      # variable that's an integer, etc. 
      
      if(is.null(N_per_cluster)&!is.character(design_object$N_clus)){
        
        
        N_per_cluster <- rep(design_object$N%/%design_object$N_clus, design_object$N_clus)
        remainder <- design_object$N%%design_object$N_clus
        N_per_cluster <- N_per_cluster + ifelse(1:design_object$N_clus %in% 
                                                  sample(1:design_object$N_clus, 
                                                         remainder), 1, 0)
        clust_var <- sample(rep(1:design_object$N_clus,N_per_cluster))
      }
      
      if(!is.character(design_object$N_clus)){
        X$cluster <- clust_var
        
        
        cluster_variance <- ICC*unit_variance/(1-ICC)
        
        clusters <-  1:10
        clustershock = rnorm(length(X$cluster), sd = cluster_variance^.5)
        
        cluster = sample(clusters, n, replace=TRUE)
        
        e <- clustershock[cluster] + rnorm(n, sd=unitvar^.5)
        
        data = data.frame(e, cluster) 
        
        icc <- clustervar/(clustervar+unit_variance)
        
        
      }
      
      # Check what the DGP of the outcome variable is and do necessary transformations
      if()
      
      
      
    }


model.matrix(


# Spare code --------------------------------------------------------------

covariate_object$make_X_matrix()
covariate_object <- make_covariates(
  X1 = declare_DGP(),
  X2 = declare_DGP(),
  event = declare_DGP(binary_probability = .5,
                      binary_categories = c("happened","did not")),
  income = function()rnorm(n = design_object$N,mean = 0,sd = 1),
  count = function()rpois(n = design_object$N,lambda = 30),
#   party_id = declare_DGP(
#     multinomial_probabilities = c(.4,.4,.2),
#     multinomial_categories = c("D","R","I")),
  design_object = design_object
)


outcome_formula <-  y ~ 0*control + .05*placebo + .4*treatment + .2*X1  + .2*X2  + 
  .2*(as.character(event)=="happened")  + 
  .2*income  + .2*count  




make_data <- function(
  treatment.formula,       
  baseline.formula = NULL,  
  covariate.matrix = NULL, 
  outcome.vector = NULL,
  residual.variance = 1
  # ... I will add in the other args later
  
){
    
    # Check if it's case 1:
    
    if(!is.null(covariate.matrix)&
       is.null(outcome.vector)
    ){
         
         # Check that the baseline is nested in the treatment formula
         if(!grepl(pattern = as.character(baseline.formula)[2],
                   x       = as.character(treatment.formula)[3]))
           stop("Treatment formula should contain outcome of baseline formula.")
         
         # Check that the variable names in the baseline formula match those in the
         # covariate matrix
         if(FALSE%in%(all.vars(baseline.formula)[-1]%in%colnames(covariate.matrix)))
           stop("Variables in baseline formula should match the column names in the covariate matrix.")
         
         # Get the names of the treatment, removing any of the baseline variables
         treat.names  <- all.vars(treatment.formula
         )[!all.vars(treatment.formula)%in%all.vars(baseline.formula)][-1]
         
         # Get the number of treatments 
         num.treats   <- length(treat.names)
         
         # Get an indicator matrix for the treatments 
         treat.mat    <- diag(num.treats)
         colnames(treat.mat) <- treat.names
         
         # Get the full formula for the potential outcomes
         full.formula <- 
           gsub(pattern     = as.character(baseline.formula)[2],
                replacement = as.character(baseline.formula)[3],
                x           = as.character(treatment.formula)[3])
         
         # Make a function that generates potential outcomes as a function of 
         # all of the variables (treatment assignment, covariates) and some normal noise
         gen.outcome  <- eval(parse(text = paste0(
           "function(slice){y <- with(slice,{",full.formula,"}) + rnorm(1,0,residual.variance);return(y)}"
         )))
         
         # Make another function that applies the gen.outcome function across 
         # the treatment condition indicators, generating a vector of outcomes for 
         # each observation, conditional on assignment
         each.treat   <- function(cov.slice){apply(
           X      = treat.mat,
           MARGIN = 1,
           FUN    = function(treat.mat.slice){
             gen.outcome(slice = as.data.frame(t(unlist(c(
               treat.mat.slice,cov.slice)
             ))))
           }
         )}
         
         # Apply that function through the covariate matrix to get the potential outcomes
         outcomes     <- data.frame(t(apply(
           X      = covariate.matrix,
           MARGIN = 1,
           FUN    = each.treat
         )))
         
         # Give outcomes user-defined names 
         names(outcomes) <- paste0(as.character(treatment.formula)[2],".",treat.names)
         
         
         data         <- data.frame(outcomes,covariate.matrix)
         
         
         return(data)
         
         
       } # end of case 3
    
    
  } 



X$X1 <- as.numeric(X$X1)
X$X2 <- as.numeric(X$X2)
X$income <- as.numeric(X$income)
X$count <- as.numeric(X$count)

data.frame


design_object <- design_6
design_object <- design_1



outcome_DGP = declare_DGP(variable_name = "Y")
outcome_formula = Y ~ .1*X1 + .2*X2 + .3*X3,
covariate_DGP = ,
N = design_object$N,
N_per_block = NULL,
clusters_per_block = NULL,


N_per_block = N/design_object$N_blocks,
clusters_per_block = N_per_block,




if(grepl(pattern = "Not",x = design_object$N_clus))
  
  make.data.frame <- function(
    n=100,
    n.obs.each.block = n,
    n.cluster.each.block= n.obs.each.block){
      
      
      if(sum((n.obs.each.block/n.cluster.each.block)%%1)>0){stop("check data structure for integer issues")}  
      if(sum(n.cluster.each.block%%1)>0){stop("check data structure for integer issues")}
      if( n!=sum(n.obs.each.block)){stop("sum of n.obs.per.block != n")}
      
      n.obs.per.cluster.per.block <- n.obs.each.block/n.cluster.each.block
      
      n.blocks <- length(n.obs.each.block)
      blocks   <- as.vector(unlist(sapply(1:n.blocks, function(j) rep(j, n.obs.each.block[j]))))
      clusters <- as.vector(unlist(sapply(1:n.blocks, function(j) sapply(1:n.cluster.each.block[j] , function(k) rep(k, n.obs.per.cluster.per.block[j])))))
      clusters<- blocks*10^nchar(as.character(max(clusters))) + clusters
      
      D<-data.frame(id= paste0(1:n, blocks, clusters), blocks,clusters)
      return(D)                          
    }


make.outcomes <- function(
  treatment,
  blocks = rep(1,length(treatment)),
  clusters = blocks,
  linear = TRUE,
  binary = FALSE,
  treatment.effect = .5,
  block.means = rep(0,length(unique(blocks))),
  block.sds = rep(1,length(unique(blocks))),
  cluster.effects = rep(0,length(unique(clusters)))
){
    if(linear==binary){stop("Outcomes cannot be both linear and binary")}
    if(length(block.means)!=length(unique(blocks))){stop(
      "Length of block means must equal number of blocks"
    )}
    if(length(cluster.effects)!=length(unique(clusters))){stop(
      "Length of cluster means must equal number of blocks"
    )}
    
    if(linear){
      if(length(block.sds)!=length(unique(blocks))){stop(
        "Length of block sds must equal number of blocks")
      }
      Y0 <- as.vector(unlist(sapply(X = unique(blocks),FUN = function(i){
        rnorm(n = length(blocks[blocks==i]),mean =  block.means[i],sd = block.sds[i])
      })))
      Y0 <- as.vector(unlist(sapply(X = 1:length(unique(clusters)),FUN = function(i){
        Y0[clusters==unique(clusters)[i]] + cluster.effects[i]})))
      Y1 <- Y0 + treatment.effect
      
    }
    
    if(binary){
      if(TRUE%in% c(block.means>1,block.means<0)){stop(
        "Binary block means must be probabilities between 0 and 1 incl.")
      }
      Y0.probs <- as.vector(unlist(sapply(X = 1:length(unique(blocks)),FUN = function(i){
        rep(block.means[i],length(blocks[blocks==i]))
      })))
      cluster.probs <- as.vector(unlist(sapply(X = 1:length(unique(clusters)),FUN = function(i){
        rep(cluster.effects[i],length(clusters[clusters==unique(clusters)[i]]))
      })))
      Y0.probs <- Y0.probs + cluster.probs
      Y1.probs <- Y0.probs + treatment.effect                        
      Y1.probs[Y1.probs>1] <- 1
      Y1.probs[Y1.probs<0] <- 0
      
      Y0 <- rbinom(n = length(Y0.probs),size = 1,prob = Y0.probs)
      Y1 <- rbinom(n = length(Y1.probs),size = 1,prob = Y1.probs)
      
    }
    Y.obs <- Y1*treatment + Y0*(1-treatment)
    
    return(data.frame(Y0 = Y0, Y1 = Y1, Y.obs = Y.obs))
    
  }


# ICC code ----------------------------------------------------------------

n <- 10000

clustervar <-  .5
unitvar <- unit_variance <-   2

clusters <-  1:10
clustershock = rnorm(length(clusters), sd=clustervar^.5)

cluster = sample(clusters, n, replace=TRUE)

e <- clustershock[cluster] + rnorm(n, sd=unitvar^.5)

data = data.frame(e, cluster) 

icc <- clustervar/(clustervar+unit_variance)












