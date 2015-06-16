
#' Create the potential outcomes of the experiment
#'
#' @param covariate_object covariates data frame
#' @return a data frame?
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
      # Put in all of the logical checks here, i.e. did they supply a cluster 
      # variable that's an integer, etc. 
      # i.e. check ICC in (0,1), etc.
      make_outcomes <- function(){
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
          "function(slice){y <- with(slice,{",outcome_formula[3],"}) + rnorm(1,0,unit_variance^.5);return(y)}"
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
        outcomes     <- data.frame(t(sapply(1:dim(X)[1],function(i)each_treat(X[i,]))))
        names(outcomes) <- paste0(as.character(outcome_formula)[2],"_",design_object$condition_names)
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
          cluster_shock <- rnorm(length(unique(X$cluster)), sd = cluster_variance^.5)[X$cluster]
          outcomes <- outcomes + cluster_shock
        }
        # Check what the DGP of the outcome variable is and do necessary transformations
        if(outcome_DGP$distribution=="binary"){
          outcomes <- apply(outcomes,2,function(i)rbinom(n = dim(outcomes)[1],size = 1,prob = 1/(1 + exp(-i))))
        }
        return(data.frame(outcomes,X))
      }
      outcomes_object <- list(
        make_outcomes = make_outcomes,
        call = match.call()
        )
      class(outcomes_object) <- "outcomes_object"
      return(outcomes_object)
    }


