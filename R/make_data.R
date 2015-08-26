#' Make the full dataset or just a sample
#'
#' @param potential_outcomes An outcomes_object made with \code{\link{declare_potential_outcomes}}.
#' @param sample_frame A sample_frame object made with \code{\link{declare_sample}}, or a pre-existing dataframe
#' @param blocks A blocks object, made with \code{\link{declare_blocks}} (optional).
#' @param clusters A clusters object, made with \code{\link{declare_clusters}} (optional).
#' @param N If sample_frame is provided, this argument is ignored.
#' @param sep a character string used in the naming of potential outcomes. Defaults to "_".
#' @export
make_data <-
  function(potential_outcomes = NULL, sample_frame = NULL,blocks = NULL, clusters = NULL, N = NULL, sep = "_") {
    if (is.null(sample_frame) & is.null(potential_outcomes))
      stop("You must provide at least a sample frame or a potential outcomes object.")
    
    
    if(class(potential_outcomes)=="list"){
      proportion_check <- sapply(potential_outcomes,function(PO)"population_proportions"%in%names(PO))
    }else{
      proportion_check <- "population_proportions"%in%names(potential_outcomes)
    }
    
    if(any(!proportion_check)&any(proportion_check)){
      stop("Elements of a list of potential_outcomes must all be defined in terms of a unit-level data-generating process or population-level proportions, but may not feature both simultaneously.")
    }
    
    
    
    if(all(proportion_check)){
      
      if(!is.null(sample_frame)){
        covariate_frame <- make_data(sample_frame = sample_frame,
                                     blocks = blocks,
                                     clusters = clusters,
                                     N = N,sep = sep)
        if(is.null(N)){
          N <- dim(covariate_frame)[1]
        }else{
          if(dim(covariate_frame)[1]!=N){
            stop("The sample size implied by sample_frame does not match the N argument you provided to make_data, harmonize them or use only one.")
          }
        }
      }
      
      if(is.null(N)){
        stop("You must provide N if you supply potential outcomes objects defined with population_proportions.")
      }
      
      make_proportions <- function(population_proportions,N){
        
        counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
        
        con_names <- rownames(population_proportions)
        
        outcomes <- apply(counts,2,function(.times){
          sample(
            rep(con_names,.times)
          )
        })
        
        colnames(outcomes) <- colnames(population_proportions)
        
        return(as.data.frame(outcomes))
      }
      
      
      
      if(class(potential_outcomes)=="list"){
        
        prop_PO_list <- lapply(1:length(proportion_check),function(i){
          prop_PO <- make_proportions(population_proportions = potential_outcomes[[i]]$population_proportions,
                           N = N
          )
          names(prop_PO) <- paste0(potential_outcomes[[i]]$proportion_outcome_name,
                                   sep,
                                   potential_outcomes[[i]]$condition_names
                                   )
          return(prop_PO)
          })
        
        return_frame <- data.frame(prop_PO_list[[1]])
        
        for(i in 2:length(prop_PO_list)){
          
          return_frame <- data.frame(return_frame,prop_PO_list[[i]])
        }
      
      }else{
        
        return_frame <- make_proportions(population_proportions = potential_outcomes$population_proportions,
                                    N = N
        )
        names(return_frame) <- paste0(potential_outcomes$proportion_outcome_name,
                                 sep,
                                 potential_outcomes$condition_names
        )
        
      }
      if(!is.null(sample_frame)){
        return_frame <- data.frame(return_frame,covariate_frame)
      }
      
      # convert factors to integers
      for(i in 1:ncol(return_frame)){
        numeric_check <- identical(return_frame[,i],as.factor(as.integer(as.character(return_frame[,i]))))
        if(numeric_check){
          return_frame[,i] <- as.integer(as.character(return_frame[,i]))
        }
      }
      
      return(return_frame)
      
    }
    
    
    
    if(!is.null(potential_outcomes) & all(sapply(potential_outcomes, class) == "potential_outcomes")) {
      return_frame <-
        make_data(
          potential_outcomes = potential_outcomes[[1]],
          sample_frame = sample_frame,
          blocks = blocks,
          clusters = clusters,
          N = N,
          sep = sep
        )
      
      for (i in 2:length(potential_outcomes)) {
        return_frame <-
          make_data(
            potential_outcomes = potential_outcomes[[i]],
            sample_frame = declare_sample(data = return_frame)
          )
      }
      return(return_frame)
    }else{
      if (!is.null(potential_outcomes)) {
        condition_names  <- potential_outcomes$condition_names
        # cluster_var_name <- potential_outcomes$cluster_variable
        outcome_formula  <- potential_outcomes$outcome_formula
        # ICC              <- potential_outcomes$ICC
        outcome_variable <- potential_outcomes$outcome_variable
        covariate_names  <-
          all.vars(outcome_formula)[!all.vars(outcome_formula) %in% condition_names][-1]
        outcome_name     <- all.vars(outcome_formula)[1]
        if (length(covariate_names) > 0) {
          model_formula    <-
            as.formula(paste0(
              outcome_name," ~ ", paste(covariate_names,collapse = "+")
            ))
        } else{
          model_formula <- NULL
        }
      }
      # Check whether sample_frame_object is sample_frame class or a user-supplied matrix
      
      if (is.null(sample_frame) |
          class(sample_frame) != "sample_frame")
        stop(
          "Please send the sample_frame argument an object created using declare_sample. You can send just a data frame to declare_sample to use your own fixed data."
        )
      
      if (!is.null(sample_frame$make_sample)) {
        X <- sample_frame$make_sample()
        
      } else {
        X <- sample_frame$data
      }
      
      if (is.null(potential_outcomes)){
        return(X)
      }
      
      ## Check that all of the variables in the formula are in the X matrix
      ## or in the treatment names
      ## Check that the baseline is nested in the treatment formula
      if (FALSE %in% (all.vars(outcome_formula)[-1] %in% c(names(X),condition_names)))
        stop(
          "All of the variables in the formula should either be in the sample matrix or in the condition_names of the design_object."
        )
      treat_mat <- diag(length(condition_names))
      colnames(treat_mat) <- condition_names
      
      # Make a function that generates potential outcomes as a function of
      # all of the variables (treatment assignment, sample_frame) and some normal noise
      
      gen_outcome  <- eval(parse(
        text = paste0(
          "function(slice){y <- with(slice,{",outcome_formula[3],"});return(y)}"
        )
      ))
      
#       if (potential_outcomes$outcome_variable$distribution == "normal") {
#         unit_variance <- potential_outcomes$outcome_variable$sd ^ 2
#         epsilon <- rnorm(
#           n = dim(X)[1],
#           mean = potential_outcomes$outcome_variable$mean,
#           sd = potential_outcomes$outcome_variable$sd
#         )
#       } else {
#         unit_variance <- 1
#         epsilon <- rnorm(n = dim(X)[1], mean = 0, sd = 1)
#       }
#       
      outcomes <- matrix(
        data = NA,
        nrow = dim(X)[1],
        ncol = length(condition_names),
        dimnames = list(NULL,condition_names)
      )
      
      for (l in condition_names) {
        treat_mat <- matrix(
          data = 0,
          nrow = dim(X)[1],
          ncol = length(condition_names),
          dimnames = list(NULL,condition_names)
        )
        treat_mat[,l] <- 1
        
        data <- data.frame(treat_mat,X)
        
        outcomes[,l] <- gen_outcome(data) #+ epsilon
        
      }
      
      colnames(outcomes) <- paste0(outcome_name,sep,condition_names)
      
#       if (!is.null(ICC) & !is.null(cluster_var_name)) {
#         cluster_variance <- ICC * unit_variance / (1 - ICC)
#         cluster_shock <- rnorm(length(unique(X[,cluster_var_name])),
#                                sd = cluster_variance ^ .5)[as.numeric(as.factor(X[,cluster_var_name]))]
#         outcomes <- outcomes + cluster_shock
#       }
      # Check what the DGP of the outcome variable is and do necessary transformations
#       if (potential_outcomes$outcome_variable$distribution == "binary") {
#         outcomes <-
#           apply(outcomes,2,function(i)
#             rbinom(
#               n = dim(outcomes)[1],size = 1,prob = 1 / (1 + exp(-i))
#             ))
#       }
#       
      return_frame <- data.frame(outcomes)
      return_frame$make_data_sort_id <- 1:nrow(return_frame)
      if (!is.null(sample_frame)) {
        return_frame <- cbind(return_frame, X)
      }
      if (!is.null(clusters)) {
        return_frame <-
          cbind(return_frame, clusters$cluster_function(sample = return_frame))
      }
      
      if (!is.null(blocks)) {
        if (is.null(blocks$call$clusters)) {
          return_frame <-
            cbind(return_frame, blocks$blocks_function(sample = return_frame))
        } else {
          cluster_frame <-
            unique(return_frame[, c(clusters$cluster_name, blocks$call$blocks)])
          if (nrow(cluster_frame) != length(unique(return_frame[,clusters$cluster_name]))) {
            stop(
              "There is more than one level of a cluster-level covariate in at least one cluster, so you cannot block on it. Please construct cluster-level variables that have a single value within clusters."
            )
          }
          cluster_frame[, blocks$block_name] <-
            blocks$blocks_function(sample = cluster_frame)
          
          return_frame <-
            merge(
              return_frame, cluster_frame[, c(blocks$block_name, clusters$cluster_name)], by = clusters$cluster_name, all.x = TRUE, all.y = FALSE
            )
          
        }
      }
      
      return_frame <-
        return_frame[order(return_frame$make_data_sort_id),]
      return_frame$make_data_sort_id <- NULL
      
      return(return_frame)
    }
  }














