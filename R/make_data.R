#' Make the full dataset or just a sample
#'
#' @param potential_outcomes An outcomes_object made with declare_potential_outcomes()
#' @param sample_frame A sample_frame object made with declare_sample_frame(), or a pre-existing dataframe
#' @param N If sample_frame is provided, this argument is ignored.
#' @export
make_data <- function(potential_outcomes = NULL, sample_frame = NULL, covariates_data = NULL, blocks=NULL, clusters=NULL, N = NULL,sep = "_"){
  
  if(is.null(sample_frame)&is.null(potential_outcomes))stop(
    "You must provide at least a sample frame or a potential outcomes object."
  )
  if(!is.null(potential_outcomes)){
    condition_names  <- potential_outcomes$condition_names
    cluster_var_name <- potential_outcomes$cluster_variable
    outcome_formula  <- potential_outcomes$outcome_formula
    ICC              <- potential_outcomes$ICC
    outcome_variable <- potential_outcomes$outcome_variable
    covariate_names  <- all.vars(outcome_formula)[!all.vars(outcome_formula)%in%condition_names][-1]
    outcome_name     <- all.vars(outcome_formula)[1]
    if(length(covariate_names)>0){
      model_formula    <- as.formula(paste0(outcome_name," ~ ", paste(covariate_names,collapse = "+")))
    }else(model_formula <- NULL)
  }
  # Check whether sample_frame_object is sample_frame class or a user-supplied matrix
  if(!is.null(sample_frame)){
    if(!class(sample_frame)%in%c("sample_frame","matrix","data.frame"))stop(
      "The sample frame must either be a sample_frame object created with declare_sample_frame(), or a dataframe or matrix."
    )
    if(class(sample_frame)=="sample_frame"){
      X <- sample_frame$make_sample()
    }
    if(class(sample_frame)%in%c("matrix","data.frame")){
      X <- sample_frame
    }
    if(is.null(potential_outcomes))return(X)}
  if(is.null(sample_frame)){
    if(is.null(N))stop("You have not supplied any sample frame, so the sample size cannot be determined. Please supply a value for N.")
    if(length(covariate_names)>0){
      X <- data.frame(matrix(data = rnorm(length(covariate_names)*N),ncol = length(covariate_names)))
      names(X) <- covariate_names
    }else{
      if(outcome_variable$distribution!="normal"){
        untreated_var <- declare_variable()
      }else{
        untreated_var <- outcome_variable
      }
      
      X <- make_X_matrix(untreated_outcome = untreated_var,
                         N = N)
      outcome_formula <- as.formula(paste0(as.character(outcome_formula)[2]," ",
                                           as.character(outcome_formula)[1],
                                           " untreated_outcome + " ,
                                           as.character(outcome_formula)[3]))
    }}
  
  
  # Check that all of the variables in the formula are in the X matrix or in the treatment names
  # Check that the baseline is nested in the treatment formula
  if(
    FALSE %in% (all.vars(outcome_formula)[-1] %in% c(names(X),condition_names))
  )stop("All of the variables in the formula should either be in the sample matrix or in the condition_names of the design_object.")
  treat_mat    <- diag(length(condition_names))
  colnames(treat_mat) <- condition_names
  # Make a function that generates potential outcomes as a function of 
  # all of the variables (treatment assignment, sample_frame) and some normal noise
  
  gen_outcome  <- eval(parse(text = paste0(
    "function(slice){y <- with(slice,{",outcome_formula[3],"});return(y)}"
  )))
  
  if(potential_outcomes$outcome_variable$distribution=="normal"){
    unit_variance <- potential_outcomes$outcome_variable$sd^2
    epsilon <- rnorm(n = dim(X)[1], 
                     mean = potential_outcomes$outcome_variable$mean,
                     sd = potential_outcomes$outcome_variable$sd)
  }else{
    unit_variance <- 1
    epsilon <- rnorm(n = dim(X)[1], mean=0, sd = 1)
  }
  
  outcomes <- matrix(data = NA,
                     nrow = dim(X)[1],
                     ncol = length(condition_names),
                     dimnames = list(NULL,condition_names))
  
  for (l in condition_names){
    treat_mat <- matrix(data = 0,
                        nrow = dim(X)[1],
                        ncol = length(condition_names),
                        dimnames = list(NULL,condition_names))
    treat_mat[,l] <- 1
    
    data <- data.frame(treat_mat,X)
    
    outcomes[,l] <- gen_outcome(data) + epsilon
    
  }
  
  colnames(outcomes) <- paste0(outcome_name,sep,condition_names)
  
  if(!is.null(ICC)&!is.null(cluster_var_name)){
    cluster_variance <- ICC*unit_variance/(1-ICC)
    cluster_shock <- rnorm(length(unique(X[,cluster_var_name])), sd = cluster_variance^.5)[X[,cluster_var_name]]
    outcomes <- outcomes + cluster_shock
  }
  # Check what the DGP of the outcome variable is and do necessary transformations
  if(potential_outcomes$outcome_variable$distribution=="binary"){
    outcomes <- apply(outcomes,2,function(i)rbinom(n = dim(outcomes)[1],size = 1,prob = 1/(1 + exp(-i))))
  }
  
  return_frame <- data.frame(outcomes)
  return_frame$make_data_sort_id <- 1:nrow(return_frame)
  if(!is.null(sample_frame)){return_frame <- cbind(return_frame, X)}
  if(!is.null(clusters)){return_frame <- cbind(return_frame, clusters$cluster_function(sample=return_frame))}
  
  if(!is.null(blocks)){
    if(is.null(blocks$call$clusters)){
      return_frame <- cbind(return_frame, blocks$blocks_function(sample=return_frame))
    }else{
      cluster_frame <- unique(return_frame[, c(clusters$cluster_name, blocks$call$blocks)])
      if(nrow(cluster_frame) != length(unique(return_frame[,clusters$cluster_name]))){
        stop("There is more than one level of a cluster-level covariate in at least one cluster, so you cannot block on it. Please construct cluster-level variables that have a single value within clusters.")
      } 
      cluster_frame[, blocks$block_name] <- blocks$blocks_function(sample=cluster_frame)
      
      return_frame <- merge(return_frame, cluster_frame[, c(blocks$block_name, clusters$cluster_name)], by = clusters$cluster_name, all.x = TRUE, all.y = FALSE)
      
    }
  }
  
  return_frame <- return_frame[order(return_frame$make_data_sort_id),]
  return_frame$make_data_sort_id <- NULL
  
  
  return(return_frame)
}












