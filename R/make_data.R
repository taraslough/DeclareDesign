#' Make the full dataset or just covariates
#'
#' @param potential_outcomes An outcomes_object made with declare_potential_outcomes()
#' @param covariates A covariate_object made with declare_covariates(), or a pre-existing dataframe
#' @param N If covariates are provided, this argument is ignored.
#' @export
make_data <- function(potential_outcomes = NULL, covariates = NULL, N = NULL,sep = "_"){

  if(is.null(covariates)&is.null(potential_outcomes))stop(
    "You must provide at least covariates or a potential outcomes object."
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
  # Check whether covariate_object is covarite_object or a user-supplied matrix
  if(!is.null(covariates)){
    if(!class(covariates)%in%c("covariate_object","matrix","data.frame"))stop(
      "The covariates must either be a covariate_object created with declare_covariates(), or a dataframe or matrix."
    )
    if(class(covariates)=="covariate_object"){
      X <- covariates$make_covariates()
    }
    if(class(covariates)%in%c("matrix","data.frame")){
      X <- covariates
    }
    if(is.null(potential_outcomes))return(X)}
  if(is.null(covariates)){
    if(is.null(N))stop("You have not supplied any covariates, so the sample size cannot be determined. Please supply a value for N.")
    if(length(covariate_names)>0){
      X <- data.frame(matrix(data = rnorm(length(covariate_names)*N),ncol = length(covariate_names)))
      names(X) <- covariate_names
    }else{
      X <- make_X_matrix(untreated_outcome = outcome_variable,
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
  )stop("All of the variables in the formula should either be in the covariate matrix or in the condition_names of the design_object.")
  treat_mat    <- diag(length(condition_names))
  colnames(treat_mat) <- condition_names
  # Make a function that generates potential outcomes as a function of 
  # all of the variables (treatment assignment, covariates) and some normal noise
  
  if(potential_outcomes$outcome_variable$distribution=="normal"){
    if(is.null(covariates)){
      gen_outcome  <- eval(parse(text = paste0(
        "function(slice){y <- with(slice,{",outcome_formula[3],"});return(y)}"
      )))
    }else{
  gen_outcome  <- eval(parse(text = paste0(
    "function(slice){y <- with(slice,{",outcome_formula[3],"}) + rnorm(1,potential_outcomes$outcome_variable$mean,potential_outcomes$outcome_variable$sd);return(y)}"
  )))}
  unit_variance <- potential_outcomes$outcome_variable$sd^2
  }else{
    gen_outcome  <- eval(parse(text = paste0(
      "function(slice){y <- with(slice,{",outcome_formula[3],"}) + rnorm(1);return(y)}")))
    unit_variance <- 1
  }

  
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
  if(dim(X)[2]==1){
    X_name <- names(X)
    X <- X[,1]
    X <- setNames(X,rep(X_name,length(X))) 
    outcomes     <- data.frame(t(sapply(1:length(X),function(i)each_treat(X[i]))))
  }else{
  outcomes     <- data.frame(t(sapply(1:dim(X)[1],function(i)each_treat(X[i,]))))}
  names(outcomes) <- paste0(outcome_name,sep,condition_names)
  
  if(!is.null(ICC)&!is.null(cluster_var_name)){
    cluster_variance <- ICC*unit_variance/(1-ICC)
    cluster_shock <- rnorm(length(unique(X[,cluster_var_name])), sd = cluster_variance^.5)[X[,cluster_var_name]]
    outcomes <- outcomes + cluster_shock
  }
  # Check what the DGP of the outcome variable is and do necessary transformations
  if(potential_outcomes$outcome_variable$distribution=="binary"){
    outcomes <- apply(outcomes,2,function(i)rbinom(n = dim(outcomes)[1],size = 1,prob = 1/(1 + exp(-i))))
  }
  if(is.null(covariates))return(data.frame(outcomes))else{
    return(data.frame(outcomes,X))
  }
    
  
}









