#' Make the full dataset or just a sample
#'
#' @param potential_outcomes An outcomes_object made with \code{\link{declare_potential_outcomes}}, or a list of outcomes_objects.
#' @param sample A sample object made with \code{\link{declare_sample}}, or a pre-existing dataframe
#' @param design A design object, made with \code{\link{declare_design}}
#' @param N An integer indicating sample size. If sample is provided, this argument is ignored.
#' @param sep a character string used in the naming of potential outcomes. Defaults to "_".
#' @param noncompliance A noncompliance object, made with \code{\link{declare_noncompliance}}.
#' @export
make_data <-
  function(potential_outcomes = NULL, sample = NULL, N = NULL, sep = "_", 
           assign_treatment = FALSE, observed_outcomes = FALSE,
           design = NULL, treatment_variable = NULL, outcome_variable = NULL,
           noncompliance = NULL) {
    
    if(is.null(potential_outcomes)&assign_treatment){
      stop("If you want to assign treatment, you must provide a potential_outcomes object (see declare_potential_outcomes()).")
    }
    
    if(observed_outcomes == TRUE & assign_treatment == FALSE)
      stop("To reveal outcome variable(s) from a given treatment assignment, set assign_treatment to TRUE.")
    
    if((is.null(design) | is.null(treatment_variable)) & assign_treatment){
      stop("If assign_treatment = TRUE, you must supply both the name of the treatment variable (i.e. 'Z', or 'treatment_status') and the design object, declared using declare_design().")
    }
    
    ##if(!assign_treatment & (!is.null(design) | !is.null(treatment_variable))){
    ##  warning("The design and the treatment_variable arguments will only be used if assign_treatment = TRUE.")
    ##}
    
    if (is.null(sample) & is.null(potential_outcomes))
      stop("You must provide at least a sample frame or a potential outcomes object.")
    
    
    if(class(potential_outcomes)=="list"){
      proportion_check <- sapply(potential_outcomes,function(PO)"population_proportions"%in%names(PO))
    }else{
      proportion_check <- "population_proportions"%in%names(potential_outcomes)
    }
    
    if(any(!proportion_check)&any(proportion_check)){
      stop("Elements of a list of potential_outcomes must all be defined in terms of a unit-level data-generating process or population-level proportions, but may not feature both simultaneously.")
    }
    
    if(!is.null(potential_outcomes)){
      outcome_variable <- potential_outcomes$outcome_name
    }
    
    if(all(proportion_check)){
      
      if(!is.null(sample)){
        covariate_frame <- make_data(sample = sample,
                                     design = design,
                                     #blocks = blocks,
                                     #clusters = clusters,
                                     N = N,sep = sep)
        if(is.null(N)){
          N <- dim(covariate_frame)[1]
        }else{
          if(dim(covariate_frame)[1]!=N){
            stop("The sample size implied by sample does not match the N argument you provided to make_data, harmonize them or use only one.")
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
        
        outcomes <- integerize(as.data.frame(outcomes))
        
        outcomes <- with_treatment(outcomes,design=design,assign_treatment=assign_treatment,treatment_variable=treatment_variable,outcome_variable=outcome_variable,sep = sep)
        
        return(outcomes)
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
          prop_PO <- integerize(prop_PO)
          prop_PO <- with_treatment(prop_PO,design=design,assign_treatment=assign_treatment,treatment_variable=treatment_variable,outcome_variable=outcome_variable,sep = sep)
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
        names(return_frame) <- paste0(potential_outcomes$outcome_name,
                                      sep,
                                      potential_outcomes$condition_names
        )
        
      }
      if(!is.null(sample)){
        return_frame <- data.frame(return_frame,covariate_frame)
      }
      
      # convert factors to integers
      
      return_frame <- integerize(return_frame)
      return_frame <- with_treatment(return_frame,design=design,assign_treatment=assign_treatment,treatment_variable=treatment_variable,outcome_variable=outcome_variable,sep = sep)
      
      return(return_frame)
      
    }
    
    if(!is.null(potential_outcomes) & all(sapply(potential_outcomes, class) == "potential_outcomes")) {
      return_frame <-
        make_data(
          potential_outcomes = potential_outcomes[[1]],
          sample = sample,
          design = design,
          #blocks = blocks,
          #clusters = clusters,
          N = N,
          sep = sep
        )
      
      for (i in 2:length(potential_outcomes)) {
        return_frame <-
          make_data(
            potential_outcomes = potential_outcomes[[i]],
            sample = declare_sample(data = return_frame)
          )
      }
      
      return_frame <- integerize(return_frame)
      
      return(return_frame)
    }else{
      if (!is.null(potential_outcomes)) {
        condition_names  <- potential_outcomes$condition_names
        outcome_formula  <- potential_outcomes$outcome_formula
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
      # Check whether sample_object is sample class or a user-supplied matrix
      
      if (is.null(sample) |
          class(sample) != "sample")
        stop(
          "Please send the sample argument an object created using declare_sample. You can send just a data frame to declare_sample to use your own fixed data."
        )
      
      if (!is.null(sample$make_sample)) {
        X <- sample$make_sample()
        
      } else {
        X <- sample$data
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
      # all of the variables (treatment assignment, sample) and some normal noise
      
      gen_outcome  <- eval(parse(
        text = paste0(
          "function(slice){y <- with(slice,{",outcome_formula[3],"});return(y)}"
        )
      ))
      
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
      
      return_frame <- data.frame(outcomes)
      return_frame$make_data_sort_id <- 1:nrow(return_frame)
      if (!is.null(sample)) {
        return_frame <- cbind(return_frame, X)
      }
      if (!is.null(design$clusters)) {
        return_frame <-
          cbind(return_frame, design$clusters$cluster_function(sample = return_frame))
      }
      
      if (!is.null(design$blocks)) {
        if (is.null(design$blocks$call$clusters)) {
          return_frame <-
            cbind(return_frame, design$blocks$blocks_function(sample = return_frame))
        } else {
          cluster_frame <-
            unique(return_frame[, c(design$clusters$cluster_name, design$blocks$call$blocks)])
          if (nrow(cluster_frame) != length(unique(return_frame[,design$clusters$cluster_name]))) {
            stop(
              "There is more than one level of a cluster-level covariate in at least one cluster, so you cannot block on it. Please construct cluster-level variables that have a single value within clusters."
            )
          }
          cluster_frame[, design$blocks$block_name] <-
            design$blocks$blocks_function(sample = cluster_frame)
          
          return_frame <-
            merge(
              return_frame, 
              cluster_frame[, c(design$blocks$block_name, design$clusters$cluster_name)], 
              by = design$clusters$cluster_name, all.x = TRUE, all.y = FALSE
            )
          
        }
      }
      
      return_frame <-
        return_frame[order(return_frame$make_data_sort_id),]
      return_frame$make_data_sort_id <- NULL
      
      return_frame <- integerize(return_frame)
      return_frame <- with_treatment(return_frame,design=design,assign_treatment=assign_treatment,observed_outcomes=observed_outcomes,treatment_variable=treatment_variable,outcome_variable=outcome_variable,sep = sep)
      
      return(return_frame)
    }
  }

#' @export
integerize <- function(data_frame){
  for(i in 1:ncol(data_frame)){
    numeric_check <- FALSE
    numeric_check <- class(data_frame[,i])%in%c("numeric","integer")
    
    if(!numeric_check){
      suppressWarnings(numeric_check <- identical(data_frame[,i],as.factor(as.integer(as.character(data_frame[,i])))))
      if(!numeric_check){
        suppressWarnings(numeric_check <- identical(data_frame[,i],as.factor(as.numeric(as.character(data_frame[,i])))))
        if(!numeric_check){
          suppressWarnings(numeric_check <- identical(data_frame[,i],as.numeric(as.character(data_frame[,i]))))
          if(!numeric_check){
            suppressWarnings(numeric_check <- identical(data_frame[,i],as.numeric(as.character(data_frame[,i]))))
          }
        }
      }
      if(numeric_check){
        data_frame[,i] <- as.integer(as.character(data_frame[,i]))
      }
    }
  }
  return(data_frame)
}


#' @export
with_treatment <- function(X, design, assign_treatment, observed_outcomes, treatment_variable, outcome_variable, sep){
  if(assign_treatment == TRUE){
    X <- as.data.frame(X)
    X[,treatment_variable] <- assign_treatment(design = design, data = X)
    if(observed_outcomes == TRUE){
      X[,outcome_variable] <- observed_outcome(outcome = outcome_variable, 
                                               treatment_assignment = treatment_variable, 
                                               data = X, sep = sep)
    }
  }
  return(X)
}







