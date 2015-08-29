#' Declare the structure of the sample frame 
#'
#' @param ... a list either of variable declarations, or of lists of variable declarations (one per level)
#' @param N_per_level vector of the sample sizes per level, one number per level
#' @param group_sizes_by_level description
#' @param N total sample size of the sample frame
#' @param data optional data frame to include variables that are not defined in the sample frame
#' @param resample when data are provided, indicates whether the data is resampled. By default, the data is returned as is. Resampling will automatically respect the levels defined by the variable declarations.
#' @param level_ID_variables optional strings indicating the variable names for the identifiers of each level, i.e. c("individual_id", "village_id")
#' @export
declare_sample <- function(..., N_per_level = NULL, group_sizes_by_level = NULL, N = NULL, data = NULL, 
                           resample = FALSE, level_ID_variables = NULL) {
  
  # Check whether the user has supplied data
  no_data <- is.null(data)
  
  if(!no_data & resample == FALSE & (!is.null(N) | !is.null(N_per_level) | !is.null(group_sizes_by_level)))
    stop("Please do not provide N, N_per_level, or group_sizes_by_level when resample is set to FALSE and you provided a dataframe.")
  
  # N_per_level and N should not be provided simultaneously
  if(!is.null(N_per_level) & !is.null(N)) {
    stop("You may not specify N and N_per_level simultaneously.")
  }
  
  # group_sizes_by_level and N should not be provided simultaneously
  if(!is.null(group_sizes_by_level) & !is.null(N)){
    stop("You may not specify N and group_sizes_by_level simultaneously.")
  }
  
  # N_per_level and group_sizes_by_level should not be provided simultaneously
  if(!is.null(group_sizes_by_level) & !is.null(N_per_level)){
    stop("You may not specify N_per_level and group_sizes_by_level simultaneously.") 
  }
  
  # Check that the necessary data structure info has been provided
  if(is.null(N_per_level) & is.null(N) & is.null(group_sizes_by_level) & 
     (no_data | (!no_data & resample == TRUE))){
    stop("You must either specify N, group_sizes_by_level, N_per_level.")
  }
  
  # Generate N from data if there is no resampling 
  if(is.null(N_per_level) & is.null(N) & is.null(group_sizes_by_level) & 
     (!no_data & resample == FALSE)){
    N <- dim(data)[1]
  }
  
  # If group_sizes_by_level is supplied
  if(!is.null(group_sizes_by_level)){
    # Test that the structure is logical
    lower_units_test <- sapply(length(group_sizes_by_level):2,
                               function(i){
                                 sum(group_sizes_by_level[[i]])==
                                   length(group_sizes_by_level[[i-1]])})
    
    if(!all(lower_units_test)){
      stop("The argument supplied to group_sizes_by_level is not logical. The sum of every higher level should be equal to the length of the preceding lower level. For example, in a study with 4 units and 2 groups, group_sizes_by_level = list(c(1,1,1,1),c(2,2)).")
    }
    # Generate N_per_level and N
    N_per_level <- sapply(group_sizes_by_level,length)
    N <- sum(group_sizes_by_level[[1]])
  }else{
    # If N_per_level is supplied
    if(!is.null(N_per_level)){
      # Check if it is multi-level
      if(length(N_per_level) > 1){
        # Make sure it is logical
        if(!all(diff(N_per_level)<0))
          stop("Each level in N_per_level should be smaller than the preceding level.")
        # Generate group_sizes_by_level
        group_sizes_by_level <- 
          list(rep(NA,
                   length(N_per_level)))
        group_sizes_by_level[2:length(N_per_level)] <- 
          lapply(2:length(N_per_level),
                 function(i){
                   remaindr(N_per_level[i-1],N_per_level[i])
                 })
        group_sizes_by_level[[1]] <- 
          rep(1,N_per_level[1])
      }else{
        group_sizes_by_level <- list(rep(1,N_per_level[1]))
      }
      # Generate N
      N <- sum(group_sizes_by_level[[1]])
    }else{
      if(!is.null(N)){
        N_per_level <- c(N)
        group_sizes_by_level <- list(rep(1,N))
      }
      
      
    }
    
  }
  
  # Make the objects used to generate data 
  
  variable_list  <- list(...)
  level_ids <- level_names <- NULL
  
  if(!is.null(level_ID_variables)){
    level_ids <- level_ID_variables
  }
  
  # Check whether the user has supplied variables
  no_variables <- length(variable_list)==0
  
  if(!no_variables&all(sapply(variable_list,length)==0)){
    no_variables <- TRUE
    level_names <- names(variable_list)
    if(is.null(level_ids)){
      level_ids <- paste0(level_names,"_id")
    }
  }
  
  
  # If there are no variables specified, just data structure
  if(no_variables){
    N_levels <- length(N_per_level)
    if(is.null(level_names)){
      level_names <- paste0("level_", 1:N_levels)
    }
    if(is.null(level_ids)){
      level_ids <- paste0(level_names,"_id")
    }
    one_level <- N_levels==1
  }else{
    
    # Get the classes of the variables
    variable_classes <- sapply(variable_list,class)
    
    # If all of the variables are list objects, then they are levels
    if(all(variable_classes=="list")){
      
      level_names <- names(variable_list)
      
      # If there are no level names, generate them
      if(is.null(level_names)){
        level_names <- paste0("level_",1:length(variable_list))
        warning(paste0("You have not supplied level_names  (i.e. level_1 = list(variables)), using the following defaults: \n", level_names))
      }
      
      # If some level names not provided
      if( "" %in% level_names ){
        N_missing_names <- sum(level_names=="")
        
        level_names[level_names==""] <- paste0("level_",LETTERS[1:N_missing_names])
        warning(paste0("You have not supplied level_names for all levels, using the following defaults for some levels: \n", level_names))
      }
      
      # Indicator for whether there is just one level
      one_level <- FALSE
      
      # in the case of just one level, bring it back to the no-list case
      if(length(variable_list)==1){
        # variable_list <- unlist(variable_list,recursive = F)
        variable_list <- variable_list[[1]]
        variable_classes <- sapply(variable_list,class)
        one_level <- TRUE
      }
      
      # Create this as a consistency check
      N_levels <- length(level_names)
      
      if(is.null(level_ids)){
        level_ids <- paste0(level_names,"_id")
      }
      
      # Find any levels that have no variables declared
      level_lengths <- sapply(variable_list,length)
      any_empty <- any(!level_lengths>0)
      which_empty <- level_lengths==0
      
    }else{
      # In this case the user didn't provide a multi-level data structure, 
      # there is just one level
      if(all(variable_classes %in% c("function","DGP_object"))&!no_variables){
        one_level <- TRUE
        
        N_levels <- 1
        
        if(is.null(level_names)){
          level_names <- "level_1"
        }
        
        if(is.null(level_ids)){
          level_ids <- paste0(level_names,"_id")
        }
        
      }else{stop("You must supply ... with variable declarations or lists of variable declarations.")}
    }
  }
  
  
  if(length(N_per_level)>N_levels){
    stop("The argument supplied to N_per_level or group_sizes_by_level implies more levels than you have allowed for in the variable declarations or user data provided to declare_sample().")
  }
  
  if(length(N_per_level)<N_levels){
    stop("The argument supplied to N_per_level or group_sizes_by_level implies fewer levels than you have allowed for in the variable declarations or user data provided to declare_sample().")
  }
  
  # Now generate the make_sample() function when there is...
  
  # ... no data, one level, variables, and no empty lists
  if(no_data & one_level & !no_variables & !any_empty){
    make_sample <- function(){
      X_mat <- make_X_matrix(variable_list,
                             N = N)
      X_mat[,level_ids] <- 1:dim(X_mat)[1]
      X_mat <- integerize(X_mat)
      return(X_mat)
    }
  }
  
  
  
  # ... no data, one level and no variables
  if(no_data & one_level & no_variables){
    make_sample <- function(){
      X_mat <- data.frame(1:N)
      names(X_mat) <- level_ids
      X_mat <- integerize(X_mat)
      return(X_mat)
    }
  } 
  
  # ... no data, multiple levels and either variables or no variables
  if(no_data & !one_level){
    
    make_sample <- function(){
      
      X_list <- lapply(1:N_levels,function(i){
        # Case when variables are supplied
        if(!no_variables){
          
          if(which_empty[i]){
            X_mat <- data.frame(id = 1:N_per_level[i])
          }else{
            X_mat <- make_X_matrix(variables = variable_list[[i]],
                                   N = N_per_level[i])  
            X_mat$id <- 1:dim(X_mat)[1]
          }
          names(X_mat)[names(X_mat)=="id"] <- level_ids[i]
          # Case when variables not supplied
        }else{
          X_mat <- matrix(1:N_per_level[i], 
                          dimnames = list(NULL, level_ids[i]))
        }
        X_mat <- data.frame(X_mat)
        X_mat <- integerize(X_mat)
        return(X_mat)
      })
      
      sample_matrix <- X_list[[1]]
      
      if(N_levels > 1){ 
        X_mat_joined <- NA
        for(i in N_levels:2){
          X_list[[i-1]]$merge_id <- 
            sample(rep(X_list[[i]][,level_ids[i]],
                       group_sizes_by_level[[i]]))
          
          names(X_list[[i-1]])[names(X_list[[i-1]])=="merge_id"] <- level_ids[i]
          
          X_mat_joined <- 
            merge(x  = X_list[[i-1]],
                  y  = X_list[[i]],
                  by = level_ids[i])
        }
        sample_matrix <- X_mat_joined
      }
      
      sample_matrix <- sample_matrix[order(sample_matrix[,level_ids[1]]), , drop=FALSE]
      sample_matrix <- integerize(sample_matrix)
      return(sample_matrix)
    }
  }
  
  
  
  if(!no_data){
    
    if(resample == TRUE){
      
      user_data <- data
      ## if data is sent into make_sample, this does not work (data is NULL), must have different name
      
      if(N_levels == 1) {
        make_sample <- function(){
          return(user_data[sample(1:nrow(user_data), N, replace = TRUE), , drop = FALSE])
        }
      } else if (N_levels > 1){
        make_sample <- function(){
          sample_by_level <- list()
          for(j in N_levels:1){
            if(j == N_levels){
              sample_by_level[[j]] <- sample(user_data[, level_ids[j]], N_per_level[j], replace = TRUE)
            } else {
              ## now go through each of the units in the level above it
              sample_current_level <- c()
              for(k in sample_by_level[[j+1]]){
                sample_current_level <- c(sample_current_level, 
                                          sample(user_data[user_data[, level_ids[j+1]] == k, level_ids[j]], 
                                                 round(N_per_level[j]/N_per_level[j+1]), replace = TRUE))
              }
              sample_by_level[[j]] <- sample_current_level
            }
          }
          user_data <- user_data[sample_by_level[[1]], , drop = FALSE]
          user_data <- integerize(user_data)
          return(user_data)
        }
      }
      
    } else {
      make_sample <- NULL
      covariate_names <- names(data)
    }
  }
  
  if(!is.null(make_sample)){
    test_mat <- make_sample()
    
    covariate_names <- names(test_mat)
  }
  
  sample_object <- list(
    make_sample = make_sample,
    data = data,
    covariate_names = covariate_names,
    level_names = level_names,
    level_ids = level_ids,
    number_levels = N_levels,
    N_per_level = N_per_level,
    group_sizes_by_level = group_sizes_by_level,
    call = match.call()
  )
  
  class(sample_object) <- "sample"
  return(sample_object)
}


#' @export 
make_X_matrix <- function(...,N) {
  
  variables  <- list(...)
  
  class_list <- sapply(variables,class)
  
  if (all(c("function","list") %in% class_list) |
      all(c("DGP_object","list") %in% class_list))
    stop(
      "Function takes either direct variable declarations or a list of variable declarations, not both at once."
    )
  
  if ("list" %in% class_list) {
    if(length(class_list)>1){
      stop("You should provide one single list of variables to make_X_matrix()")
    }
    variables <- variables[[1]]
  }
  
  # Get the variable names
  variable_names <- names(variables)
  
  if(is.null(variable_names)){
    variable_names <- ""
  }
  variable_name_missing <- variable_names == ""
  
  if(any(variable_name_missing)){
    N_varname_missing <- sum(variable_name_missing)
    variable_names[variable_name_missing] <-
      paste0("variable_",letters[1:N_varname_missing])
  }
  
  transformation_check <- sapply(variables,function(variable_elements){
    element_names <- names(variable_elements)
    is_transformation <- "transformation"%in%element_names
    return(is_transformation)
  })
  
  names(variables) <- variable_names
  
  transformations <- variables[which(transformation_check)]
  variables <- variables[which(!transformation_check)]
  
  var_names <- names(variables)
  trans_names <- names(transformations)
  
  
  fun.list <- lapply(variables,function(variable) {
    
    if (!class(variable) %in% c("function","DGP_object")) {
      stop(
        "Variables should either be random number functions or DGP_object (see declare_variable())"
      )
    }
    if (class(variable) == "function") {
      return(variable)
    }
    if (variable$distribution == "normal") {
      return(function() {
        rnorm(n = N,mean = variable$mean, sd = variable$sd)
      })
    }
    if (variable$distribution == "binary") {
      return(function() {
        binom_out <- rbinom(n = N,size = 1,prob = variable$probability)
        if (!is.null(variable$categories)) {
          binom_out <-
            factor(binom_out,c(0,1),
                   variable$categories)
        }
        return(binom_out)
      })
    }
    if (variable$distribution == "multinomial") {
      return(function() {
        multinom_out <-
          apply(rmultinom(
            n = N,size = 1,
            prob = variable$probability
          ),2,function(i)
            which(i == 1))
        if (!is.null(variable$categories)) {
          multinom_out <- factor(
            multinom_out,
            levels = 1:length(variable$categories),
            labels = variable$categories
          )
        }
        return(multinom_out)
      })
    }
  })
  
  x <- lapply(fun.list,function(each_function){each_function()})
  
  X.char <- do.call(cbind.data.frame,x)
  
  X <- data.frame(matrix(rep(NA, dim(X.char)[1] * dim(X.char)[2]),
                         nrow = dim(X.char)[1]))
  
  for (i in 1:dim(X.char)[2]) {
    X[,i] <- X.char[i][,1]
  }
  
  names(X) <- var_names
  
  if(length(transformations)>0){
    
    transformation_calls <- sapply(transformations,function(trans){
      trans$transformation
    })
    
    for(i in 1:length(trans_names)){
      X[trans_names[i]] <- with(data = X,expr = eval(parse(text = transformation_calls[i])))
    }
  }
  X <- integerize(X)
  return(X)
}

#' @export
remaindr <- function(numerator,denominator) {
  m_each <- rep(numerator %/% denominator, denominator)
  remainder <- numerator %% denominator
  m_each <-
    m_each + ifelse(1:denominator %in% sample(1:denominator, remainder), 1, 0)
  return(m_each)
}

#' @param object what is it?
#' @rdname declare_sample
#' @export
summary.sample <- function(object, ...){
  
  summ <- data.frame(object$level_names, object$N_per_level, "")
  colnames(summ) <- c("Level", "Units per level", "Description")
  summ
  
}

#' Print a table describing covariates described in sample
#'
#' @param sample a sample object created with declare_sample frame.
#' @export
covariates_table <- function(sample){
  cat("This will be a summary table of the distribution of each covariate at each level. Not implemented yet.")
}






