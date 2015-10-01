#' @export
declare_population <- function(..., 
                               N_per_level = NULL, group_sizes_per_level = NULL, N = NULL, 
                               level_ID_variables = NULL, super_population = FALSE,
                               random_seed = 42, data = NULL, 
                               custom_population_function = NULL,
                               potential_outcomes = NULL) {
  
  
  # Checks --------------------------------------------------------
  
  # Check whether the user has supplied data
  no_data <- is.null(data)
  
  # Create population function --------------------------------------------------------
  
  if(no_data){
    
    if(!is.null(custom_population_function)){
      
      population_function <- wrap_custom_population_function(custom_population_function = custom_population_function, 
                                                             N = N, N_per_level = N_per_level,
                                                             group_sizes_per_level = group_sizes_per_level)
      
    } else {
      
      population_function <- create_population_function(variable_list = list(...), N_per_level = N_per_level, N = N,
                                                        group_sizes_per_level = group_sizes_per_level, 
                                                        level_ID_variables = level_ID_variables)
      
    }
    
  } else {
    
    # Data provided --------------------------------------------------------
    
    if(super_population == TRUE) {
      
      if(!is.null(custom_population_function)){
        
        population_function <- wrap_custom_population_function(custom_population_function = custom_population_function, 
                                                               data = data, N = N, N_per_level = N_per_level,
                                                               group_sizes_per_level = group_sizes_per_level)
        
      } else{
  
        population_function <- create_bootstrap_data_function(data = data, N = N, N_per_level = N_per_level,
                                                              group_sizes_per_level = group_sizes_per_level, 
                                                              level_ID_variables = level_ID_variables)
        
      }
    }
    
    
  } 
  
  return_object <- list(population = population_function, super_population = super_population, random_seed = random_seed, call = match.call())
  class(return_object) <- "population"
  
  return(return_object)
  
}

#' @export
wrap_custom_population_function <- function(custom_population_function, data = NULL, 
                                            N = NULL, N_per_level = NULL, group_sizes_per_level = NULL){
  
  ## this is a helper function for custom function so it can pass N or N_per_level or group_sizes_per_level
  
  required_arguments <- c("N_per_level", "group_sizes_per_level", "N")
  custom_arguments <- names(formals(custom_population_function))
  
  if(!any(required_arguments %in% custom_arguments))
    stop("Your custom data function must include as arguments at least one of N_per_level, group_sizes_per_level, or N.")
  
  if("group_sizes_per_level" %in% names(formals(custom_population_function))){
    make_population <- function() custom_population_function(group_sizes_per_level = group_sizes_per_level)
  } else if("N_per_level" %in% names(formals(custom_population_function))){
    make_population <- function() custom_population_function(N_per_level = N_per_level)
  } else if("N" %in% names(formals(custom_population_function))){
    make_population <- function() custom_population_function(N = N)
  }
  
  return(make_population)
}

#' @export 
create_population_function <- function(variable_list = NULL, N_per_level = NULL, N = NULL,
                                       group_sizes_per_level = NULL, 
                                       level_ID_variables = NULL){
  
  # Checks ------------------------------------------------------------------
  
  # N_per_level and N should not be provided simultaneously
  if(!is.null(N_per_level) & !is.null(N)) {
    stop("You may not specify N and N_per_level simultaneously.")
  }
  
  # group_sizes_per_level and N should not be provided simultaneously
  if(!is.null(group_sizes_per_level) & !is.null(N)){
    stop("You may not specify N and group_sizes_per_level simultaneously.")
  }
  
  # N_per_level and group_sizes_per_level should not be provided simultaneously
  if(!is.null(group_sizes_per_level) & !is.null(N_per_level)){
    stop("You may not specify N_per_level and group_sizes_per_level simultaneously.") 
  }
  
  # Check that the necessary data structure info has been provided
  if(all(is.null(N_per_level), is.null(N), is.null(group_sizes_per_level))){
    stop("You must either specify N, group_sizes_per_level, N_per_level.")
  }
  
  # Check data structure, create data size indicators ----------------------------------
  
  hierarchy <- infer_data_hierarchy(N = N, N_per_level = N_per_level, group_sizes_per_level = group_sizes_per_level)
  N <- hierarchy$N
  N_per_level <- hierarchy$N_per_level
  group_sizes_per_level <- hierarchy$group_sizes_per_level
  
  # Make the objects used to generate data ----------------------------------
  
  level_IDs <- level_names <- NULL
  
  if(!is.null(level_ID_variables)){
    level_IDs <- level_ID_variables
  }
  
  any_empty <- FALSE
  
  # Check whether the user has supplied variables
  no_variables <- length(variable_list)==0
  
  if(!no_variables & all(sapply(variable_list, length)==0)){
    no_variables <- TRUE
    level_names <- names(variable_list)
    if(is.null(level_IDs)){
      level_IDs <- paste0(level_names,"_id")
    }
  }
  
  
  # If there are no variables specified, just data structure
  if(no_variables){
    N_levels <- length(N_per_level)
    if(is.null(level_names)){
      level_names <- paste0("level_", 1:N_levels)
    }
    if(is.null(level_IDs)){
      level_IDs <- paste0(level_names,"_id")
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
      
      if(is.null(level_IDs)){
        level_IDs <- paste0(level_names,"_id")
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
        
        if(is.null(level_IDs)){
          level_IDs <- paste0(level_names,"_id")
        }
        
      }else{stop("You must supply ... with variable declarations or lists of variable declarations.")}
    }
  }
  
  
  if(length(N_per_level)>N_levels){
    stop("The argument supplied to N_per_level or group_sizes_per_level implies more levels than you have allowed for in the variable declarations or user data provided to declare_population().")
  }
  
  if(length(N_per_level)<N_levels){
    stop("The argument supplied to N_per_level or group_sizes_per_level implies fewer levels than you have allowed for in the variable declarations or user data provided to declare_population().")
  }
  
  # Now generate the make_population() function when there is...
  
  # ... no data, one level, variables, and no empty lists
  if(one_level & !no_variables & !any_empty){
    make_population <- function(){
      X_mat <- make_covariate_matrix(variable_list,
                                     N = N)
      X_mat[,level_IDs] <- 1:dim(X_mat)[1]
      X_mat <- integerize(X_mat)
      X_mat <- as.data.frame(X_mat)
      X_mat$level_ID <- generate_ID(data = X_mat,level_names = level_IDs)
      return(X_mat)
    }
  }
  
  
  
  # ... no data, one level and no variables
  if(one_level & no_variables){
    make_population <- function(){
      X_mat <- data.frame(1:N)
      names(X_mat) <- level_IDs
      X_mat <- integerize(X_mat)
      X_mat$level_ID <- generate_ID(data = X_mat,level_names = level_IDs)
      return(X_mat)
    }
  } 
  
  # ... no data, multiple levels and either variables or no variables
  if(!one_level){
    
    make_population <- function(){
      
      X_list <- lapply(1:N_levels,function(i){
        # Case when variables are supplied
        if(!no_variables){
          
          if(which_empty[i]){
            X_mat <- data.frame(id = 1:N_per_level[i])
          }else{
            X_mat <- make_covariate_matrix(variables = variable_list[[i]],
                                           N = N_per_level[i])  
            X_mat$id <- 1:dim(X_mat)[1]
          }
          names(X_mat)[names(X_mat)=="id"] <- level_IDs[i]
          # Case when variables not supplied
        }else{
          X_mat <- matrix(1:N_per_level[i], 
                          dimnames = list(NULL, level_IDs[i]))
        }
        X_mat <- data.frame(X_mat)
        X_mat <- integerize(X_mat)
        return(X_mat)
      })
      
      
      if(N_levels > 1){ 
        for(i in N_levels:2){
          X_list[[i-1]]$merge_id <- 
            sample(rep(X_list[[i]][,level_IDs[i]],
                       group_sizes_per_level[[i]]))
          
          names(X_list[[i-1]])[names(X_list[[i-1]])=="merge_id"] <- level_IDs[i]
          
          X_list[[i-1]] <- 
            merge(x  = X_list[[i-1]],
                  y  = X_list[[i]],
                  by = level_IDs[i])
        }
        population_matrix <- X_list[[1]]
      }
      
      population_matrix <- population_matrix[order(population_matrix[,level_IDs[1]]), , drop=FALSE]
      population_matrix <- integerize(population_matrix)
      population_matrix <- as.data.frame(population_matrix)
      population_matrix$level_ID <- generate_ID(data = population_matrix,level_names = level_IDs)
      
      return(population_matrix)
    }
  }
  
  return(make_population)
  
}

#' @export
remaindr <- function(numerator,denominator) {
  m_each <- rep(numerator %/% denominator, denominator)
  remainder <- numerator %% denominator
  m_each <-
    m_each + ifelse(1:denominator %in% sample(1:denominator, remainder), 1, 0)
  return(m_each)
}

#' @export 
make_covariate_matrix <- function(...,N) {
  
  variables  <- list(...)
  
  class_list <- sapply(variables,class)
  
  if (all(c("function","list") %in% class_list) |
      all(c("DGP_object","list") %in% class_list))
    stop(
      "Function takes either direct variable declarations or a list of variable declarations, not both at once."
    )
  
  if ("list" %in% class_list) {
    if(length(class_list)>1){
      stop("You should provide one single list of variables to make_covariate_matrix()")
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
  
  mat_out <- lapply(fun.list,function(each_function){each_function()})
  
  mat_out.char <- do.call(cbind.data.frame,mat_out)
  
  X_mat <- data.frame(matrix(rep(NA, dim(mat_out.char)[1] * dim(mat_out.char)[2]),
                             nrow = dim(mat_out.char)[1]))
  
  for (i in 1:dim(mat_out.char)[2]) {
    X_mat[,i] <- mat_out.char [i][,1]
  }
  
  names(X_mat) <- var_names
  
  if(length(transformations)>0){
    
    transformation_calls <- sapply(transformations,function(trans){
      trans$transformation
    })
    
    for(i in 1:length(trans_names)){
      X_mat[trans_names[i]] <- with(data = X_mat,expr = eval(parse(text = transformation_calls[i])))
    }
  }
  X_mat <- integerize(X_mat)
  return(X_mat)
}


#' Print a table describing covariates described in population
#'
#' @param population a population object created with declare_population frame.
#' @export
covariates_table <- function(population){
  cat("This will be a summary table of the distribution of each covariate at each level. Not implemented yet.")
}

#' @export
generate_ID <- function(data,level_names){
  # Get the maximum ID for each
  max_IDs <- apply(data, 2 ,max)[level_names]
  # Re-order from lowest - highest level
  level_names <- level_names[order(max_IDs, decreasing = T)]
  ID_mat <- data[,level_names, drop = FALSE]
  IDs <- apply(ID_mat, 1 , paste ,collapse = "_")
  return(IDs)
}

#' @export
infer_data_hierarchy <- function(N, N_per_level, group_sizes_per_level){
  
  # If group_sizes_per_level is supplied
  if(!is.null(group_sizes_per_level)){
    # Test that the structure is logical
    lower_units_test <- sapply(length(group_sizes_per_level):2,
                               function(i){
                                 sum(group_sizes_per_level[[i]])==
                                   length(group_sizes_per_level[[i-1]])})
    
    if(!all(lower_units_test)){
      stop("The argument supplied to group_sizes_per_level is not logical. The sum of every higher level should be equal to the length of the preceding lower level. For example, in a study with 4 units and 2 groups, group_sizes_per_level = list(c(1,1,1,1),c(2,2)).")
    }
    # Generate N_per_level and N
    N_per_level <- sapply(group_sizes_per_level,length)
    N <- sum(group_sizes_per_level[[1]])
  } else {
    # If N_per_level is supplied
    if(!is.null(N_per_level)){
      # Check if it is multi-level
      if(length(N_per_level) > 1){
        # Make sure it is logical
        if(!all(diff(N_per_level)<0))
          stop("Each level in N_per_level should be smaller than the preceding level.")
        # Generate group_sizes_per_level
        group_sizes_per_level <- 
          list(rep(NA,
                   length(N_per_level)))
        group_sizes_per_level[2:length(N_per_level)] <- 
          lapply(2:length(N_per_level),
                 function(i){
                   remaindr(N_per_level[i-1],N_per_level[i])
                 })
        group_sizes_per_level[[1]] <- 
          rep(1,N_per_level[1])
      }else{
        group_sizes_per_level <- list(rep(1,N_per_level[1]))
      }
      # Generate N
      N <- sum(group_sizes_per_level[[1]])
    }else{
      if(!is.null(N)){
        N_per_level <- c(N)
        group_sizes_per_level <- list(rep(1,N))
      }
      
      
    }
    
  }
  
  N_levels <- length(N_per_level)
  
  return(list(N = N, N_per_level = N_per_level, group_sizes_per_level = group_sizes_per_level, N_levels = N_levels))
}

#' @export
create_bootstrap_data_function <- function(data, N = NULL, N_per_level = NULL,
                                           group_sizes_per_level = NULL, level_ID_variables = NULL){
  
  hierarchy <- infer_data_hierarchy(N = N, N_per_level = N_per_level, group_sizes_per_level = group_sizes_per_level)
  N_levels <- hierarchy$N_levels
  
  ## do checks
  if(is.null(level_ID_variables) & N_levels > 1){
    stop("Please provide level_id_variables if N_levels is greater than 1")
  }
  
  population_function <- function(){
    bootstrap_data(data = data, N = N, N_per_level = N_per_level,
                   group_sizes_per_level = group_sizes_per_level, level_ID_variables = level_ID_variables)
  }
  return(population_function)
  
}

#' @export
bootstrap_data <- function(data, N = NULL, N_per_level = NULL,
                           group_sizes_per_level = NULL, level_ID_variables = NULL){
  
  hierarchy <- infer_data_hierarchy(N = N, N_per_level = N_per_level, group_sizes_per_level = group_sizes_per_level)
  N <- hierarchy$N
  N_per_level <- hierarchy$N_per_level
  group_sizes_per_level <- hierarchy$group_sizes_per_level
  N_levels <- hierarchy$N_levels
  
  sample_by_level <- list()
  for(j in N_levels:1){
    if(j == N_levels){
      if(is.null(level_ID_variables) & N_levels==1){
        sample_by_level[[j]] <- sample(1:nrow(data), N_per_level[j], replace = TRUE)
      }
      sample_by_level[[j]] <- sample(data[, level_ID_variables[j]], N_per_level[j], replace = TRUE)
    } else {
      ## now go through each of the units in the level above it
      sample_current_level <- c()
      for(k in sample_by_level[[j+1]]){
        sample_current_level <- c(sample_current_level, 
                                  sample(data[data[, level_ID_variables[j+1]] == k, level_ID_variables[j]], 
                                         round(N_per_level[j]/N_per_level[j+1]), replace = TRUE))
      }
      sample_by_level[[j]] <- sample_current_level
    }
  }
  data <- data[sample_by_level[[1]], , drop = FALSE]
  
  ## reset row names so they are unique
  rownames(data) <- 1:nrow(data)
  
  return(data)
}

