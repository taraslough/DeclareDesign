#' New Declare the Study Population
#'
#' @param ... A named list of expressions or list of named lists of expressions that generate data. These should generate vectors, the privileged name n_ can be used to specify the leve-specific N (i.e. rnorm(n_) creates 100 normal draws if it is at the level of 100 individuals, or 10 normal draws if it is at the level of 10 cities). These must be declared consecutively and are level-specific (see global_transformations for expressions that can be evaluated over the entire dataset). 
#' @param size A scalar (for single-level datasets) indicating the N, a vector of N's for each level (i.e. c(100,50,2) for 100 individuals in 50 cities in 2 regions), or a list of vectors specifying how many of the lower-level units are in each unit at that level (i.e. list(rep(1,100),rep(2,50),rep(25,2)) for the same example).
#' @param global_transformations An optional named list of expressions that can be evaluated across levels. For example, if group-invariant means and SDs have been created, these can be used with an expression in global_transformations in order to create individual-level draws of these group-level parameters.
#' @param other_arguments An optional list of objects that are used by the expressions passed to ... and global_transformations.
#' @param level_IDs An optional list of level ID indicators that are otherwise inferred from ... or generated using a default.
#' @param super_population If TRUE, data is thought of as a single draw from a super-population, and data-resampling is performed during the diagnostics phase. If FALSE, the population is thought of as finite. 
#' @param random_seed The random seed used for re-sampling. 
#' @param data User-provided data for bootrstrapping.
#' @param custom_population_function User-provided function for regenerating data.
#' @param make_unique_ID If TRUE, an ID is made for each unit that indicates all of the other level IDs.
#
#' @examples 
#' 
#' # Lots of nice examples go here.
#'
#' @export
declare_population <- function(..., 
                               size, 
                               global_transformations = NULL,
                               other_arguments = NULL,
                               level_IDs = NULL, 
                               super_population = FALSE,
                               random_seed = 42, 
                               data = NULL, 
                               custom_population_function = NULL,
                               make_unique_ID = FALSE) {
  
  # Check whether the user has supplied data
  
  no_data <- is.null(data)
  
  # Create user function
  
  if(no_data){
    
    if(!is.null(custom_population_function)){
      
      population_function <- wrap_custom_population_function(
        custom_population_function = custom_population_function,
        size = size,
        data = data)
      
    } else {
      
      population_function <- make_population_function(
        expressions = list(...),
        size = size, 
        global_transformations = global_transformations,
        other_arguments = other_arguments,
        level_IDs = level_IDs, 
        make_unique_ID = make_unique_ID)
      
    }
    
  } else {
    
    # Data provided 
    
    if(super_population == TRUE) {
      
      # Super population with data 
      
      if(!is.null(custom_population_function)){
        
        population_function <- wrap_custom_population_function(
          custom_population_function = custom_population_function,
          size = size,
          data = data,
          other_arguments = other_arguments)
        
      } else{
        
        population_function <- make_bootstrap_data_function(
          data = data, 
          size = size, 
          level_IDs = level_IDs)
        
      }
    } else {
      
      # Fixed data 
      # Add ability to add and resample covariates
      
      population_function <- function() return(data)
      
    }
    
    
  } 
  
  return_object <- list(population = population_function, 
                        super_population = super_population, 
                        random_seed = random_seed, 
                        call = match.call())
  class(return_object) <- "population"
  
  return(return_object)
  
}


#' @export
make_population_function <- function(
  expressions,
  size,
  level_IDs,
  global_transformations,
  other_arguments,
  make_unique_ID
){
  # Make sure that all of the levels are properly named
  expressions <- make_level_names(expressions = expressions,
                                  level_IDs = level_IDs)
  
  # Start creating custom function here
  make_population <- function(
    .size, 
    .other_arguments 
  ){
    
    # If the defaults are missing, grab them from the environment of the 
    # function, defined below
    if(!missing(.size)){
      size <- .size
    } else {
      size <- get("size",envir = make_pop_env)
    }
    
    if(!missing(.other_arguments)){
      other_arguments <- .other_arguments 
    } else {
      other_arguments <- get("other_arguments",envir = make_pop_env)
    }
    
    # Infer the data structure from the size argument
    hierarchy <- get_hierarchy(size = size)
    
    # Unpack the data hierarchy
    N_levels <- hierarchy$N_levels
    N <- hierarchy$N
    N_per_level <- hierarchy$N_per_level
    group_sizes <- hierarchy$group_sizes_per_level
    
    # Get the names of the level IDs
    level_IDs <- get_level_IDs(expression_list = expressions,
                               level_IDs = level_IDs,
                               N_levels = N_levels)
    
    # Get the IDs that are not the first level, to merge by
    merge_IDs <- level_IDs[-1]
    
    # make_structure goes through and creates an ID for each level in a list,
    # this can be thought of as the "skeleton" of the data
    data_structure <- make_structure(
      hierarchy = hierarchy,
      level_IDs = level_IDs)
    
    # at each level, make_environ creates an environment with the n_ object, 
    # other arguments, and all the expressions specific to a level
    environ_list <- mapply(
      FUN = make_environ,
      data_structure = data_structure,
      exprs = expressions,
      MoreArgs = list(other_arguments = other_arguments)
    )
    
    # At each level, make_data_frame takes an environment and
    # - evaluates all of the expressions specific to that level
    # - removes all of the objects that aren't variables
    # - coerces the resultant stuff to a data.frame 
    # So this produces a list of data.frames:
    data_list <- lapply(X = environ_list,
                        FUN = make_data_frame,
                        other_arguments = other_arguments)
    
    # Create a list of IDs of the level higher for each lower level, 
    # for use in merging (in multi-level cases)
    merge_vars <- mapply(FUN = make_merge_ID, 
                         group_sizes = group_sizes[-1],
                         level_ID = level_IDs[-1],
                         data_structure = data_structure[-1]
    )
    
    # Name the IDs that will be used for merging (in multi-level cases)
    merge_vars <- data.frame(merge_vars)
    names(merge_vars) <- merge_IDs
    
    # Store the data as return_data for sinle-level cases
    return_data <- data_list[[1]]
    
    # If the data is multi-level
    if(length(merge_vars)>0){
      
      # Go through the merge IDs and put it in the list of data to merge
      for (i in 1:length(merge_IDs)){
        data_list[[i]][,merge_IDs[i]] <- merge_vars[i]
      }
      
      # Store the return_data again, since it now has the merge variable
      return_data <- data_list[[1]]
      merge_data <- data_list[-1]
      
      # Go through and merge all of the levels
      for (i in 1:length(merge_data)){
        return_data <- merge(x = return_data,
                             y = merge_data[[i]],
                             by = merge_IDs[i],
                             all.x = T)
      }
    }
    
    # Now create the environment for evaluating all of the global transformatons
    return_env <- make_environ(
      data_structure = return_data,
      exprs = global_transformations,
      other_arguments = other_arguments)
    
    # And evaluate them, returning a data frame
    return_data <- make_data_frame(temp_env = return_env,
                                   other_arguments = other_arguments)
    return_data <- as.data.frame(as.list(return_env))
    
    # If the data is multilevel, reorder the data frame in order of the IDs
    if(length(level_IDs)>1){
      return_data <- reorder_by_ID(
        level_IDs = level_IDs,
        data = return_data,
        make_unique_ID = make_unique_ID)
    }
    
    return(return_data)
    
  }

  # Create the environment that make_population needs to do the above, 
  # but only if it doesn't have values provided for .size, .other_arguments
  make_pop_env <- list2env(
    list(expressions = expressions,
         size = size,
         level_IDs = level_IDs,
         global_transformations = global_transformations,
         other_arguments = other_arguments,
         make_unique_ID = make_unique_ID)
  )
  
  environment(make_population) <- make_pop_env
  
  return(make_population)
  
}

get_level_IDs <- function(expression_list, level_IDs, N_levels){
  
  level_names <- NULL
  
  # Check whether the user has supplied variables
  no_variables <- length(expression_list)==0
  
  if(!no_variables & all(sapply(expression_list, length)==0)){
    no_variables <- TRUE
    level_names <- names(expression_list)
    if(is.null(level_IDs)){
      level_IDs <- paste0(level_names,"_ID")
    }
  }
  
  # If there are no variables specified, just data structure
  if(no_variables){
    
    if(is.null(level_names)){
      level_names <- paste0("level_", 1:N_levels)
    }
    if(is.null(level_IDs)){
      level_IDs <- paste0(level_names,"_ID")
    }
    one_level <- N_levels==1
  }else{
    # Check all this stuff for finding names: 
    
    # Get the classes of the variables
    variable_classes <- sapply(expression_list,class)
    
    # If all of the variables are list objects, then they are levels
    if(all(variable_classes=="list")){
      
      level_names <- names(expression_list)
      
      # If there are no level names, generate them
      if(is.null(level_names)){
        level_names <- paste0("level_",1:length(expression_list))
      }
      
      # If some level names not provided
      if( "" %in% level_names ){
        N_missing_names <- sum(level_names=="")
        
        level_names[level_names==""] <- paste0("level_",LETTERS[1:N_missing_names])
      }
      
      # Indicator for whether there is just one level
      one_level <- FALSE
      
      # in the case of just one level, bring it back to the no-list case
      if(length(expression_list)==1){
        # expression_list <- unlist(expression_list,recursive = F)
        expression_list <- expression_list[[1]]
        variable_classes <- sapply(expression_list,class)
        one_level <- TRUE
      }
      
      # Create this as a consistency check
      N_levels <- length(level_names)
      
      if(is.null(level_IDs)){
        level_IDs <- paste0(level_names,"_ID")
      }
      
      # Find any levels that have no variables declared
      level_lengths <- sapply(expression_list,length)
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
          level_IDs <- paste0(level_names,"_ID")
        }
        
      }else{stop("You must supply ... with variable declarations or lists of variable declarations.")}
    }
  }
  return(level_IDs)
}

make_structure <- function(hierarchy,level_IDs){
  dat_list <- lapply(hierarchy$N_per_level,
                     function(n)
                       data.frame(1:n)
  )
  for(i in 1:length(dat_list)) 
    names(dat_list[[i]]) <- level_IDs[i]
  return(dat_list)
}

make_environ <- function(data_structure,exprs,other_arguments){
  temp_env <- list2env(data_structure)
  temp_env$n_ <- nrow(data_structure)
  temp_env$expressions <- exprs
  if(!is.null(other_arguments)){
    if(is.null(names(other_arguments))){
      stop("Elements of the other_arguments list should be named.")
    }
    list2env(x = other_arguments,envir = temp_env)
  }
  return(temp_env)
}

make_data_frame <- function(temp_env,other_arguments){
  other_names <- names(other_arguments)
  temp_expr <- get("expressions",envir = temp_env)
  varnames <- names(temp_expr)
  for (variable in varnames){
    assign(x = variable,
           value = eval(parse(text = temp_expr[variable]), 
                        envir = temp_env),
           envir = temp_env
    )
  }
  # See if it speeds things up to just keep stuff that's not varnames
  rm(list = c("n_","expressions",other_names),envir = temp_env)
  data <- as.data.frame(as.list(temp_env))
  return(data)
}

make_merge_ID <- function(group_sizes,data_structure,level_ID){
  rep(data_structure[,level_ID],group_sizes)
}

make_level_names <- function(expressions, level_IDs){
  if(!is.null(level_IDs)){
    level_names <- level_IDs
  }
  # Check it is a list of lists
  all_lists <- all(sapply(expressions,class)=="list")
  
  
  if(!all_lists){
    expressions <- list(expressions)
  }
  
  level_names <- names(expressions)
  
  # Check if all have names
  if(is.null(level_names)){
    level_names <- ""
  }
  
  if( "" %in% level_names ){
    N_missing_names <- sum(level_names=="")
    
    level_names[level_names==""] <- paste0("level_",LETTERS[1:N_missing_names])
    
  }
  
  names(expressions) <- level_names
  
  return(expressions)
}

reorder_by_ID <- function(
  level_IDs, data , make_unique_ID
){
  not_IDs <- names(data)[!names(data) %in% level_IDs]
  ID_data <- subset(data,select = level_IDs)
  not_ID_data <- subset(data,select = not_IDs)
  max_IDs <- sapply(ID_data, function(x)length(unique(x)))
  level_IDs <- level_IDs[order(max_IDs, decreasing = T)]
  ID_data <- ID_data[,level_IDs]
  if(make_unique_ID){
    unique_ID <- apply(ID_data, 1 , paste ,collapse = "_")
    data <- data.frame(ID_data[,level_IDs],
                       unique_ID = unique_ID,
                       not_ID_data)
  } else {
    data <- data.frame(ID_data[,level_IDs],
                       not_ID_data)
  }
  return(data)
} 

get_hierarchy <- function(size){
  
  size_class <- class(size)
  is_list <- size_class == "list"
  
  if(is_list){
    N <- N_per_level <- NULL
    group_sizes_per_level <- size
  }
  
  is_numeric <- size_class %in% c("numeric","integer")
  
  if(is_numeric){
    if(length(size) == 1){
      group_sizes_per_level <- N_per_level <- NULL
      N <- size
    } else {
      group_sizes_per_level <- N <- NULL
      N_per_level <- size
    }
  }
  
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
    N <- sum(group_sizes_per_level[[1]])
    
    # Check if first level is all 1's
    all_ones <- all(unique(group_sizes_per_level[[1]])==1)
    
    # If not, add in the ones level
    if(!all_ones){
      first_level <- list(rep(1,N))
      group_sizes_per_level <- c(first_level,group_sizes_per_level)
    }
    N_per_level <- sapply(group_sizes_per_level,length)
    
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



wrap_custom_population_function <- function(
  custom_population_function,
  size,
  data = NULL, 
  other_arguments = NULL
  ){
  
  if(!any("size" %in% custom_arguments))
    stop("Your custom data function must include a size argument.")
  
  custom_pop_env <- list2env(
    list(size = size,
         custom_arguments = custom_arguments,
         data = data)
  )
  
  make_population <- function(.size,.custom_arguments,.data) {
    
    if(!missing(.size)){
      size <- .size
    } else {
      size <- get("size",envir = custom_pop_env)
    }
    
    if(!missing(.data)){
      data <- .data
    } else {
      data <- get("data",envir = custom_pop_env)
    }
    
    if(!missing(.custom_arguments)){
      custom_arguments <- .custom_arguments 
    } else {
      custom_arguments <- get("custom_arguments",envir = custom_pop_env)
    }
    
    function_args <- c(size = size, data = data, custom_arguments)
    
    function_args <- get_custom_args(other_arguments = function_args,
                                     custom_function = custom_population_function) 

    do.call(what = custom_population_function,args = function_args)

    }

  environment(make_population) <- custom_pop_env

  return(make_population)
}

make_bootstrap_data_function <- function(data, size, level_IDs = NULL){
  
  hierarchy <- get_hierarchy(size = size)
  N_levels <- hierarchy$N_levels
  
  ## do checks
  if(is.null(level_IDs) & N_levels > 1){
    stop("Please provide level_IDs if N_levels is greater than 1")
  }
  
  boot_pop_env <- list2env(
    list(size = size,
         level_IDs = level_IDs,
         data = data)
  )
  
  population_function <- function(.size){
    
    if(!missing(.size)){
      size <- .size
    } else {
      size <- get("size",envir = boot_pop_env)
    }
    
    data <- get("data",envir = boot_pop_env)
    level_IDs <- get("level_IDs",envir = boot_pop_env)
    
    bootstrap_data(data = data, size = size, level_IDs = level_IDs)
    
  }
  
  environment(make_population) <- boot_pop_env
  
  return(population_function)
  
}

get_custom_args <- function(other_arguments,custom_function){
  arg_list <- other_arguments[names(other_arguments) %in% names(formals(custom_function))]
  return(arg_list)
}


