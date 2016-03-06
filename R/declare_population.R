#' New Declare the Study Population
#'
#' @param ... A named list of expressions or list of named lists of expressions that generate data. These should generate vectors, the privileged name n_ can be used to specify the leve-specific N (i.e. rnorm(n_) creates 100 normal draws if it is at the level of 100 individuals, or 10 normal draws if it is at the level of 10 cities). These must be declared consecutively and are level-specific (see global_transformations for expressions that can be evaluated over the entire dataset). 
#' @param size A scalar (for single-level datasets) indicating the N, a vector of N's for each level (i.e. c(100,50,2) for 100 individuals in 50 cities in 2 regions), or a list of vectors specifying how many of the lower-level units are in each unit at that level (i.e. list(rep(1,100),rep(2,50),rep(25,2)) for the same example).
#' @param global_transformations An optional named list of expressions that can be evaluated across levels. For example, if group-invariant means and SDs have been created, these can be used with an expression in global_transformations in order to create individual-level draws of these group-level parameters.
#' @param options An optional list of objects that are used by the expressions passed to ... and global_transformations.
#' @param level_IDs An optional list of level ID indicators that are otherwise inferred from ... or generated using a default.
#' @param data User-provided data for bootrstrapping.
#' @param resample_data If TRUE, data is thought of as a single draw from a super-population, and data-resampling is performed during the diagnostics phase. If FALSE, the population is thought of as finite. 
#' @param custom_population_function User-provided function for regenerating data.
#' @param make_unique_ID If TRUE, an ID is made for each unit that indicates all of the other level IDs.
#' @param description A description of the population in words.
#
#' @examples 
#' 
#' # Lots of nice examples go here.
#'
#' @export
declare_population <- function(
  ..., 
  size, 
  global_transformations = NULL,
  options = NULL,
  level_IDs = NULL, 
  data = NULL, 
  resample_data = FALSE,
  custom_population_function = NULL,
  make_unique_ID = FALSE,
  description = NULL
) {
  
  # Check whether the user has supplied data
  
  no_data <- is.null(data)
  
  # Get expressions
  
  expressions <- list(...)
  
  # Create user function
  
  if(no_data){
    
    if(!is.null(custom_population_function)){
      
      population_function <- wrap_custom_population_function(
        custom_population_function = custom_population_function,
        size = size,
        data = data)
      
    } else {
      
      population_function <- make_population_function(
        expressions = expressions,
        size = size, 
        global_transformations = global_transformations,
        options = options,
        level_IDs = level_IDs, 
        make_unique_ID = make_unique_ID)
      
    }
    
  } else {
    
    # Data provided 
    
    if(resample_data == TRUE) {
      
      # Super population with data 
      
      if(!is.null(custom_population_function)){
        
        population_function <- wrap_custom_population_function(
          custom_population_function = custom_population_function,
          size = size,
          data = data,
          options = options)
        
      } else{
        
        population_function <- make_bootstrap_data_function(
          data = data, 
          size = size, 
          level_IDs = level_IDs)
        
      }
    } else {
      
      # Fixed data 
      
      if(length(expressions > 0)){
        
        if(!missing(size)){
          warning("Please note that arguments provided to size will be ignored, and size will be inferred from the level_IDs in the data.")
        }
        
        population_function <- make_population_data_function(
          expressions = expressions,
          data = data,
          global_transformations = global_transformations,
          options = options,
          level_IDs = level_IDs, 
          make_unique_ID = make_unique_ID
        )
      } else {
        
        population_function <- function() return(data)
        
      }
    }
    
    
  } 
  
  return_object <- list(population = population_function, 
                        description = description,
                        call = match.call())
  class(return_object) <- "population"
  
  return(return_object)
  
}

make_population_function <- function(
  expressions,
  size,
  level_IDs,
  global_transformations,
  options,
  make_unique_ID
){
  
  if(length(expressions) == 0){
    expressions <- list(unit = list())
  }
  
  # Make sure that all of the levels are properly named
  expressions <- make_level_names(expressions = expressions,
                                  level_IDs = level_IDs)
  
  # Start creating custom function here
  make_population <- function(
    size, 
    options 
  ){
    
    # If the defaults are missing, grab them from the environment of the 
    # function, defined below
    if(!missing(size)){
      size_internal <- size
    } else {
      size_internal <- get("size",envir = make_pop_env)
    }
    
    if(!missing(options)){
      options_internal <- options 
    } else {
      options_internal <- get("options",envir = make_pop_env)
    }
    
    # Infer the data structure from the size argument
    hierarchy <- get_hierarchy(size = size_internal)
    
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
      MoreArgs = list(options = options_internal)
    )
    
    # At each level, make_data_frame takes an environment and
    # - evaluates all of the expressions specific to that level
    # - removes all of the objects that aren't variables
    # - coerces the resultant stuff to a data.frame 
    # So this produces a list of data.frames:
    data_list <- lapply(X = environ_list,
                        FUN = make_data_frame,
                        options = options_internal)
    
    # Create a list of IDs of the level higher for each lower level, 
    # for use in merging (in multi-level cases)
    merge_vars <- mapply(FUN = make_merge_ID, 
                         group_sizes = group_sizes[-1],
                         level_ID = level_IDs[-1],
                         data_structure = data_structure[-1]
    )
    
    # Name the IDs that will be used for merging (in multi-level cases)
    if(length(merge_IDs)==1){
      merge_vars <- data.frame(merge_vars)
    }
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
      options = options_internal)
    
    # And evaluate them, returning a data frame
    return_data <- make_data_frame(temp_env = return_env,
                                   options = options_internal)
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
  # but only if it doesn't have values provided for .size, .options
  make_pop_env <- list2env(
    list(expressions = expressions,
         size = size,
         level_IDs = level_IDs,
         global_transformations = global_transformations,
         options = options,
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

make_environ <- function(data_structure,exprs,options){
  temp_env <- list2env(data_structure)
  temp_env$n_ <- nrow(data_structure)
  temp_env$expressions <- exprs
  if(!is.null(options)){
    if(is.null(names(options))){
      stop("Elements of the options list should be named.")
    }
    list2env(x = options,envir = temp_env)
  }
  return(temp_env)
}

make_data_frame <- function(temp_env,options){
  other_names <- names(options)
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
  suppressWarnings(rm(list = c("n_","expressions",other_names,"temp_var"),envir = temp_env))
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
    
    
    # Generate N_per_level and N
    N <- sum(group_sizes_per_level[[1]])
    
    # Check if first level is all 1's
    all_ones <- all(unique(group_sizes_per_level[[1]])==1)
    
    # If not, add in the ones level
    if(!all_ones){
      first_level <- list(rep(1,N))
      group_sizes_per_level <- c(first_level,group_sizes_per_level)
    }
    
    # Test that the structure is logical
    if(length(group_sizes_per_level)>1){
      lower_units_test <- sapply(length(group_sizes_per_level):2,
                                 function(i){
                                   sum(group_sizes_per_level[[i]])==
                                     length(group_sizes_per_level[[i-1]])})
      
      if(!all(lower_units_test)){
        stop("The argument supplied to group_sizes_per_level is not logical. The sum of every higher level should be equal to the length of the preceding lower level. For example, in a study with 4 units and 2 groups, group_sizes_per_level = list(c(1,1,1,1),c(2,2)).")
      }
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
  options = NULL
){
  
  if(!any("size" %in% names(formals(custom_population_function))))
    stop("Your custom data function must include a size argument.")
  
  custom_pop_env <- list2env(
    list(size_internal = size,
         options_internal = options,
         data_internal = data)
  )
  
  make_population <- function(size,options,data) {
    
    if(!missing(size)){
      size_internal <- size
    } 
    
    if(!missing(data)){
      data_internal <- data
    } 
    
    if(!missing(options)){
      options_internal <- options 
    } 
    
    function_args <- list(size = size_internal, 
                       data = data_internal, 
                       options =  options_internal)
    
    function_args <- get_custom_args(options = function_args,
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
    list(size_internal = size,
         level_IDs = level_IDs,
         data = data)
  )
  
  population_function <- function(size){
    
    if(!missing(size)){
      size_internal <- size
    } 
    
    bootstrap_data(data = data, size = size_internal, level_IDs = level_IDs)
    
  }
  
  environment(population_function) <- boot_pop_env
  
  return(population_function)
  
}

get_custom_args <- function(options,custom_function){
  arg_list <- options[names(options) %in% names(formals(custom_function))]
  return(arg_list)
}






make_population_data_function <- function(
  expressions,
  data,
  global_transformations,
  options,
  level_IDs,
  make_unique_ID
){
  # Make sure that all of the levels are properly named
  expressions <- make_level_names(expressions = expressions,
                                  level_IDs = level_IDs)
  
  if(is.null(level_IDs)){
    stop("If you wish to generate new variables within an existing dataset, please provide the names of the variables that indicate levels to level_IDs.")
  }
  
  IDs_in_data <- all(level_IDs %in% names(data))
  
  if(!IDs_in_data){
    stop("All of the level_IDs should correspond to variable names in the data.")
  }
  
  if(length(expressions) != length(level_IDs)){
    stop("The number of levels for which you declare (possibly empty) lists of variables must be the same as the number of level_IDs that you have indicated (i.e. length(list(...)) == length(level_IDs) must be true).")
  }
  
  
  
  N_per_level <- sapply(level_IDs,function(ID) length(unique(data[,ID])))
  N_per_level <- N_per_level[order(N_per_level,decreasing = T)]
  level_IDs <- names(N_per_level)
  
  
  if(nrow(data) > N_per_level[1]){
    level_IDs <- c("unit_ID",level_IDs)
    data$unit_ID <- 1:nrow(data)
  }
  
  group_sizes_per_level <- list()
  
  group_sizes_per_level$level_1 <- get_group_per_level(
    lower_level = level_IDs[1],
    upper_level = level_IDs[1],
    data = data
  )
  
  for (i in 2:length(level_IDs)) {
    group_sizes_per_level[[i]] <- get_group_per_level(
      lower_level = level_IDs[i-1],
      upper_level = level_IDs[i],
      data = data
    )
  }
  
  hierarchy <- get_hierarchy(size = group_sizes_per_level)
  
  N_levels <- hierarchy$N_levels
  N <- hierarchy$N
  N_per_level <- hierarchy$N_per_level
  group_sizes <- hierarchy$group_sizes_per_level
  
  # Start creating custom function here
  make_population <- function(){
    
    # Get the IDs that are not the first level, to merge by
    merge_IDs <- level_IDs[-1]
    
    # make_structure goes through and creates an ID for each level in a list,
    # this can be thought of as the "skeleton" of the data
    data_structure <- lapply(level_IDs,function(id){
      ID <- data.frame(unique(data[,id]))
      names(ID) <- id
      return(ID)
    })
    
    # at each level, make_environ creates an environment with the n_ object, 
    # other arguments, and all the expressions specific to a level
    environ_list <- mapply(
      FUN = make_environ,
      data_structure = data_structure,
      exprs = expressions,
      MoreArgs = list(options = options)
    )
    
    # At each level, make_data_frame takes an environment and
    # - evaluates all of the expressions specific to that level
    # - removes all of the objects that aren't variables
    # - coerces the resultant stuff to a data.frame 
    # So this produces a list of data.frames:
    data_list <- lapply(X = environ_list,
                        FUN = make_data_frame,
                        options = options)
    
    # Create a list of IDs of the level higher for each lower level, 
    # for use in merging (in multi-level cases)
    merge_vars <- mapply(FUN = make_merge_ID, 
                         group_sizes = group_sizes[-1],
                         level_ID = level_IDs[-1],
                         data_structure = data_structure[-1]
    )
    
    # Name the IDs that will be used for merging (in multi-level cases)
    if(length(merge_IDs)==1){
      merge_vars <- data.frame(merge_vars)
    }
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
    
    
    drop_vars <- names(return_data) %in% level_IDs[-1]
    return_data <- return_data[!drop_vars]
    return_data <- merge(x = data,y = return_data,by = level_IDs[1])
    
    # Now create the environment for evaluating all of the global transformatons
    return_env <- make_environ(
      data_structure = return_data,
      exprs = global_transformations,
      options = options)
    
    # And evaluate them, returning a data frame
    return_data <- make_data_frame(temp_env = return_env,
                                   options = options)
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
  # but only if it doesn't have values provided for .size, .options
  make_pop_env <- list2env(
    list(expressions = expressions,
         hierarchy = hierarchy,
         level_IDs = level_IDs,
         global_transformations = global_transformations,
         options = options,
         make_unique_ID = make_unique_ID)
  )
  
  environment(make_population) <- make_pop_env
  
  return(make_population)
  
}


get_group_per_level <- function(lower_level,upper_level,data){
  tapply(X = data[,lower_level],INDEX = data[,upper_level],
         FUN = function(x)length(unique(x)))
}
























