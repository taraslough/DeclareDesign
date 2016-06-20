#' Quick Design Generation Function
#' 
#' @param template Template function to create a set of designs to compare
#' @param intersection Describes how to 
#' @param ... options for the template function. If you wish to compare designs over a range of values of a template input, use vary().
#' 
#' @export
quick_design <- function(template, intersection = NULL, ...){
  
  if (missing(template)) {
    stop("You must specify a template function in the template argument.")
  }
  
  argument_names <- names(formals(template))
  options <- list(...)
  
  if (length(options) > 0) {
    for (i in 1:length(options)) {
      if (names(options)[[i]] %in% argument_names) {
        options[[names(options)[[i]]]] <- options[[i]]
      } else {
        stop(paste("The argument", names(options)[[i]], "is not used by this template. Please remove it."))
      }
    }
  }
  
  is_variable <- 
    sapply(options, function(option) class(option) == "vary")
  
  if(any(is_variable)){ # At least one arg to vary over
    
    constant_options <- options[!is_variable]
    variable_options <- options[is_variable]
    
    variable_lengths <- sapply(variable_options,length)
    
    intersection <- set_intersection_warn_user(intersection = intersection,
                                               variable_lengths = variable_lengths)
    
    variable_indices <- get_indices(
      intersection = intersection,
      length_list = variable_lengths)
    
    design_list <- list()
    
    for(i in 1:nrow(variable_indices)){
      arg_list <- get_arg_list(
        constants = constant_options,
        variables = variable_options,
        variable_index = variable_indices[i,])
      
      design_list[[i]] <- do.call(template, args = arg_list)
      
      class(design_list[[i]]) <- "design"
    }
    
  } else { # Only constants supplied
    design_list <- do.call(template, args = options)
  }
  
  return(design_list)
  
}


#' @export
vary <- function(...){
  vary_list <- list(...)
  class(vary_list) <- "vary"
  return(vary_list)
}

#' @export
get_indices <- function(intersection,length_list){
  indices <- as.list(parse(text = paste0("1:",length_list)))
  names(indices) <- names(length_list)
  
  if(intersection){
    index_grid <- do.call("expand.grid",indices)
  } else {
    index_grid <- do.call("data.frame",indices)
  }
  
  return(index_grid)
}

#' @export
get_arg_list <- function(constants,variables,variable_index){
  variable_args <- mapply(
    FUN = function(variable_list,index){
      variable_list[index]
    },
    variable_list = variables,
    index = variable_index
  )
  return(c(constants,variable_args))
}


set_intersection_warn_user <- function(intersection, variable_lengths){
  
  same_length <- length(unique(variable_lengths)) == 1
  
  if(is.null(intersection)){ # User has not set intersection
    
    if(same_length){ # All variables are of same length
      
      intersection <- FALSE
      
      N_designs <- unique(variable_lengths)
      
      user_message <- 
        paste0("All arguments passed through vary(...) are of same length, so quick_design() will set intersection to FALSE by default and create ", N_designs, " unique design(s). \nIf you would like one design per unique combination of arguments passed through vary(...), set intersection = TRUE.")
      
      
    } else{ # Variables are of different lengths 
      
      intersection <- TRUE
      
      N_designs <- prod(variable_lengths)
      
      user_message <- 
        paste0("Arguments passed to quick_design() through vary(...) are of different lengths, so quick_design() will set intersection to TRUE and create ", N_designs, " unique design(s).")
      
    }
  } else { # User has set intersection 
    
    if(!intersection & !same_length) { # User has set intersection = FALSE but the variables are of differing lengths
      
      user_message <- paste0(
        "The arguments you have passed to quick_design() have the following lengths: ",paste(names(variable_lengths), "=",variable_lengths,collapse = ", "),", but you have set intersection = FALSE. Please ensure that either all variables have the same length or intersection = TRUE. Note that if you set intersection = TRUE, quick_design() will return one design for each unique combination of the variables (", prod(variable_lengths)," designs).")
      
      stop(user_message)
      
    }
    if(intersection){ # User has set intersection = TRUE and vars are unequal or equal
      
      N_designs <- prod(variable_lengths)
      
      user_message <- 
        paste0("You have set intersection = TRUE, so quick_design() will create ", N_designs, " design(s) out of the unique combinations of the arguments passed to it through vary(...).")
      
    } 
    if(!intersection & same_length){ # User has set intersection = FALSE and vars are equal
      
      N_designs <- unique(variable_lengths)
      
      user_message <- 
        paste0("You have set intersection = FALSE, so quick_design() will create ", N_designs, " design(s) as it iterates through the arguments passed to it through vary(...). Use intersection = TRUE to make one design for each unique combination of the variables (", prod(variable_lengths)," designs).")
    }
  }
  message(user_message)
  return(intersection)
}



