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

