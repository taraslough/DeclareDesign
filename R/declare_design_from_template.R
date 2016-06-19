

#' Compare Designs from Template Function
#' 
#' @param template Template function to create a set of designs to compare
#' @param ... options for the template function over which to compare designs
#' 
#' @export
compare_designs_from_template <- function(template, ...){
  
  if (missing(template)) {
    stop("You must specify a template function in the template argument.")
  }
  
  argument_names <- names(formals(template))
  options <- list(...)
  
  if(length(options)>2){
    stop("You may only compare designs across one dimension.")
  }
  
  if (length(options) > 0) {
    for (i in 1:length(options)) {
      if (names(options)[[i]] %in% argument_names) {
        options[[names(options)[[i]]]] <- options[[i]]
      } else {
        stop(paste("The argument", names(options)[[i]], "is not used by this template. Please remove it."))
      }
    }
  }
  
  expand_options <- expand.grid(options)
  
  design_list <- list()
  
  for(i in 1:nrow(expand_options)){
    design_list[[i]] <- do.call(template, args = as.list(expand_options[i,]))
  }
  
  return(design_list)
  
}

