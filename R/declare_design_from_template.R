#' @export
declare_design_from_template <- function(template, ...){
  
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
  
  design <- do.call(template, args = options)
  
  if (class(design) != "design") {
    stop("The template function you chose did not create a design object. Please choose another template function.")
  }

  return(design)
  
}
