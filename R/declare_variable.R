#' Declare the data-generating process of a variable
#'
#' @param linear_mean If the variable is linear 
#' @export
declare_variable <-
  function(linear_mean = NULL,linear_sd = NULL,binary_probability = NULL, binary_categories = NULL, multinomial_probabilities = NULL, multinomial_categories = NULL, transformation = NULL) {
    
    type_check <- list(
      normal = c(linear_mean,linear_sd),
      binary = c(binary_probability,binary_categories),
      multinomial = c(multinomial_probabilities,multinomial_categories)
    )
    
    type_lengths <-
      sapply(type_check,length)
    
    if (sum(type_lengths > 0) > 1) {
      stop(
        "Please enter parameters for only one kind of variable (linear, binary or multinomial)."
      )
    }
    
    if (!is.null(linear_mean)) {
      if (!is.null(linear_sd)) {
        variable <- list(distribution = "normal",
                         mean = linear_mean,
                         sd = linear_sd)
      }else{
        variable <- list(distribution = "normal",
                         mean = linear_mean,
                         sd = linear_mean * .10 + 1)
      }
    }
    
    if (!is.null(binary_probability)) {
      if (!is.null(binary_categories)) {
        variable <- list(
          distribution = "binary",
          probability = binary_probability,
          categories = binary_categories
        )
      }else{
        variable <- list(
          distribution = "binary",
          probability = binary_probability,
          categories = c(0,1)
        )
      }
    }
    
    if (!is.null(multinomial_probabilities)) {
      if (!is.null(multinomial_categories)) {
        variable <- list(
          distribution = "multinomial",
          probability = multinomial_probabilities,
          categories = multinomial_categories
        )
      }else{
        variable <- list(
          distribution = "multinomial",
          probability = multinomial_probabilities,
          categories = 1:length(multinomial_probabilities)
        )
      }
    }
    
    if (sum(type_lengths > 0) == 0) {
      variable <- list(distribution = "normal",
                       mean = 0,
                       sd = 1)
    }
    
    if (!is.null(transformation)) {
      variable <- c(variable, transformation = transformation)
    }
    
    class(variable) <- "DGP_object"
    
    return(variable)
    
  }
