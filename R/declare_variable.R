#' Declare the data-generating process of a variable
#'
#' @param normal_mean If the variable is normally distributed, its mean
#' @param normal_sd If the variable is normally distributed, its standard deviation
#' @param binary_probability How does this work?
#' @param binary_categories How does this work?
#' @param multinomial_probabilities How does this work?
#' @param multinomial_categories How does this work?
#' @param transformation How does this work?
#' @export
declare_variable <-
  function(normal_mean = NULL,normal_sd = NULL,binary_probability = NULL, binary_categories = NULL, multinomial_probabilities = NULL, multinomial_categories = NULL, transformation = NULL) {
    
    type_check <- list(
      normal = c(normal_mean,normal_sd),
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
    
    if (!is.null(normal_mean)) {
      if (!is.null(normal_sd)) {
        variable <- list(distribution = "normal",
                         mean = normal_mean,
                         sd = normal_sd)
      }else{
        variable <- list(distribution = "normal",
                         mean = normal_mean,
                         sd = normal_mean * .10 + 1)
      }
    }
    
    if (!is.null(binary_probability)) {
      if(binary_probability>1|binary_probability<0){
        stop("binary_probability must be in the interval [0,1].")
      }
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
      
      if(sum(multinomial_probabilities)!=1){
        stop("multinomial_probabilities must sum to 1.")
      }
      
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
