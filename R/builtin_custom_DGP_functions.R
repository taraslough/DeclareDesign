#' Declare the data-generating process of a variable
#'
#' @param normal_mean If the variable is normally distributed, its mean
#' @param normal_sd If the variable is normally distributed, its standard deviation
#' @param binary_probability The probability of success when the outcome is binary.
#' @param binary_categories The failure and success labels, respectively.
#' @param multinomial_probabilities A vector of probabilities of different draws, as long as there are categories.
#' @param multinomial_categories A vector of labels for the multinomial draws
#' @param transformation An optional string argument that can perform any operation on the covariates in \code{\link{declare_population}}, provided that: the transformations return a vector of the same length as the dataframe; the transformations only involve variables on one level; and the transformations take place successively (i.e. only variables or transformations which are declared in previous lines can themselves be transformed).
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



# A function for grabbing variables at a given level in multi-level structures
#' @export
get_variable <- function(
  level_ID, 
  variable_name, 
  data, 
  aggregate_function = NULL,
  other_arguments = NULL
){
  default_fun <- "function(x)unique(x)[1]"
  
  if(is.null(aggregate_function)){
    aggregate_function_string <- default_fun
  } else {
    aggregate_function_string <- as.character(substitute(aggregate_function))
  }
  
  data_string <- as.character(substitute(data))
  variable_name_string <- as.character(substitute(variable_name))
  level_ID_string <- as.character(substitute(level_ID))
  other_arguments_string <- as.character(substitute(other_arguments))
  
  expr <- paste0(
    "with(",data_string,",tapply(X = ",variable_name_string,",INDEX = ",level_ID_string,
    ",FUN = ",aggregate_function_string,other_arguments_string,"))"
  )
  return(expr)
}

#' @export
make_proportions <- function(population_proportions, N){
  
  counts <- apply(population_proportions,2,rmultinom,n = 1,size = N)
  
  con_names <- rownames(population_proportions)
  
  outcomes <- apply(counts,2,function(times){
    sample(
      rep(con_names,times = times)
    )
  })
  
  colnames(outcomes) <- colnames(population_proportions)
  
  outcomes <- integerize(as.data.frame(outcomes))
  
  return(outcomes)
  
}
