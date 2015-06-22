#' Declare the data-generating process of a variable
#'
#' @param linear_mean If the variable is linear 
#' @export
declare_variable <- function(
  linear_mean        = NULL,
  linear_sd          = NULL,
  binary_probability = NULL,
  binary_categories = NULL,
  multinomial_probabilities = NULL,
  multinomial_categories = NULL
){
    if(sum(!unlist(lapply(
      X = list(
        normal = list(linear_mean,linear_sd),
        binary = list(binary_probability,binary_categories),
        multinomial = list(multinomial_probabilities,multinomial_categories)),
      FUN = function(i)sum(unlist(lapply(i,is.null))))) %in% 2) > 1){stop(
        "Please enter parameters for only one kind of variable (linear, binary or multinomial).")}
    if(!is.null(linear_mean)){
      if(!is.null(linear_sd)){
        variable <- list(
          distribution = "normal",
          mean = linear_mean,
          sd = linear_sd
        )
      }else{
      variable <- list(
        distribution = "normal",
        mean = linear_mean,
        sd = linear_mean*.10+1
      )
      }
    }
    if(!is.null(binary_probability)){
      if(!is.null(binary_categories)){
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
    if(!is.null(multinomial_probabilities)){
      if(!is.null(multinomial_categories)){
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
    if(!FALSE %in% unlist(lapply(
      X = list(
        normal = list(linear_mean,linear_sd),
        binary = list(binary_probability,binary_categories),
        multinomial = list(multinomial_probabilities,multinomial_categories)),
      FUN = function(i)unlist(lapply(i,is.null)))) 
    ){
        variable <- list(
          distribution = "normal",
          mean = 0,
          sd = 1
        )    
      }
    class(variable) <- "DGP_object"
    return(variable)
  }


