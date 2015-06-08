#' Declare the data-generating process of a variable
#'
#' @param variable_name The name of your new variable 
#' @param linear_mean If the variable is linear 
#' @export
declare_DGP <- function(
  # variable_name = "my_variable",
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
        variable <- list(#name = variable_name,
          distribution = "normal",
          mean = linear_mean,
          sd = linear_sd
        )
      }
      variable <- list(#name = variable_name,
        distribution = "normal",
        mean = linear_mean,
        sd = linear_mean*.10+1
      )
    }
    if(!is.null(binary_probability)){
      if(!is.null(binary_categories)){
        variable <- list(#name = variable_name,
          distribution = "binary",
          probability = binary_probability,
          categories = binary_categories
        )
      }else{
        variable <- list(#name = variable_name,
          distribution = "binary",
          probability = binary_probability,
          categories = c(0,1) # When it's binary, check if cats are numeric or 
          # string. Make factor when string.  
        )
      }
    }
    if(!is.null(multinomial_probabilities)){
      if(!is.null(multinomial_categories)){
        variable <- list(#name = variable_name,
          distribution = "multinomial",
          probability = multinomial_probabilities,
          categories = multinomial_categories
        )
      }else{
        variable <- list(#name = variable_name,
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
        variable <- list(#name = variable_name,
          distribution = "normal",
          mean = 0,
          sd = 1
        )    
      }
    class(variable) <- "DGP_object"
    return(variable)
  }


# DEMO


declare_DGP()

declare_DGP(linear_mean = 1000,
            linear_sd = 100)

declare_DGP(variable_name = "Event",
            binary_probability = .8,
            binary_categories = c("Did not happen","Happened")
)

declare_DGP(variable_name = "Party_ID",
            multinomial_probabilities = c(.4,.4,.2),
            multinomial_categories = c("D","R","I")
)











