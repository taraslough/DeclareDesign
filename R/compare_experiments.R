
#' Declare the data-generating process of a variable
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param sims number of iterations
#' @examples 
#'   ## imagined options:
#'   ## compare_experiments(N = c(500, 1000, 2000), m = 5, design = design, etc.)
#'   ## compare_experiments(N = c(500, 1000, 2000), m = c(5, 10, 15), design = design, etc.)
#'   ## ideas: 
#'   ## if user wants to change N and provides data, bootstrap data to that size
#' @export
compare_experiments <- function(N, m,
                                design, analysis = NULL, sample_frame = NULL, potential_outcomes = NULL,
                                blocks = NULL, clusters = NULL, data = NULL, 
                                bootstrap_data = FALSE, N_bootstrap, sims = 5){
  
  if(missing(design))
    stop("Please provide a design object created using the declare_design() function.")
  
  comparison_counts <- c(length(N), length(m))
  if(any(comparison_counts != max(comparison_counts) & comparison_counts > 1))
    stop("Please provide either no inputs for a given option (for example, not providing any N), one input (for example a single value of N), or more than one input (for example N = c(500, 1000)). The set of varying inputs (i.e. for N and m) must be the same length (you cannot provide three values of N and two values of n).")
  
  design_compare <- design
  
  ## loop over different configurations of experiments
  comparisons <- list()
  for(e in 1:max(lengths)){
    
    input_changes <- c()
    if(!is.null(N))
      input_changes <- c(input_changes, "N")
    
    ## loop over the inputs that need to be changed, and change them in temporary objects
    for(i in input_changes){
      ## for each do some if statements to turn on / off depending on whether the input is relevant
      design_compare <- substitute_input(design_compare, input_changes[i], N[e])
      
      if(!is.null(analysis))
        analysis_compare <- substitute_input(analysis_compare, input_changes[i], N[e])
      
      if(!is.null(sample_frame))
        sample_frame_compare <- substitute_input(sample_frame_compare, input_changes[i], N[e])
      
      if(!is.null(potential_outcomes))
        potential_outcomes_compare <- substitute_input(potential_outcomes_compare, input_changes[i], N[e])
      
      if(!is.null(blocks))
        blocks_compare <- substitute_input(blocks_compare, input_changes[i], N[e])
      
      if(!is.null(clusters))
        clusters_compare <- substitute_input(clusters_compare, input_changes[i], N[e])
    }
    
    comparisons[[e]]$simulations <- simulate_experiment(design = design_compare, analysis = analysis_compare, 
                                                        sample_frame = sample_frame_compare, 
                                                        potential_outcomes = potential_outcomes_compare, 
                                                        blocks = blocks_compare, clusters = clusters_compare, 
                                                        data = data)
    
    comparisons[[e]]$values <- list() ## put values of inputs that are changing here, i.e. N and m
    
  }
  
  return(comparisons)
  
}


substitute_input <- function(object, input_name, input_value){
  call <- x$call
  
  ## replace thing in the call
  call[,input_name] <- input_value
  
  which_input <- names(call) == input_name
  
  if(any(which_input)>0)
    call[[which_input]] <- input_value
  
  eval(call)
}

