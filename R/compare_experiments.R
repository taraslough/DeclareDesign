
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
compare_experiments <- function(N = NULL, N_clusters = NULL, N_per_cluster = NULL,
                                design = NULL, analysis = NULL, sample_frame = NULL, potential_outcomes = NULL,
                                blocks = NULL, clusters = NULL, sims = 5){
  
  if(sum(c(!is.null(N), !is.null(N_clusters), !is.null(N_per_cluster))) > 1)
    stop("Please provide one of N, N_clusters, or N_per_cluster.")
  
  if(class(design) == "design")
    design <- list(design)
  if(class(analysis) == "analysis")
    analysis <- list(analysis)
  if(class(sample_frame) == "sample_frame")
    sample_frame <- list(sample_frame)
  if(class(potential_outcomes) == "outcomes_object")
    potential_outcomes <- list(potential_outcomes)
  if(class(blocks) == "blocks")
    blocks <- list(blocks)
  if(class(clusters) == "clusters")
    clusters <- list(clusters)
  
  if(is.null(design))
    stop("Please provide a design object created using the declare_design() function.")
  
  comparison_counts <- c(length(N), length(N_per_cluster), length(N_clusters), 
                         length(design), length(analysis), length(sample_frame), 
                         length(potential_outcomes))
  if(any(comparison_counts != max(comparison_counts) & comparison_counts > 1))
    stop("Please provide either no inputs for a given option (for example, not providing any N), one input (for example a single value of N), or more than one input (for example N = c(500, 1000)). The set of varying inputs (i.e. for N and m) must be the same length (you cannot provide three values of N and two values of n).")
  
  design_compare <- design
  analysis_compare <-  analysis
  sample_frame_compare <- sample_frame
  potential_outcomes_compare <- potential_outcomes
  blocks_compare <- blocks
  clusters_compare <- clusters
  
  ## loop over different configurations of experiments
  comparisons <- list()
  for(e in 1:max(comparison_counts)){
    
    input_changes <- c()
    if(!is.null(N))
      input_changes <- c(input_changes, "N")
    
    if(length(input_changes) > 0){
      ## loop over the inputs that need to be changed, and change them in temporary objects
      for(i in input_changes){
        ## for each do some if statements to turn on / off depending on whether the input is relevant
        
        ## temp var
        ##N_e <- sample_frame[[e]]$N_per_level[1]
        
        if(!is.null(clusters[[e]])){
          stop("this doesn't work! cluster compare exp")
          if(!is.null(N_per_cluster))
            N_clusters_tmp <- N_e / N_per_cluster
          if(!is.null(N_clusters))
            N_per_cluster_tmp <- N_e / N_clusters
        }
        
        sample_frame_compare[[min(length(sample_frame_compare), e)]] <- substitute_input(sample_frame_compare[[min(length(sample_frame_compare), e)]], "N", N[e])
        sample_frame_compare[[min(length(sample_frame_compare), e)]] <- substitute_input(sample_frame_compare[[min(length(sample_frame_compare), e)]], "N_per_level", NULL)
        sample_frame_compare[[min(length(sample_frame_compare), e)]] <- substitute_input(sample_frame_compare[[min(length(sample_frame_compare), e)]], "lower_units_per_level", NULL)
        
      }
    }
    
    ##comparisons[[e]] <- list()
    
    ##comparisons[[e]]$simulations <- 
    comparisons[[e]] <-   simulate_experiment(design = design_compare[[min(length(design_compare), e)]], 
                                              analysis = analysis_compare[[min(length(analysis_compare), e)]], 
                                              sample_frame = sample_frame_compare[[min(length(sample_frame_compare), e)]], 
                                              potential_outcomes = potential_outcomes_compare[[min(length(potential_outcomes_compare), e)]], 
                                              blocks = blocks_compare[[min(length(blocks_compare), e)]], 
                                              clusters = clusters_compare[[min(length(clusters_compare), e)]],
                                              sims=sims)
    
    ##comparisons[[e]]$values <- list(design = design_compare[[min(length(design_compare), e)]], 
    ##                                analysis = analysis_compare[[min(length(analysis_compare), e)]], 
    ##                                sample_frame = sample_frame_compare[[min(length(sample_frame_compare), e)]], 
    ##                                potential_outcomes = potential_outcomes_compare[[min(length(potential_outcomes_compare), e)]], 
    ##                                blocks = blocks_compare[[min(length(blocks_compare), e)]], 
    ##                                clusters = clusters_compare[[min(length(clusters_compare), e)]]) ## put values of inputs that are changing here, i.e. N and m
    
  }
  
  class(comparisons) <- "experiment_comparisons"
  
  return(comparisons)
  
}



#' @export
summary.experiment_comparisons <- function(object, ...) {
  
  sum_list <- list()
  
  for(i in 1:length(object)){
    sum_list[[i]] <- summary(object[[i]])
  }
  return_object <- do.call(rbind, sum_list)
  
}

#' @export
print.summary.experiment_simulations <- function(x, ...){
  ## prints paragraph describing design
}


#' @export
print.experiment_comparisons <- function(x, ...){
  print(summary.experiment_comparisons(x, ... = ...))
  return()
}

#' Plot power across values of a parameter like the sample size
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param vary_parameter indicating which parameter is varied, default is "N"
#' @param vary_sequence set of values of the parameter to calculate power for
#' @param sims number of iterations
#' @export
plot_power <- function(data, design, analysis, vary_parameter = "N", vary_sequence){
  
  if(vary_parameter != "N")
    stop("The power analysis plot is only implemented for varying N at first.")
  
  if(missing(vary_sequence)){
    vary_sequence <- seq(from = 0, to = 1, by = .1)
  }
  
  power_sequence <- rep(NA, length(vary_sequence))
  for(parameter in vary_sequence)
    power_sequence[i] <- power(data = data, design = design, analysis = analysis,
                               N = parameter)
  
  return(power_sequence)
  ##plot(vary_sequence, power_sequence, axes = F, las = 1)
  ##axis(1)
  ##axis(2)
  
}



substitute_input <- function(object, input_name, input_value){
  call <- object$call
  
  ## replace thing in the call
  if(!(is.null(input_value) & is.null(call[[input_name]])))
    call[[input_name]] <- input_value
  
  ##which_input <- names(call) == input_name
  
  ##if(any(which_input)>0)
  ##  call[[which_input]] <- input_value
  
  eval(call)
}


