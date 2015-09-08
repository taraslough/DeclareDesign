
#' Declare the data-generating process of a variable
#'
#' @param N what is it?
#' @param N_per_level  what is it?
#' @param group_sizes_by_level  what is it?
#' @param design what is it?
#' @param analysis  what is it?
#' @param sample what is it?
#' @param potential_outcomes what is it?
#' @param blocks what is it?
#' @param clusters what is it?
#' @param sims what is it?
#' @param labels labels for each simulation
#' @param analysis_labels labels for each analysis
#' @examples 
#'   ## here are examples
#' @export
compare_experiments <- function(N = NULL, N_per_level = NULL, group_sizes_by_level = NULL,
                                design = NULL, analysis = NULL, sample = NULL, potential_outcomes = NULL,
                                blocks = NULL, clusters = NULL, sims = 5, labels = NULL, analysis_labels = NULL){
  
  if(is.null(blocks))
    blocks <- design$blocks
  if(is.null(clusters))
    clusters <- design$clusters
  
  if(class(design) == "design")
    design <- list(design)
  if(class(analysis) == "analysis")
    analysis <- list(analysis)
  if(class(sample) == "sample")
    sample <- list(sample)
  if(class(potential_outcomes) == "potential_outcomes")
    potential_outcomes <- list(potential_outcomes)
  if(class(blocks) == "blocks")
    blocks <- list(blocks)
  if(class(clusters) == "clusters")
    clusters <- list(clusters)
  
  if(sum(!is.null(N), !is.null(N_per_level), !is.null(group_sizes_by_level)) > 1)
    stop("Please provide either N, N_per_level, or group_sizes_by_level (not more than one).")
  
  if(is.null(design))
    stop("Please provide a design object created using the declare_design() function.")
  
  if(is.null(analysis_labels)){
    if(class(analysis) == "list")
      analysis_labels <- paste(substitute(analysis)[-1L])
    else
      analysis_labels <- paste(substitute(analysis))
  }
  
  comparison_counts <- c(length(N), length(N_per_level), length(group_sizes_by_level), 
                         length(design), length(analysis), length(sample), 
                         length(potential_outcomes))
  if(any(comparison_counts != max(comparison_counts) & comparison_counts > 1))
    stop("Please provide either no inputs for a given option (for example, not providing any N), one input (for example a single value of N), or more than one input (for example N = c(500, 1000)). The set of varying inputs (i.e. for N and m) must be the same length (you cannot provide three values of N and two values of n).")
  
  design_compare <- design
  analysis_compare <-  analysis
  sample_compare <- sample
  potential_outcomes_compare <- potential_outcomes
  blocks_compare <- blocks
  clusters_compare <- clusters
  
  ## loop over different configurations of experiments
  comparisons <- list()
  for(e in 1:max(comparison_counts)){
    
    if((!is.null(N) & 
        exists_input(sample_compare[[min(length(sample_compare), e)]], "N")) | 
       (!is.null(N_per_level) & 
        exists_input(sample_compare[[min(length(sample_compare), e)]], "N_per_level")) | 
       (!is.null(group_sizes_by_level) & 
        exists_input(sample_compare[[min(length(sample_compare), e)]], "group_sizes_by_level")))
      stop("When N, N_per_level, or group_sizes_by_level is specified in compare_experiment, you can only specify the one that was used in the original declare_sample call. For instance, if you specified N in declare_sample, you can only vary N in compare_experiment.")
    
    if(!(is.null(N) & is.null(N_per_level) & is.null(group_sizes_by_level))){
      sample_compare[[min(length(sample_compare), e)]] <- substitute_input(sample_compare[[min(length(sample_compare), e)]], "N", N[e])
      sample_compare[[min(length(sample_compare), e)]] <- substitute_input(sample_compare[[min(length(sample_compare), e)]], "N_per_level", N_per_level[[e]])
      sample_compare[[min(length(sample_compare), e)]] <- substitute_input(sample_compare[[min(length(sample_compare), e)]], "group_sizes_by_level", group_sizes_by_level[[e]])
    }
    
    comparisons[[e]] <- list()
    
    comparisons[[e]]$simulations <-   get_diagnostics(design = design_compare[[min(length(design_compare), e)]], 
                                                          analysis = analysis_compare[[min(length(analysis_compare), e)]], 
                                                          sample = sample_compare[[min(length(sample_compare), e)]], 
                                                          potential_outcomes = potential_outcomes_compare[[min(length(potential_outcomes_compare), e)]], 
                                                          blocks = blocks_compare[[min(length(blocks_compare), e)]], 
                                                          clusters = clusters_compare[[min(length(clusters_compare), e)]],
                                                          sims=sims, label = labels[[e]], analysis_labels = analysis_labels)
    
    comparisons[[e]]$values <- list(design = design_compare[[min(length(design_compare), e)]], 
                                    analysis = analysis_compare[[min(length(analysis_compare), e)]], 
                                    sample = sample_compare[[min(length(sample_compare), e)]], 
                                    potential_outcomes = potential_outcomes_compare[[min(length(potential_outcomes_compare), e)]], 
                                    blocks = blocks_compare[[min(length(blocks_compare), e)]], 
                                    clusters = clusters_compare[[min(length(clusters_compare), e)]])
    
  }
  
  class(comparisons) <- "experiment_comparisons"
  
  return(comparisons)
  
}

#' @export
summary.experiment_comparisons <- function(object, ...) {
  
  sum_list <- list()
  
  for(i in 1:length(object)){
    sum_list[[i]] <- summary(object[[i]]$simulations)
  }
  return_object <- do.call(rbind, sum_list)
  
}

#' @export
print.summary.experiment_simulations <- function(x, ...){
  print(x)
  return()
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
plot_power <- function(data, design, analysis, vary_parameter = "N", vary_sequence, sims = 100){
  
  if(vary_parameter != "N")
    stop("The power analysis plot is only implemented for varying N at first.")
  
  if(missing(vary_sequence)){
    vary_sequence <- seq(from = 0, to = 1, by = .1)
  }
  
  power_sequence <- rep(NA, length(vary_sequence))
  for(parameter in vary_sequence)
    power_sequence[parameter] <- get_diagnostics(data = data, design = design, analysis = analysis,
                                       N = parameter)
  
  return(power_sequence)
  ##plot(vary_sequence, power_sequence, axes = F, las = 1)
  ##axis(1)
  ##axis(2)
  
}

exists_input <- function(object, input_name){
  call <- object$call
  
  return(is.null(call[[input_name]]))
}


substitute_input <- function(object, input_name, input_value){
  call <- object$call
  
  if(!(is.null(input_value) & is.null(call[[input_name]])))
    call[[input_name]] <- input_value
  
  eval(call)
}


