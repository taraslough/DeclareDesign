#' Declare the data-generating process of a variable
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param sims number of iterations
#' @export
get_power <- function(data=NULL, potential_outcomes = NULL, covariates = NULL, design, analysis, sims = 100, ...){
  if(is.null(data) & is.null(potential_outcomes) & is.null(covariates)){
    stop("You must provide either a dataframe or one or both of the potential outcomes argument and the covariates argument.")
  }
  
  if(!is.null(data) & (!is.null(potential_outcomes) | !is.null(covariates))){
    stop("Please do not simultaneously specify both the data argument and one or both of the potential outcomes argument and the covariates argument.")
  }
  
  simulation_vector <- rep(NA, sims)
  for(i in 1:sims){
    
    ## sends ... options to assign_treatment, which passes them onto ra_fun()
    ## this is how we allow varying N and other parameters
    data_sim <- data
    if(is.null(data)){
      data_sim <- make_data(potential_outcomes = potential_outcomes, covariates = covariates)
    }
    
    data_sim[, analysis_treatment_variable(analysis = analysis)] <- 
      assign_treatment(design = design)
    
    data_sim[, analysis_outcome_variable(analysis = analysis)] <- 
      observed_outcome(outcome = analysis_outcome_variable(analysis),  
                       treatment_assignment = analysis_treatment_variable(analysis),
                       design = design,
                       data = data_sim)
    
    simulation_vector[i] <- test_success(analysis = analysis, data = data_sim)
    
  }
  return(power = mean(simulation_vector))
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

