#' Declare the data-generating process of a variable
#'
#' @param data data object
#' @param design design object
#' @param analysis analysis object
#' @param sims number of iterations
#' @export
get_power <- function(data, design, analysis, sims = 100){
  sims_vec <- rep(NA, sims)
  for(i in 1:sims){
    
    data[, analysis_treatment_variable(analysis)] <- assign_treatment(design)
    data[, analysis_outcome_variable(analysis)] <- observed_outcome(outcome = analysis_outcome_variable(analysis), 
                                                                    treatment_assignment = analysis_treatment_variable(analysis),
                                                                    data = data)
    
    sims_vec[i] <- test_success(analysis = analysis, data = data)
    
  }
  return(power = mean(sims_vec))
}
