### NOT YET WRITTEN:

# is wrapper for po.  may specify baseline condition


#' Define the potential outcomes of the experiment
#'
#' @param condition_names A character vector indicating the names of the conditions to which subjects can be assigned. Conceptually identical to the potential outcomes that are revealed in each condition, implicitly invoking SUTVA.
#' @param outcome_formula A regression-like expression for declaring the relationship between the outcome, treatments, and optionally, the covariates.
#' @param population_proportions what is it?
#' @param proportion_outcome_name what is it?
#' @return outcomes_object
#' @export
declare_noncompliance <- function(condition_names = NULL, outcome_formula = NULL,
                                       # outcome_variable_DGP = declare_variable(), 
                                       # cluster_variable = NULL, 
                                       # ICC = NULL, 
                                       population_proportions = NULL, proportion_outcome_name = NULL){
    
    outcomes_object <- declare_potential_outcomes(condition_names = condition_names,
                                                  outcome_formula = outcome_formula,
                                                  population_proportions = population_proportions,
                                                  proportion_outcome_name = proportion_outcome_name)
    class(outcomes_object) <- "compliance_outcomes"
    return(outcomes_object)
}


#' @export
outcomes_table <- function(x){
  if(class(x) == "potential_outcomes")
    x <- list(x)
  cat("This will be a summary table of the distribution of each outcome. Not implemented yet.")
}



