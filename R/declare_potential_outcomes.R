#' Define the potential outcomes of the experiment
#'
#' @param covariate_object covariates data frame
#' @return outcomes_object
#' @export
declare_potential_outcomes <- 
  function(
    condition_names,
    outcome_formula,
    outcome_variable_DGP = declare_variable(),
    cluster_variable = NULL,
    ICC = NULL
  ){
      if(FALSE %in% (condition_names %in% all.vars(outcome_formula)))stop(
        "All conditions must be included as variables in the outcome formula."
      )
      outcomes_object <- list(
        condition_names  = condition_names,
        outcome_formula  = outcome_formula,
        outcome_name     = all.vars(outcome_formula)[1],
        outcome_variable = outcome_variable_DGP,
        cluster_variable = cluster_variable,
        ICC = .01,
        call = match.call()
      )
      class(outcomes_object) <- "potential_outcomes"
      return(outcomes_object)
    }


#' @export
outcomes_table <- function(x){
  if(class(x) == "potential_outcomes")
    x <- list(x)
  cat("This will be a summary table of the distribution of each outcome. Not implemented yet.")
}
