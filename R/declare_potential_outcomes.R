#' Define the potential outcomes of the experiment
#'
#' @param condition_names A character vector indicating the names of the conditions to which subjects can be assigned. Conceptually identical to the potential outcomes that are revealed in each condition, implicitly invoking SUTVA.
#' @param outcome_formula A regression-like expression for declaring the relationship between the outcome, treatments, and optionally, the covariates.
#' @param outcome_variable_DGP using \code{\link{declare_variable}}, a function for describing the data-generating process of the outcome variable.
#' @param cluster_variable The name of the clustering variable
#' @param ICC the intracluster correlation coefficient.  Note that if the outcome formula includes covariates, and those covariates are correlated with clusters, the true ICC may be higher or lower than is declared by this argument.
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
