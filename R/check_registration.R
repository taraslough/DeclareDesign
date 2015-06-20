#' Check a pre-registration for consistency between design and analysis
#'
#' @param design design object
#' @param covariates covariates object
#' @param potential_outcomes potential outcomes object
#' @param analysis analysis object
#' @param sims number of iterations
#' @export
check_registration <- function(design, covariates, potential_outcomes, analysis){
  
  ##-- You have a blocked design but are not taking account of blocks in your analysis
  ##-- Your randomization scheme implies unequal propensities but you are not taking account of this in your analysis
  ##-- Your randomization scheme implies correlated assignments, but...
  ##-- You indicate X as your primary analysis but your power is only good for Y
  ##-- With so many outcomes you might consider adjustments for multiple comparisons
  
  warning("This is a test warning about the design.", call. = FALSE)
  
}
