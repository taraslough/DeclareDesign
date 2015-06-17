#' Construct data frame for mock analysis using potential outcomes and covariates
#'
#' Description
#' @param potential_outcomes tmp
#' @param covariates tmp
#' @param ... additional options to be sent to the analysis function and the test_success function
#' @return a data frame
#' @examples
#' # these examples don't work yet
#' # declare_analysis(analysis = "diff-in-means")
#' # declare_analysis(analysis = function(Y, Z, data) lm(paste(Y, "~", Z), data = data))
#' @export
make_data <- function(potential_outcomes, covariates){
  
  ## right now this only takes our functions and draws one realization of the potential outcomes and the covariates
  ## in future it should alternatively be able to take two dataframes, or just a dataframe of covariates
  
  return(cbind(make_potential_outcomes(potential_outcomes, covariates = covariates), make_covariates(covariates)))
  
}


