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
make_data_frame <- function(potential_outcomes, covariates){
  
  return(cbind(potential_outcomes, covariates))
  
}
