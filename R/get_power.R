#' @export
get_power <- function(data, design, analysis, sims = 100){
  sims_vec <- rep(NA, sims)
  for(i in 1:sims){
    
    ## Note from Graeme: when you edit an object such as data within a function, 
    ## it does not edit the "real" object in the general environment. so we can add whatever variables we want temporarily, they will be removed later.
    data[, analysis_treatment_variable(analysis)] <- assign_treatment(design)
    data[, analysis_outcome_variable(analysis)] <- observed_outcome(outcome = analysis_outcome_variable(analysis), 
                                                                    treatment_assignment = analysis_treatment_variable(analysis), data = data)
    ## fyi for now the observed_outcome just takes the values of the treatment var created by ra_fun() to guess the names of the potential outcome variables
    ## i.e. if it creates Z_sim with values 0 and 1, this will take the outcome string "Y" and look for Y_1 and Y_0
    
    # Hey, shouldn't "alpha" be inherited from analysis? Yes; fixed everything so it does that.
    
    # I don't understand how to use the analysis object
    
    sims_vec[i] <- test_success(analysis = analysis, data = data)
    ## this runs an analysis using analysis$analysis() and then from that output runs the test_success function defined in analysis$test_success().
  
  }
  return(power=mean(sims_vec))
}
