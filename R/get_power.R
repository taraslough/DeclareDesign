get_power <- function(data, design, analysis, sims=100){
  sims_vec <- rep(NA, sims)
  for(i in 1:sims){
    Z_sim <- assign_treatment(design)
    Y_sim <- observed_outcome(treatment_assignment=Z_sim, data=data)
    
    # Hey, shouldn't "alpha" be inherited from analysis?
    
    # I don't understand how to use the analysis object
    
    sims_vec[i] <- test_success(analysis=analysis, data=data, alpha = .05)
  }
  return(power=mean(sims_vec))
}
