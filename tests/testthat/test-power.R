
rm(list=ls())
library(testthat)
library(DeclareDesign)

power_calculator <- function(mu_t, mu_c, sigma, alpha=0.05, N){ 
  lowertail <- (abs(mu_t - mu_c)*sqrt(N))/(2*sigma)
  uppertail <- -1*lowertail 
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 1- pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE) 
  return(beta) 
} 

power_calculator(65, 60, 20, N=500)

population <- declare_population(noise = 
                                   declare_variable(normal_mean = 0, normal_sd = 20),
                                 N=1000)

sampling <- declare_sampling(n = 500)

potential_outcomes <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                                 outcome_formula = Y ~ 60 + 0*Z0 + 5*Z1 + noise)

assignment <- declare_assignment(potential_outcomes = potential_outcomes)

estimand_ATE <- declare_estimand(estimand = declare_ATE(condition_treat = "Z1", 
                                                        condition_control = "Z0", 
                                                        outcome = "Y"), target = "population")

estimator_diff_in_means <- declare_estimator(Y ~ Z, 
                                             estimates = difference_in_means, 
                                             estimand = estimand_ATE)

diagnosis <- diagnose(population = population, sampling = sampling, 
                      assignment = assignment, estimator = estimator_diff_in_means, 
                      potential_outcomes = potential_outcomes, sims = 1000)

diagnosis

