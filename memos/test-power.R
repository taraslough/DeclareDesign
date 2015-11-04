
rm(list=ls())
library(testthat)
library(DeclareDesign)
library(dplyr)

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

potential_outcomes <- declare_potential_outcomes(formula = Y ~ 60 + 5*Z + noise, treatment_variable = 'Z')

assignment <- declare_assignment(condition_names = c(0, 1))

estimand_ATE <- declare_estimand(estimand_text = "mean(Y_1 - Y_0)", 
                                 target = "population",
                                 potential_outcomes = potential_outcomes)

estimator_diff_in_means <- declare_estimator(Y ~ Z, 
                                             estimates = difference_in_means, 
                                             estimand = estimand_ATE)

design <- declare_design(population = population, sampling = sampling, 
                         assignment = assignment, estimator = estimator_diff_in_means, 
                         potential_outcomes = potential_outcomes)

diagnosis <- diagnose_design(design = design, sims = 1000)

diagnosis

draw_population(population = population) %>%
  draw_data(sampling = sampling,
                assignment = assignment, 
                potential_outcomes = potential_outcomes) %>% 
  head


