
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Permutation")

test_that("test permutation matrix", {
  
  sample <- declare_sample(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1),
      ethnicity = declare_variable(multinomial_probabilities = c(.1, .2, .3, .4), multinomial_categories = 1:4)
      ),
    N = 1000)
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income + -.1*Z1*income + runif(1000)
  )
  
  blocks <- declare_blocks(blocks = "ethnicity", block_name = "income_groups", block_count = 4)
  
  assignment_blocked <- declare_assignment(potential_outcomes = potential_outcomes, blocks = blocks, prob_each = c(.9, .1))
  assignment_notblocked <- declare_assignment(potential_outcomes = potential_outcomes)
  
  analysis_lsdv <- declare_analysis(formula = Y ~ Z + factor(income_groups), estimator = linear_regression,
                                    quantity_of_interest = average_treatment_effect,
                                    estimand_formula = Y ~ Z)
  
  analysis_lm <- declare_analysis(formula = Y ~ Z, estimator = linear_regression,
                                  quantity_of_interest = average_treatment_effect,
                                  estimand_formula = Y ~ Z)
  
  analysis <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = difference_in_means_blocked, 
                               block_variable = "income_groups")
  
  power_test        <- diagnose(sims = 5, 
                                  analysis = list(analysis_lsdv, analysis_lm, analysis), 
                                  assignment = assignment_blocked, 
                                  blocks = blocks, sample = sample, 
                                  potential_outcomes = potential_outcomes)
  
  mock <- make_data(potential_outcomes = potential_outcomes, sample = sample, assignment = assignment_blocked)
  
  mock$Z <- assign_treatment(assignment = assignment_blocked, data = mock)
  mock$Y <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  dimM <- get_estimates(analysis = analysis, data = mock)
  lsdvM <- get_estimates_model(analysis = analysis_lsdv, data = mock)
  lmM <- get_estimates_model(analysis = analysis_lm, data = mock)
  
  
})
