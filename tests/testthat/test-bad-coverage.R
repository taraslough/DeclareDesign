
rm(list=ls())
library(testthat)
library(experimentr)

context("Permutation")

test_that("test permutation matrix", {
  
  sample_frame <- declare_sample_frame(
    individuals = list(
      income = declare_variable(normal_mean = 3, normal_sd = 1)),
    N_per_level = c(10))
  
  potential_outcomes     <-  declare_potential_outcomes(
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + .5*income
  )
  
  blocks <- declare_blocks(blocks = "income", block_name = "income_groups", block_count = 10)
  
  design <- declare_design(potential_outcomes = potential_outcomes, blocks = blocks)
  
  analysis_lsdv <- declare_analysis(formula = Y ~ Z + factor(income_groups), estimator = linear_regression,
                                    quantity_of_interest = average_treatment_effect)
  
  analysis <- declare_analysis(formula = Y ~ Z)
  
  power         <- simulate_experiment(sims = 5, analysis = list(analysis, analysis_lsdv), design = design, 
                                       clusters = clusters, sample_frame = sample_frame, 
                                       potential_outcomes = potential_outcomes)
  summary(power_1)
  
  
  
  mock <- make_data(potential_outcomes = potential_outcomes, sample_frame = sample_frame, 
                    blocks = blocks)
  
  mock$Z <- assign_treatment(design = design, data = mock)
  mock$Y <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock)
  
  a <- get_estimates(analysis = analysis, data = mock)
  a <- get_estimates(analysis = analysis, data = mock)
  
  
})
