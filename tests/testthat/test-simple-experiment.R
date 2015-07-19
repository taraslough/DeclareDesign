context("Simple experiment")

rm(list=ls())
library(testthat)
library(registration)

test_that("test whether a simple experiment can be pre-registered", {
  
  smp <- declare_sample_frame(N = 850)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")

  sims <- simulate_experiment(potential_outcomes = po, 
                              sample_frame =  smp, 
                              design = design, 
                              analysis = analysis_1, 
                              sims = 100)
  summary(sims)
  
  # Run analysis on a single realization
  mock          <- make_data(potential_outcomes = po, sample_frame =  smp)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  estimates <- get_estimates(analysis = analysis_1,data = mock)
  estimates
  
  estimands <- get_estimands(analysis = analysis_1,data = mock)
  estimands
  
  fit <- get_estimates_model(analysis = analysis_1, data = mock)
  summary(fit)
  
})
