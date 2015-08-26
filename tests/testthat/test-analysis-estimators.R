
rm(list=ls())
library(testthat)
library(experimentr)

context("Simple experiment")

test_that("test whether a simple experiment can be pre-registered", {
  
  smp <- declare_sample(error = declare_variable(),N = 850)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + error)
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_diff <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  analysis_ols <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = linear_regression, quantity_of_interest = average_treatment_effect)
  analysis_logit <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = logistic_regression, quantity_of_interest = average_treatment_effect)
  analysis_probit <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = probit_regression, quantity_of_interest = average_treatment_effect)

  mock          <- make_data(potential_outcomes = po, sample_frame = smp)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  estimates_diff <- get_estimates(analysis_diff, data = mock)
  estimands_diff <- get_estimands(analysis_diff, data = mock)
  
  estimates_ols <- get_estimates(analysis_ols, data = mock)
  estimands_ols <- get_estimands(analysis = analysis_ols, data = mock)
  fit_ols <- get_estimates_model(analysis = analysis_ols, data = mock)
  fit_ols_estimands <- get_estimands_model(analysis = analysis_ols, data = mock)

  estimates_logit <- get_estimates(analysis_logit, data = mock)
  estimands_logit <- get_estimands(analysis = analysis_logit, data = mock)
  fit_logit <- get_estimates_model(analysis = analysis_logit, data = mock)
  fit_logit_estimands <- get_estimands_model(analysis = analysis_logit, data = mock)
  
  estimates_probit <- get_estimates(analysis_probit, data = mock)
  estimands_probit <- get_estimands(analysis = analysis_probit, data = mock)
  fit_probit <- get_estimates_model(analysis = analysis_probit, data = mock)
  fit_probit_estimands <- get_estimands_model(analysis = analysis_probit, data = mock)
  
  
})
