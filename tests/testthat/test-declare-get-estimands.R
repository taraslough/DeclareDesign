rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Estimands")

test_that("Robust SEs", {
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  
  estimator_ols <- declare_estimator(
    formula           = Y ~ Z, 
    model             = lm, 
    estimates         = get_regression_coefficient, 
    coefficient_name  = "Z",
    estimand          = estimand)
  
  estimator_robust <- declare_estimator(
    formula           = Y ~ Z, 
    model             = lm, 
    estimates         = get_regression_coefficient_robust, 
    coefficient_name  = "Z",
    estimand          = estimand)
  
  
  # Diagnosis ---------------------------------------------------------------
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_ols, estimator_robust), 
                           potential_outcomes = potential_outcomes)
  
  diagnosis <- diagnose_design(design = design)
  diagnosis
})
