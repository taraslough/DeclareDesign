rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = declare_variable(), N = 250)
  sampling <- declare_sampling(n = 100)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                   condition_names = c(0, 1),
                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(condition_names = c(0,1), probability_each = c(.7, .3))
  

# Diagnosis ---------------------------------------------------------------

  estimand <- declare_estimand(estimand_text = "mean(Y_1 - Y_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    estimates_options = list(coefficient_name = "Z"),
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_d_i_m, estimator_lm), 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
    
  diagnosis <- diagnose_design(design = design)
  
# mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(population = population, sampling = sampling, assignment = assignment, potential_outcomes = potential_outcomes)
  
})
