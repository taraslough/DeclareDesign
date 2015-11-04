rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = declare_variable(), N = 1000)
  sampling <- declare_sampling(n = 500)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                   condition_names = c(0, 1),
                                   treatment_variable = "Z")
  assignment <- declare_assignment(condition_names = c(0,1))
  

# Diagnosis ---------------------------------------------------------------

  estimand <- declare_estimand(default_estimand_function("mean(Y_1 - Y_0)"), potential_outcomes = potential_outcomes)
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
    
  diagnosis <- diagnose_design(design = design, sims = 5)
  
# mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, population_data = pop_draw)
  
  # test reveal_design
  
  smp_draw_reveal <- reveal_design(data = pop_draw, sampling = sampling, assignment = assignment, potential_outcomes = potential_outcomes)
  
})
