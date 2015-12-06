rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Multi-arm experiment")

test_that("test multi-arm experiment analysis and diagnosis", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*(Z=="1") + .9*(Z=="2") + noise,
                                                   condition_names = c("0", "1", "2"),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes)
  
  estimand_1 <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_1 <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                   coefficient_name = c("Z1"),
                                   formula = Y ~ Z, estimand = estimand_1)
  
  estimand_2 <- declare_estimand(estimand_text = "mean(Y_Z_2 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_2 <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                   coefficient_name = c("Z2"),
                                   formula = Y ~ Z, estimand = estimand_2)
  
  
  
  # Diagnosis ---------------------------------------------------------------
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_1, estimator_2), 
                           potential_outcomes = potential_outcomes,
                           label = "Multi-arm-trial")
  
  diagnosis <- diagnose_design(design = design)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  
  estimates_1 <- get_estimates(estimator = estimator_1, data = smp_draw)
  estimates_2 <- get_estimates(estimator = estimator_2, data = smp_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(design = design)
  
})

