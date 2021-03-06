rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Multiple Potential Outcomes")

test_that("test multiple potential outcomes", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  potential_outcomes_2 <- declare_potential_outcomes(formula = Y2 ~ 5 + .25*Z + noise,
                                                   inherit_condition_names = TRUE,
                                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(potential_outcomes = potential_outcomes_1, probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes_1)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  # Diagnosis ---------------------------------------------------------------
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = list(potential_outcomes_1, potential_outcomes_2),
                           label = "Simple Design")
  
  diagnosis <- diagnose_design(design = design)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2))
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2), condition_names = c(0, 1))
  head(smp_draw)  
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(design = design)
  
})

