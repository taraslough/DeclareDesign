rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Noncompliance")

test_that("test whether noncompliance works", {
  
  population <- declare_population(noise = declare_variable(), N = 1000)
  sampling <- declare_sampling(n = 500)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                   condition_names = c(0, 1),
                                                   treatment_variable = "Z")
  noncompliance <- declare_noncompliance(condition_names = c(0,1), 
                                         treatment_variable = "Z", 
                                         compliance_proportions=c(1, .5), 
                                         baseline_condition=0)
  assignment <- declare_assignment(condition_names = c(0,1))
  
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(text_estimand = declare_ATE(condition_treat = 1, condition_control = 0), potential_outcomes = potential_outcomes)
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
  
  diagnosis <- diagnose(design = design, sims = 1000)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = list(potential_outcomes, noncompliance))
  head(smp_draw)
  with(smp_draw, table(Z, D))
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  
})
