rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Blocked and clustered experiment")

test_that("test", {
  
  population <- declare_population(individuals = list(noise = declare_variable()),
                                   villages = list(elevation = declare_variable(),
                                                   high_elevation = declare_variable(transformation = "1*(elevation > 0)")), 
                                   N_per_level = c(1000, 100))
  
  sampling <- declare_sampling(n = 10, cluster_variable_name = "villages_ID")
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1),
                                                   treatment_variable = "Z")
  
  assignment <- declare_assignment(condition_names = c(0,1),
                                   block_variable_name = "elevation_high", 
                                   custom_block_function = function(data) return(1*(data$elevation > 0)),
                                   cluster_variable_name = "villages_ID")
  
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(text_estimand = declare_ATE(condition_treat = 1, condition_control = 0), potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means_blocked,
                                       estimates_options = list(block_variable = "elevation_high"),
                                       formula = Y ~ Z, estimand = estimand)
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    estimates_options = list(coefficient_name = "Z"),
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_d_i_m, estimator_lm), 
                           potential_outcomes = potential_outcomes,
                           label = "Blocked and Clustered Design")
  
  diagnosis <- diagnose(design = design, sims = 5)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  
})
