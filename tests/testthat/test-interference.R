rm(list=ls())
library(testthat)
library(DeclareDesign)
library(sna)

context("Interference")

test_that("test simple interference model", {
  
  adj <- rgnm(n = 1, nv = 100, m = 2000)
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  interference <- declare_interference(formula = E ~ adj %*% Z, 
                                       options = list(adj = adj))
  
  potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .1*E  + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  potential_outcomes_2 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .5*E  + noise,
                                                     condition_names = c(0, 1),
                                                     assignment_variable_name = "Z")
  
  assignment <- declare_assignment(condition_names = c(0,1), 
                                   probability_each = c(.7, .3))
  
  # Should throw error!
  pop_draw <- draw_population(population = population, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2))
  # Should not throw error
  pop_draw <- draw_population(population = population)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = list(interference, potential_outcomes_1, potential_outcomes_2), 
                           condition_names = c(0, 1))
  head(smp_draw)  
  
    
  # test draw_data
  
  smp_draw_reveal <- draw_data(design = design)
  
  
  
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(estimand_text = "0.5", potential_outcomes = potential_outcomes_1)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = list(interference, potential_outcomes_1, potential_outcomes_2),
                           label = "Simple Design")
  
  diagnosis <- diagnose_design(design = design)
  
  # mock data  ---------------------------------------------------------------  
  
})

