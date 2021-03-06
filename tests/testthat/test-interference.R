rm(list=ls())
library(testthat)
library(DeclareDesign)
suppressMessages(library(sna))

context("Interference")

test_that("test simple interference model", {
  
  adj <- rgnm(n = 1, nv = 100, m = 2000)
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  interference <- declare_interference(formula = E ~ adj %*% Z, adj = adj, condition_names = c(0,1))
  
  potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .1*E  + noise,
                                                     condition_names = c(0, 1),
                                                     assignment_variable_name = "Z")
  
  potential_outcomes_2 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .5*E  + noise,
                                                     inherit_condition_names = TRUE,
                                                     assignment_variable_name = "Z")
  
  assignment <- declare_assignment(condition_names = c(0,1), 
                                   probability_each = c(.7, .3))
  
  # Should throw error!
  expect_error(pop_draw <- draw_population(population = population, potential_outcomes = list(potential_outcomes_1, potential_outcomes_2)))
  # Should not throw error
  pop_draw <- draw_population(population = population)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = list(interference, potential_outcomes_1, potential_outcomes_2), 
                           condition_names = c(0, 1))
  head(smp_draw)
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(estimand_text = "0.5", potential_outcomes = potential_outcomes_1,
                               fixed = TRUE, label = "fixed_estimand", estimand_level = "assignment")
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  ##estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = list(interference, potential_outcomes_1, potential_outcomes_2),
                           label = "Simple Design")
  
  expect_warning(smp_draw_reveal <- draw_data(design = design))
  
  expect_warning(diagnosis <- diagnose_design(design = design))
  
  # mock data  ---------------------------------------------------------------  
  
  # More complex kinds of exposure ------------------------------------------
  
  #   test continuous exposure matrix
  adj_cont <- adj*rnorm(prod(dim(adj)))
  interference_cont_1 <- declare_interference(
    formula = E1 ~ adj %*% Z, adj = adj_cont, condition_names = c(0,1))
  
  interference_cont_2 <- declare_interference(
    formula = E2 ~ adj %*% Z / 2, adj = adj_cont, condition_names = c(0,1))
  
  POs_1 <- declare_potential_outcomes(formula = Y1 ~ 5 + .5*Z + .1*E1  + noise,
                                      condition_names = c(0, 1),
                                      assignment_variable_name = "Z")
  POs_2 <- declare_potential_outcomes(formula = Y2 ~ 5 + .5*Z + .1*E2  + noise,
                                      condition_names = c(0, 1),
                                      assignment_variable_name = "Z")
  
  #   test multiple exposures
  POs_3 <- declare_potential_outcomes(formula = Y3 ~ 5 + .5*Z + .1*E2 + E1 + noise,
                                      condition_names = c(0, 1),
                                      assignment_variable_name = "Z")
  
  new_smp_draw <- draw_outcome(
    data = smp_draw, 
    potential_outcomes = list(
      interference_cont_1,
      interference_cont_2, 
      POs_1, POs_2, POs_3), 
    condition_names = c(0, 1))
  
  
})



