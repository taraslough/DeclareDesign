rm(list=ls())
library(testthat)
library(DeclareDesign)

context("get_estimand")

test_that("test that get_estimand works with and without POs in the data", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population)
  
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
})

