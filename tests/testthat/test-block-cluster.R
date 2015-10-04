
rm(list=ls())
library(testthat)
library(experimentr)

context("test block and cluster treatments")

test_that("test with no blocks or clusters", {
  
  pop <- declare_population(N = 5000)
  
  smp <- declare_sampling(m = 500)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_assignment(data = smp_draw, assignment = assignment)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  
})

test_that("with blocks only", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50))
  
  smp <- declare_sampling(prob = 0.1)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  assignment <- declare_assignment(potential_outcomes = po, block_variable_name = "villages_id")
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_assignment(data = smp_draw, assignment = assignment)
  
})

test_that("with clusters only", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50))
  
  smp <- declare_sampling(prob = 0.1)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  assignment <- declare_assignment(potential_outcomes = po, cluster_variable_name = "villages_id")
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_assignment(data = smp_draw, assignment = assignment)
  
})

test_that("with clusters and strata", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(5000, 50, 5))
  
  smp <- declare_sampling(prob = 0.1)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  assignment <- declare_assignment(potential_outcomes = po, cluster_variable_name = "villages_id", block_variable_name = "regions_id")
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_assignment(data = smp_draw, assignment = assignment)
  
})
