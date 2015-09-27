
rm(list=ls())
library(testthat)
library(experimentr)

context("test sampling strategies")

test_that("test sampling from population with no clusters or strata", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  pop <- declare_population(N = 5000, super_population = FALSE, potential_outcomes = po)
  
  smp <- declare_sampling(m = 500)
  
  design <- declare_design(potential_outcomes = po)
  
  analysis <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  
  diagnosis <- diagnose(population = pop, sampling = smp, analysis = analysis,
                        design = design, sims = 500)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_design(data = smp_draw, design = design)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  
})

test_that("with strata only", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50))
  
  smp <- declare_sampling(prob = 0.1, strata_variable_name = "villages_id")
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_design(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 10)
  
})

test_that("with clusters only", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50))
  
  smp <- declare_sampling(prob = 0.1, cluster_variable_name = "villages_id")
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_design(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})

test_that("with clusters and strata", {
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(5000, 50, 5))

  smp <- declare_sampling(prob = 0.1, cluster_variable_name = "villages_id", strata_variable_name = "regions_id")
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_design(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})
