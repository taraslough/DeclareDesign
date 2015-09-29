
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
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  analysis_2 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  
  diagnosis <- diagnose(population = pop, sampling = smp, analysis = list(analysis_1, analysis_2),
                        design = design, sims = 5)
  
  estimates <- diagnosis$estimates
  sample_estimands <- diagnosis$sample_estimands
  population_estimands <- diagnosis$population_estimands
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_design(data = smp_draw, design = design)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  
})

test_that("with strata only", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50), potential_outcomes = po)
  
  smp <- declare_sampling(prob = 0.1, strata_variable_name = "villages_id")
  
  design <- declare_design(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 10)
  
})

test_that("with clusters only", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50), potential_outcomes = po)
  
  smp <- declare_sampling(prob = 0.1, cluster_variable_name = "villages_id")
  
  design <- declare_design(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})

test_that("with clusters and strata", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + rnorm(5000) + regions_id*3)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(5000, 50, 5), potential_outcomes = po)
  
  smp <- declare_sampling(cluster_variable_name = "villages_id", strata_variable_name = "regions_id")
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_joint_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_sampling_weights",
                                        quantity_of_interest = average_treatment_effect)
  
  analysis_sampling_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "sampling_weights",
                                             quantity_of_interest = average_treatment_effect)
  
  analysis_assignment_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_weights",
                                                 quantity_of_interest = average_treatment_effect)
  
  analysis_unweighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm,
                                          quantity_of_interest = average_treatment_effect)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_design(data = smp_draw, design = design)
  
  get_estimates(list(analysis_joint_weighted, 
                     analysis_sampling_weighted,
                     analysis_assignment_weighted,
                     analysis_unweighted), data = data)  
  
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})

test_that("with clusters and strata", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 + rnorm(1000) + regions_id*3)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(1000, 100, 5), potential_outcomes = po)

  smp <- declare_sampling(strata_prob = c(.1, .2, .33, .1, .25), cluster_variable_name = "villages_id", strata_variable_name = "regions_id")
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_joint_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_sampling_weights",
                                              quantity_of_interest = average_treatment_effect)
  
  analysis_sampling_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "sampling_weights",
                                                 quantity_of_interest = average_treatment_effect)
  
  analysis_assignment_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_weights",
                                                   quantity_of_interest = average_treatment_effect)
  
  analysis_unweighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm,
                                          quantity_of_interest = average_treatment_effect)
  
  library(parallel)
  cl <- makeCluster(2)
  
  diagnosis2 <- diagnose(population = pop, sampling = smp, design = design, analysis = analysis_joint_weighted, sims = 400)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  revealed_data <- reveal_design(data = smp_draw, design = design)
  
  get_estimates(list(analysis_joint_weighted, 
                     analysis_sampling_weighted,
                     analysis_assignment_weighted,
                     analysis_unweighted), data = revealed_data)  
  
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})