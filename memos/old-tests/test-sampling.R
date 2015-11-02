
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("test sampling strategies")

test_that("test sampling from population with no clusters or strata", {
  
  pop <- declare_population(gender = declare_variable(),
                            N = 5000, super_population = FALSE)
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  smp <- declare_sampling(m = 500)
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  analysis_1 <- declare_estimator(formula = Y ~ Z, estimates = difference_in_means)
  
  test_data <- data.frame(Y = rbinom(n = 500, size = 1,prob = .2), Z = rbinom(500, size = 1, prob = .5))
  
  get_estimates(estimator = analysis_1, data = test_data)
  
  estimand1 <- declare_estimand(estimand = estimand_ATE(), subset = "gender > 1")
  
  analysis_2 <- declare_estimator(formula = Y ~ Z, model = lm, estimates = get_regression_coefficient, estimates_options = list(coefficient_name = "Z"),
                                  estimand = estimand1)
  
  a <- get_estimates_model(estimator = analysis_2, data = test_data)
  get_estimates(estimator = analysis_2, data = test_data)
  
  
  diagnosis <- diagnose(population = pop, sampling = smp, analysis = list(analysis_1, analysis_2),
                        assignment = assignment, sims = 5)
  
  estimates <- diagnosis$estimates
  sample_estimands <- diagnosis$sample_estimands
  population_estimands <- diagnosis$population_estimands
  
  pop_draw <- draw_population(population = pop, potential_outcomes = po)
  
  estimand2 <- declare_estimand(estimand = estimand_ATE(condition_treat = "Z0", condition_control = "Z1"), subset = "gender > 1")
  get_estimands(estimand = list(estimand1, estimand2), data = pop_draw)
  
  
  smp_draw <- draw_sample(data = pop_draw, sampling = smp)
  
  data <- assign_treatment(data = smp_draw, assignment = assignment)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  
})

test_that("with strata only", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50), potential_outcomes = po)
  
  smp <- declare_sampling(prob = 0.1, strata_variable_name = "villages_id")
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 10)
  
})

test_that("with clusters only", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(),
                            N_per_level = c(5000, 50), potential_outcomes = po)
  
  smp <- declare_sampling(prob = 0.1, cluster_variable_name = "villages_id")
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = smp)
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})

test_that("with clusters and strata", {
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1 + rnorm(5000) + regions_id*3)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(5000, 50, 5), potential_outcomes = po)
  
  smp <- declare_sampling(cluster_variable_name = "villages_id", strata_variable_name = "regions_id")
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  analysis_joint_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_sampling_weights",
                                              quantity_of_interest = average_treatment_effect)
  
  analysis_sampling_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "sampling_weights",
                                                 quantity_of_interest = average_treatment_effect)
  
  analysis_assignment_weighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm, weights_variable = "assignment_weights",
                                                   quantity_of_interest = average_treatment_effect)
  
  analysis_unweighted <- declare_analysis(Y ~ Z, treatment_variable = "Z", estimator = lm,
                                          quantity_of_interest = average_treatment_effect)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = smp)
  
  data <- assign_treatment(data = smp_draw, assignment = assignment)
  
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
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1 + rnorm(1000) + regions_id*3)
  
  pop <- declare_population(individuals = list(income = declare_variable()), villages = list(), regions = list(),
                            N_per_level = c(1000, 100, 5), potential_outcomes = po)
  
  smp <- declare_sampling(strata_prob = c(.1, .2, .33, .1, .25), cluster_variable_name = "villages_id", strata_variable_name = "regions_id")
  
  assignment <- declare_assignment(potential_outcomes = po)
  
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
  
  diagnosis2 <- diagnose(population = pop, sampling = smp, assignment = assignment, analysis = analysis_joint_weighted, sims = 400)
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = smp)
  
  revealed_data <- assign_treatment(data = smp_draw, assignment = assignment)
  
  get_estimates(list(analysis_joint_weighted, 
                     analysis_sampling_weighted,
                     analysis_assignment_weighted,
                     analysis_unweighted), data = revealed_data)  
  
  
  expect_equal(nrow(pop_draw),  5000)
  expect_equal(nrow(smp_draw),  500)
  expect_equal(unique(table(smp_draw$villages_id)), 100)
  expect_equal(length(unique(smp_draw$villages_id)), 5)
  
})
