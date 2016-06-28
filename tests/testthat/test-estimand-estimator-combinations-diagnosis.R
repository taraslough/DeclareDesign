
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test estimand-estimator combinations", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
  
  estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  ## no estimand
  
  data <- draw_data(design = design)
  
  est <- get_estimates(estimator = estimator, data = data)
  mand <- get_estimands(estimator = estimator, data = data)
  
  
  ## one estimator, one estimand
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  design <- modify_design(design = design, estimator = estimator)
  
  data <- draw_data(design = design)
  
  est <- get_estimates(estimator = estimator, data = data)
  mand <- get_estimands(estimator = estimator, data = data)
  
  expect_equal(object = est$estimator_label, "estimator")
  expect_equal(object = est$estimate_label, "d_i_m_1-0")
  expect_equal(object = est$estimand_label, "mean(Y_Z_1 - Y_Z_0)")
  expect_equal(object = nrow(est), 1)
  
  
  ## estimate label
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes, 
                               label = "my_estimand")
  estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand,
                                 label = "my_estimator", estimate_label = "my_difference_in_means")
  
  design <- modify_design(design = design, estimator = estimator)
  
  data <- draw_data(design = design)
  
  est <- get_estimates(estimator = estimator, data = data)
  mand <- get_estimands(estimator = estimator, data = data)
  
  expect_equal(object = est$estimator_label, "my_estimator")
  expect_equal(object = est$estimate_label, "my_difference_in_means")
  expect_equal(object = est$estimand_label, "my_estimand")
  
  
  ## greater than one estimator, greater than one estimand
  
  estimand_1 <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes,
                                 label = "estimand_1")
  estimand_2 <- declare_estimand(estimand_text = "mean(Y_Z_1 * Y_Z_0)", potential_outcomes = potential_outcomes,
                                 label = "estimand_2")
  estimator_1 <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand_1)
  estimator_2 <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand_2)
  
  design <- modify_design(design = design, estimator = list(estimator_1, estimator_2))
  
  data <- draw_data(design = design)
  
  est <- get_estimates(estimator = list(estimator_1, estimator_2), data = data)
  mand <- get_estimands(estimator = list(estimator_1, estimator_2), data = data)
  
  expect_equal(object = est$estimator_label, c("estimator_1", "estimator_2"))
  expect_equal(object = est$estimand_label, c("estimand_1", "estimand_2"))
  expect_equal(object = nrow(est), 2)
  expect_equal(object = nrow(mand), 2)
  
  
  ## one estimator, multiple estimands 
  
  estimand_1 <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes,
                                 label = "estimand_1")
  estimand_2 <- declare_estimand(estimand_text = "mean(Y_Z_1 * Y_Z_0)", potential_outcomes = potential_outcomes,
                                 label = "estimand_2")
  estimator <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = list(estimand_1, estimand_2))
  
  design <- modify_design(design = design, estimator = estimator)
  
  data <- draw_data(design = design)
  
  est <- get_estimates(estimator = estimator, data = data)
  mand <- get_estimands(estimator = estimator, data = data)
  
  expect_equal(object = est$estimator_label, c("estimator", "estimator"))
  expect_equal(object = est$estimand_label, c("estimand_1", "estimand_2"))
  expect_equal(object = nrow(est), 2)
  expect_equal(object = nrow(mand), 2)
  
})

