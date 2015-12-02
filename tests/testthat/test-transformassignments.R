rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Transform treatment assignments")

test_that("Test whether you can transform treatment assignments", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 5000)
  sampling <- declare_sampling(n = 500)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + 1*Z1 + 2*Z2 - 1*Z1*Z2 + noise,
                                                   condition_names = list(Z1 = c(0, 1), 
                                                                          Z2 = c(0, 1)),
                                                   assignment_variable_name = c("Z1", "Z2"))
  assignment <- declare_assignment(condition_names = c(1, 2, 3, 4),
                                   transform_options = list(Z1 = c(3, 4),
                                                            Z2 = c(2, 4)))
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  
  head(smp_draw)

  estimand <- declare_estimand(estimand_text = "mean(Y_Z1_1_Z2_1 - Y_Z1_1_Z2_0)", potential_outcomes = potential_outcomes)
  
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means,
                                       formula = Y ~ Z, estimand = estimand)
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  

})
