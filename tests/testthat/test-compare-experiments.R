rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 1000)
  sampling <- declare_sampling(n = 100)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(condition_names = c(0,1), probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  diagnosis <- diagnose_design(design = design)
  
  sampling_large <- declare_sampling(n = 500)
  
  ## compare two designs
  
  design_alt <- modify_design(design = design, sampling = sampling_large)
  
  comparison_two_designs <- compare_designs(list(design, design_alt))
  
  ## compare one design by changing the sampling (i.e. do not make a new design)
  
  comparison_change_inputs <- compare_designs(design = design, sampling = list(sampling, sampling_large))
  
  
})

