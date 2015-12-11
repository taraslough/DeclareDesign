rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Subsetting")

test_that("test workflow with subsetting", {
  
  population <- declare_population(noise = "rnorm(n_)", 
                                   dem = "rbinom(n_, 1, .5)",
                                   size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                   condition_names = c(0, 1),
                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes)
  

  # ways to subset ----------------------------------------------------------  
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)",
                               subset =  "dem == 1",
                               potential_outcomes = potential_outcomes)
  
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, 
                                       subset = "dem == 1", 
                                       estimand = estimand)
  
# Diagnosis ---------------------------------------------------------------


  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
    
  summ <- summary(design)
  
  diagnosis <- diagnose_design(design = design)
  
  
})


test_that("test multi-arm subsetting", {
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 +
                                                     .1*(Z=="placebo") +
                                                     .9*(Z=="treatment") + 
                                                     noise,
                                                   condition_names = c("control", "placebo", "treatment"),
                                                   assignment_variable_name = "Z")
  assignment <- declare_assignment(potential_outcomes=potential_outcomes)
  estimand_1 <- declare_estimand(estimand_text = "mean(Y_Z_treatment - Y_Z_control)", 
                                 potential_outcomes = potential_outcomes)
  estimator_1 <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, 
                                   estimand = estimand_1, subset = "Z != 'placebo'")
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_1,
                           potential_outcomes = potential_outcomes)
  diagnosis <- diagnose_design(design)
  diagnosis
})
