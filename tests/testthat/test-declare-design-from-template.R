rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Declare Design from Template")

test_that("declare_design_from_template works", {

  simple_template <- function(N, n){
    population <- declare_population(noise = "rnorm(n_)", size = N)
    sampling <- declare_sampling(n = n)
    
    potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                     condition_names = c(0, 1),
                                                     assignment_variable_name = "Z")
    
    assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
    
    estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
    estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
    
    design <- declare_design(population = population,
                             sampling = sampling, 
                             assignment = assignment, 
                             estimator = estimator_d_i_m, 
                             potential_outcomes = potential_outcomes,
                             label = "Simple Design")
    return(design)
  }
  
  design <- quick_design(template = simple_template, N = 5000, n = 100)

})
