
rm(list=ls())
library(testthat)
library(experimentr)

context("Permutation")

test_that("test permutation matrix", {
  
  # Still have to put in covariates because where else to specify n (it's in make_data() right now)
  smp <- declare_sample_frame(N = 500)
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(normal_mean = 0, normal_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 
  )
  
  design        <- declare_design(potential_outcomes = po)
  
  mock <- make_data(potential_outcomes = po, sample_frame = smp)
  
  perms <- make_permutation_matrix(design = design, data = mock)
  
})
