context("Permutation")

rm(list=ls())
library(testthat)
library(registration)

test_that("test permutation matrix", {
  
  # Still have to put in covariates because where else to specify n (it's in make_data() right now)
  cov <- declare_covariates(
    individuals = list(
      income = declare_variable()),
    N_per_level = c(500))
  
  po     <-  declare_potential_outcomes(
    outcome_variable_DGP = declare_variable(linear_mean = 0, linear_sd = 1),
    condition_names = c("Z0","Z1"),
    outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1 
  )
  
  design        <- declare_design(potential_outcomes = po)
  
  mock <- make_data(potential_outcomes = po, covariates = cov)
  
  perms <- make_permutation_matrix(design = design, data = mock)
  
})
