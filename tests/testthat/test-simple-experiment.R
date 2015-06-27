context("Basic experiment")

rm(list=ls())
library(testthat)
library(registration)

test_that("test whether a simple experiment can be pre-registered", {
  
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
  
  analysis_1      <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", 
                                      method = "lm", qoi_function = "ATE")
  
  sims <- simulate_experiment(potential_outcomes = po, covariates = cov, 
                              design = design, analysis = analysis_1,sims = 100)
  summary(sims)
  
  # Run analysis on a single realization
  mock          <- make_data(potential_outcomes = po, covariates = cov)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  estimates <- get_estimates(analysis = analysis_1,data = mock)
  estimates
  
  fit <- get_estimates_model(analysis = analysis_1, data = mock)
  summary(fit)
  
})
