
rm(list=ls())
library(testthat)
library(registration)

context("Simple experiment")

test_that("test whether a simple experiment can be pre-registered", {
  
  smp_N <- declare_sample_frame(N = 850)
  smp_N_per_level <- declare_sample_frame(N_per_level = c(850, 50, 5))
  smp_lower_units_per_level <- declare_sample_frame(lower_units_per_level = list(rep(1:50, each = 17), rep(1:5, each = 10)))
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_design(potential_outcomes = po)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", method = "lm")
  
  tmp <- make_data(sample_frame = smp_N, potential_outcomes = po)
  
  sims <- compare_experiments(N = c(10000, 500, 5),
                              potential_outcomes = po, 
                              sample_frame =  smp_N, 
                              design = design, 
                              analysis = analysis_1, 
                              sims = 100)
  
  ## pending bug fix this isn't working (issued)
  ##sims <- compare_experiments(N_per_level = list(c(10000, 500, 5), c(50, 5), c(100, 50, 2)),
  ##                            potential_outcomes = po, 
  ##                            sample_frame =  smp_N_per_level, 
  ##                            design = design, 
  ##                            analysis = analysis_1, 
  ##                            sims = 100)
  
  
})
