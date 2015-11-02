
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test compare experiment", {
  
  smp_N <- declare_sample(N = 850)
  smp_N_per_level <- declare_sample(N_per_level = c(850, 50, 5))
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  assignment <- declare_assignment(potential_outcomes = po)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z", estimator = lm, quantity_of_interest = average_treatment_effect)
  
  tmp <- make_data(sample = smp_N, potential_outcomes = po)
  
  sims <- compare_experiments(N = c(10000, 500, 5),
                              potential_outcomes = po, 
                              sample =  smp_N, 
                              assignment = assignment, 
                              analysis = analysis_1, 
                              sims = 5)
  
  ## pending bug fix this isn't working (issued)
  ##sims <- compare_experiments(N_per_level = list(c(10000, 500, 5), c(50, 5), c(100, 50, 2)),
  ##                            potential_outcomes = po, 
  ##                            sample =  smp_N_per_level, 
  ##                            assignment = assignment, 
  ##                            analysis = analysis_1, 
  ##                            sims = 100)
  
  
})
