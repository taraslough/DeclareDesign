
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Multiple outcomes")

test_that("test whether various functions can accept multiple outcomes", {
  
  smp <- declare_sample(height = declare_variable(8, 3), N = 850)
  
  po_1 <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                     formula = Y1 ~ .01 + 0*Z0 + .2*Z1 + .4*height)
  
  # Note that PO2 depends on PO1 !!!!
  po_2 <- declare_potential_outcomes(condition_names = c("Z0","Z1", "Z2"),
                                     formula = Y2 ~ .01 + 0*Z0 + .2*Z1  + .4*Z2- .4*Y1_Z0)
  
  
  assignment <- declare_assignment(potential_outcomes = list(po_1, po_2), excluded_conditions = "Z2")
  #assignment <- declare_assignment(potential_outcomes = list(po_1, po_2))
  
  analysis_1 <- declare_analysis(formula = Y1 ~ Z, treatment_variable = "Z", 
                                 method = "lm")
  
  # Run analysis on a single realization
  mock <- make_data(potential_outcomes = list(po_1, po_2), sample =  smp)
  
  mock$Z <- assign_treatment_indicator(assignment, data = mock)
  
  mock$Y1 <- reveal_outcome(outcome = "Y1", treatment_assignment = "Z", 
                              data = mock, sep = "_")
  
  mock$Y2 <- reveal_outcome(outcome = "Y2", treatment_assignment = "Z", 
                              data = mock, sep = "_")
  
  estimates <- get_estimates(analysis = list(analysis_1) ,data = mock)
  estimates
  
  fit_1 <- fit_model(analysis = analysis_1, data = mock)
  summary(fit_1)
  
})
