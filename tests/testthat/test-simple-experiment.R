
rm(list=ls())
library(testthat)
library(experimentr)

context("Simple experiment")

test_that("test whether a simple experiment can be pre-registered", {
  
  pop <- declare_population(individuals = list(noise = declare_variable()),
                            villages = list(),
                            N_per_level = c(1000, 10), super_population = FALSE)
  
  smp <- declare_sampling(prob = .1, strata_variable_name = "villages_id")
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  design <- declare_assignment(potential_outcomes = po)
  
  analysis_1 <- declare_analysis(formula = Y ~ Z, treatment_variable = "Z")
  
  pop_draw <- draw_population(population = pop)
  
  smp_draw <- draw_sample(population_data = pop_draw, sampling = smp)
  
  data <- reveal_design(data = smp_draw, design = design)

  sims <- diagnose(potential_outcomes = po, 
                   sample =  smp, 
                   design = design, 
                   analysis = analysis_1, 
                   sims = 100)
  summary(sims)
  
  # Run analysis on a single realization
  mock          <- make_data(potential_outcomes = po, sample = smp)
  mock$Z        <- assign_treatment(design, data = mock)
  mock$Y        <- observed_outcome(outcome = "Y", treatment_assignment = "Z", data = mock, sep = "_")
  
  estimates <- get_estimates(analysis_1, data = mock)
  estimates
  
  estimands <- get_estimands(analysis = analysis_1,data = mock)
  estimands
  
  fit <- get_estimates_model(analysis = analysis_1, data = mock)
  summary(fit)
  
  fit <- get_estimands_model(analysis = analysis_1, data = mock)
  summary(fit)
})
