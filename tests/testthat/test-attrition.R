rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Attrition")

test_that("test whether attrition works", {
  
  population <- declare_population(noise = declare_variable(), N = 1000)
  sampling <- declare_sampling(n = 500)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                   condition_names = c(0, 1),
                                                   treatment_variable_name = "Z")
  attrition_1 <- declare_attrition(condition_names = c(0,1), 
                                   outcome_name = "R1",
                                   treatment_variable_name = "Z", 
                                   reporting_proportions = c(.5, .7))
  
  attrition_2 <- declare_attrition(condition_names = c(0,1), 
                                   outcome_name = "R2",
                                   treatment_variable_name = "Z", 
                                   proportion_always_reporters = .8)
  
  assignment <- declare_assignment(condition_names = c(0,1))
  
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = list(potential_outcomes, attrition_1, attrition_2))
  head(smp_draw)
  with(smp_draw, table(Z, R1))
  with(smp_draw, table(Z, R2))
  
  
})
