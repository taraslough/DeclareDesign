rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Attrition")

test_that("test whether attrition works", {
  
  population <- declare_population(noise = "rpois(n = n_, lambda = 12)", size = 1000)
  sampling <- declare_sampling(n = 500)
  
  attrition_1 <- declare_attrition(condition_names = c(0,1), 
                                   outcome_variable_name = "R1",
                                   assignment_variable_name = "Z", 
                                   options=list(reporting_proportions = c(.5, .7)))
  
  attrition_2 <- declare_attrition(condition_names = c(0,1), 
                                   outcome_variable_name = "R2",
                                   assignment_variable_name = "Z", 
                                   options=list(proportion_always_reporters = .8))
  
  potential_outcomes_1 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z", 
                                                   attrition = attrition_1)
  
  potential_outcomes_2 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z", 
                                                   attrition = attrition_2)
  
  potential_outcomes_3 <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + noise,
                                                     condition_names = c(0, 1),
                                                     assignment_variable_name = "Z")
  
  assignment <- declare_assignment(condition_names = c(0,1))
  
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  
  smp_draw_1 <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes_1)
  smp_draw_2 <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes_2)
  
  head(smp_draw_1)
  head(smp_draw_2)
  
  with(smp_draw_1, table(Z, R1))
  with(smp_draw_2, table(Z, R2))
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes_3,
                              attrition = attrition_2)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  
  smp_draw_1 <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes_3, attrition = attrition_2)

  head(pop_draw)
  
})
