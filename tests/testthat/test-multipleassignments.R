rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 5000)
  sampling <- declare_sampling(n = 4999)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + 1*Z1 + 2*Z2 - 3*Z1*Z2 + noise,
                                                   condition_names = list(Z1 = c(0, 1), 
                                                                          Z2 = c(0, 1)),
                                                   assignment_variable_name = c("Z1", "Z2"))
  assignment_1 <- declare_assignment(condition_names = c(0, 1),assignment_variable_name  = "Z1")
  assignment_2 <- declare_assignment(condition_names = c(0, 1),assignment_variable_name  = "Z2")
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  
  smp_draw <- assign_treatment(data = smp_draw, assignment = list(assignment_1, assignment_2))
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes)
  
  head(smp_draw)
  
  summary(lm(Y~ Z1*Z2, data= smp_draw))
  

})
