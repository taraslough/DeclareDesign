rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Noncompliance")

test_that("test whether noncompliance works", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 1000)
  sampling <- declare_sampling(n = 500)
  noncompliance <- declare_noncompliance(condition_names = c(0,1), 
                                         assignment_variable_name = "Z", 
                                         compliance_proportions=c(1, .5), 
                                         baseline_condition=0)
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*D,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "D",
                                                   noncompliance = noncompliance)
  
  assignment <- declare_assignment(condition_names = c(0,1))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")
  
  ##diagnosis <- diagnose_design(design = design)
  
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population,
                              potential_outcomes = list(potential_outcomes),
                              noncompliance = noncompliance)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes, noncompliance = noncompliance)
  
  head(pop_draw)
  head(smp_draw)
  with(smp_draw, table(Z, D))
  
})
