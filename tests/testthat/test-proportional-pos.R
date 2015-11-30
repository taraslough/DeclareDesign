rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 10000)
  sampling <- declare_sampling(n = 500)
  
  condition_names <- c("tr","cn")
  
  potential_outcomes <- declare_potential_outcomes(
    potential_outcomes_function = proportion_potential_outcomes_function, 
    outcome_variable_name = "Y",
    options = list(population_proportions = c(.5,.8)),
    condition_names = condition_names,
    assignment_variable_name = "Z")
  
  pop_proportions <- matrix(c(2,2,1,3)/4,ncol = 2,dimnames = list(
    rows = c("happy","sad"),cols = condition_names
  ))
  
  potential_outcomes2 <- declare_potential_outcomes(
    potential_outcomes_function = proportion_potential_outcomes_function, 
    outcome_variable_name = "Y",
    options = list(population_proportions = pop_proportions),
    assignment_variable_name = "Z")
  
  assignment <- declare_assignment(condition_names = condition_names, 
                                   probability_each = c(.7, .3))
  
  
  # Diagnosis ---------------------------------------------------------------
  
  # This doesn't work:
#   estimand <- declare_estimand(estimand_text = "mean(Y_Z_tr - Y_Z_cn)", potential_outcomes = potential_outcomes)
#   estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
#   
#   design <- declare_design(population = population,
#                            sampling = sampling, 
#                            assignment = assignment, 
#                            estimator = estimator_d_i_m, 
#                            potential_outcomes = potential_outcomes,
#                            label = "Simple Design")
#   
#   diagnosis <- diagnose_design(design = design)
#   
#   design <- declare_design(population = population,
#                            sampling = sampling, 
#                            assignment = assignment, 
#                            estimator = estimator_d_i_m, 
#                            potential_outcomes = potential_outcomes,
#                            label = "Simple Design")
#   
#   diagnosis <- diagnose_design(design = design)
  
  # mock data  ---------------------------------------------------------------  
  
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes)
  pop_draw <- draw_population(population = population, potential_outcomes = potential_outcomes2)
  
  smp_draw <- draw_sample(data = pop_draw, sampling = sampling)
  smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = potential_outcomes, 
                           condition_names = c("tr", "cn"))
  smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = potential_outcomes, condition_names = c("tr", "cn"))
  
  # This doesn't work:
  smp_draw <- draw_outcome(data = smp_draw, 
                           potential_outcomes = potential_outcomes2, 
                           condition_names = c("tr", "cn"))
  head(smp_draw)  
  
  estimates <- get_estimates(estimator = estimator_d_i_m, data = smp_draw)
  estimand <- get_estimands(estimator = estimator_d_i_m, data = pop_draw)
  
  # test draw_data
  
  smp_draw_reveal <- draw_data(design = design)
  
})







