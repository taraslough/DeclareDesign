rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Blocked and clustered experiment")

test_that("test blocked and clustered experiment", {
  
  population <- declare_population(individuals = list(noise = declare_variable()),
                                   villages = list(elevation = declare_variable(),
                                                   high_elevation = "1*(elevation > 0)"), 
                                   size = c(1000, 100))
  
  sampling <- declare_sampling(n = 24, cluster_variable_name = "villages_ID")
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z + .2*Z*elevation + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(condition_names = c(0,1),
                                   block_variable_name = "elevation_high", 
                                   custom_blocking_function = function(data) return(1*(data$elevation > 0)),
                                   cluster_variable_name = "villages_ID")
  
  
  # Diagnosis ---------------------------------------------------------------
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means,
                                       formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_clustered <- declare_estimator(estimates = difference_in_means,
                                                 cluster_variable_name = "villages_ID",
                                                 formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_blocked <- declare_estimator(estimates = difference_in_means_blocked,
                                               block_variable_name = "elevation_high",
                                               formula = Y ~ Z, estimand = estimand)
  estimator_d_i_m_blocked_clustered <- declare_estimator(estimates = difference_in_means_blocked,
                                                         block_variable_name = "elevation_high",
                                                         cluster_variable_name = "villages_ID",
                                                         formula = Y ~ Z, estimand = estimand)
  estimator_lm <- declare_estimator(model = lm, estimates = get_regression_coefficient, 
                                    coefficient_name = "Z",
                                    formula = Y ~ Z, estimand = estimand)
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = list(estimator_d_i_m, estimator_d_i_m_clustered, estimator_d_i_m_blocked, estimator_d_i_m_blocked_clustered, estimator_lm), 
                           potential_outcomes = potential_outcomes,
                           label = "Blocked and Clustered Design")
  
  summary(design)
  
  diagnosis <- diagnose_design(design = design, population_draws = 2, sample_draws = 4)
  
  # mock data  ---------------------------------------------------------------  
  
  data <- draw_data(design = design)
  estimates <- get_estimates(estimator = list(estimator_d_i_m, estimator_d_i_m_clustered, estimator_d_i_m_blocked, estimator_d_i_m_blocked_clustered, estimator_lm), data = data)
  
  estimates1 <- get_estimates(estimator = estimator_lm, data = data)
  estimates2 <- get_estimates(estimator = estimator_d_i_m, data = data)
  
  estimands <- get_estimands(estimator = estimator_d_i_m, data = data)
  
})
