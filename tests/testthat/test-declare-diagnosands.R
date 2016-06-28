rm(list = ls())
library(testthat)
library(DeclareDesign)

context("Simple experiment")

#test_that("test declare diagnosands", {
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <-
    declare_potential_outcomes(
      formula = Y ~ 5 + .5 * Z * rnorm(n_) + noise,
      condition_names = c(0, 1),
      assignment_variable_name = "Z"
    )
  
  assignment <-
    declare_assignment(potential_outcomes = potential_outcomes,
                       probability_each = c(.7, .3))
  
  
  estimand_1 <-
    declare_estimand(
      estimand_text = "mean(Y_Z_1 - Y_Z_0)",
      potential_outcomes = potential_outcomes,
      estimand_level = "population"
    )
  estimand_2 <-
    declare_estimand(
      estimand_text = "mean(Y_Z_1 - Y_Z_0)",
      potential_outcomes = potential_outcomes,
      estimand_level = "sample"
    )
  estimand_3 <-
    declare_estimand(
      estimand_text = "mean(Y_Z_1 - Y_Z_0)",
      potential_outcomes = potential_outcomes,
      estimand_level = "assignment"
    )
  
  estimator_0 <-
    declare_estimator(
      estimates = difference_in_means,
      formula = Y ~ Z,
      label = "diff_1"
    )
  
  estimator_1 <-
    declare_estimator(
      estimates = difference_in_means,
      formula = Y ~ Z,
      label = "diff_1",
      estimand = list(estimand_1, estimand_2, estimand_3)
    )
  estimator_2 <-
    declare_estimator(
      estimates = difference_in_means,
      formula = Y ~ Z,
      label = "diff_2",
      estimand = estimand_2
    )
  
  bias_diagnosand <-
    declare_diagnosand(diagnostic_statistic_text = "est - estimand", summary_function = mean)
  

  
  # Case 1  
  design <- declare_design(
    population = population,
    sampling = sampling,
    assignment = assignment,
    estimator = estimator_1,
    potential_outcomes = potential_outcomes
  )
  
  simulations_df <- diagnose_design(design = design, population_draws = 3, sample_draws = 3, assignment_draws = 3)
  
  #debugonce(get_diagnosand)
  get_diagnosand(diagnosand = bias_diagnosand, simulations = simulations_df$simulations)
  
#})
