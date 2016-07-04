rm(list=ls())
library(testthat)
library(DeclareDesign)

context("Custom Diagnosis")

test_that("test simple experiment analysis and diagnosis", {
  
  population <- declare_population(noise = "rnorm(n_)", size = 250)
  sampling <- declare_sampling(n = 100)
  
  potential_outcomes <- declare_potential_outcomes(formula = Y ~ 5 + .5*Z*rnorm(n_) + noise,
                                                   condition_names = c(0, 1),
                                                   assignment_variable_name = "Z")
  
  assignment <- declare_assignment(potential_outcomes=potential_outcomes, probability_each = c(.7, .3))
  
  estimand <- declare_estimand(estimand_text = "mean(Y_Z_1 - Y_Z_0)", potential_outcomes = potential_outcomes)
  estimator_d_i_m <- declare_estimator(estimates = difference_in_means, formula = Y ~ Z, estimand = estimand)
  
  # Diagnosis ---------------------------------------------------------------
  
  design <- declare_design(population = population,
                           sampling = sampling, 
                           assignment = assignment, 
                           estimator = estimator_d_i_m, 
                           potential_outcomes = potential_outcomes,
                           label = "Simple Design")

  sum_pvals <- function(estimates){
    Nrows <- length(estimates[[1]][[1]])
    rowNames <- rownames(estimates[[1]][[1]])
    estmat <- matrix(data = unlist(estimates,recursive = T),
                     nrow = Nrows,dimnames = list(rowNames))
    p_sum <- sum(estmat["p", ], na.rm = T)
    return(p_sum)
  }
  
  # This doesn't work at the moment
  expect_error({
    diagnosis <- diagnose_design(
      design = design,
      statistics = sum_pvals)
  })
  
})

