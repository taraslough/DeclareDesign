
rm(list=ls())
library(testthat)
library(experimentr)

context("Sample frame")

test_that("test the sample frame functionality with user-provided covariates", {
  
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    group_sizes_by_level = list(
      individuals = rep(1,1000), 
      villages = rep(5,200)
    ))
  
  po <- declare_potential_outcomes(condition_names = c("Z0","Z1"),
                                   outcome_formula = Y ~ .01 + 0*Z0 + .2*Z1)
  
  userdata          <- make_data(sample_frame = smp)
  
  smp_userdata <- declare_sample_frame(data = userdata[,c("income","development_level")])
  
  
  smp_userdata_rsmp <- declare_sample_frame(data = userdata[,c("income","development_level")], 
                                            resample = TRUE, N = 250)
  
  # Run analysis on a single realization
  mock2          <- make_data(potential_outcomes = po, sample_frame = smp_userdata)
  
  mock2_rsmp         <- make_data(potential_outcomes = po, sample_frame = smp_userdata_rsmp)
  
})
