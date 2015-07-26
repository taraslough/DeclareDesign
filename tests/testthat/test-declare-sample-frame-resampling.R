context("Assignment and probability functions")

rm(list=ls())
library(testthat)
library(registration)

test_that("test assignment and probability functions", {
  
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    lower_units_per_level = list(
      individuals = rep(1,1000), 
      villages = rep(5,200)
    ))
  
  user_data          <- make_data(sample_frame = smp)
  
  sample_frame_N <- declare_sample_frame(data = user_data, N = 1000, resample = TRUE)

  data_N          <- make_data(sample_frame = sample_frame_N)
    
  sample_frame_vars <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable()
    ),
    N_per_level = c(50, 10),
    data = user_data, resample = TRUE)
  
  data_vars          <- make_data(sample_frame = sample_frame_vars)
  
})
