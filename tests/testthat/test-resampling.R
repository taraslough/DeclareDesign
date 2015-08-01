
rm(list=ls())
library(testthat)
library(registration)

context("Assignment and probability functions")

test_that("test assignment and probability functions", {
  
  smp <- declare_sample_frame(
    individuals = list(
      income = declare_variable()),
    villages = list(
      development_level = declare_variable(multinomial_probabilities = 1:5/sum(1:5))
    ),
    N_per_level = c(1000, 100))
  
  user_data <- make_data(sample_frame = smp)
  
  sample_frame_N <- declare_sample_frame(data = user_data, N = 1000, resample = TRUE)

  data_N          <- make_data(sample_frame = sample_frame_N)
    
  sample_frame_vars <- declare_sample_frame(level_ID_variables = c("individuals_id", "villages_id"),
    N_per_level = c(50, 10),
    data = user_data, resample = TRUE)
  
  data_vars          <- make_data(sample_frame = sample_frame_vars)
  
})
